import gleam/option.{Some, type Option, None}
import glance_printer
import gleam/pair
import gleam/result
import gleam/bool
import glance as g
import gleam/regexp as re
import gleam/list
import gleam/string
import deriv/internal/glance.{z} as dg

const gen_magic_comment_start = "//$ gen"

pub fn process(
  module module: g.Module,
  src src: String,
) -> String {
  use <- bool.guard(!{ src |> string.contains(gen_magic_comment_start) }, src)

  let lines = src |> string.split("\n")

  module.functions
  |> list.map(build_func_gens(func: _, lines:))
  |> list.fold(src, run)
}

//

fn run(
  src src: String,
  func_gens func_gens: FuncGens,
) -> String {
  let FuncGens(func:, gens:) = func_gens

  gens
  |> list.fold(#(src, 0), fn(acc, gen) {
    let #(old, offset) = acc

    // offset positions based on previous code gen results
    let gen = Gen(..gen, pos: gen.pos + offset)

    // build & format `glance.Expression` as `String`
    let expr = test1(gen:)
    let expr_src = gleam_format_expr(expr:, indent: gen.indent)

    // add magic comment back to gen'd `glance.Expression` src
    let expr_src =
      case expr_src |> string.split("\n") {
        [] ->
          expr_src // impossible

        [_] ->
          panic as { "`gen " <> gen.str <> "` must generate a multiline block, but failed to" }

        [opening_bracket, ..rest] ->
          [opening_bracket <> " " <> gen.comment, ..rest] |> string.join("\n")
      }

    // calc span to overwrite
    let span = gen_span(gen:, src:)

    // construct new src
    let #(start, end) = dg.splice_out_span(old, span)
    let new = string.join([
      start,
      expr_src |> string.trim_start,
      end,
    ], "")

    // calc diff for new `offset`
    let diff = string.length(new) - string.length(old)

    #(new, offset + diff)
  })
  |> pair.first
}

pub fn test1(
  gen gen: Gen,
) -> g.Expression {
  g.Block(z, [g.Expression(
    g.Case(z, [g.Variable(z, "foo")], [
      g.Clause(patterns: [[g.PatternDiscard(z, "")]], guard: None, body: g.Variable(z, "foo")),
    ]),
  )])
}

pub fn test2(
  gen gen: Gen,
) -> g.Expression {
  g.Block(z, [g.Expression(
    g.Case(z, [g.Variable(z, "bar")], [
      g.Clause(patterns: [[g.PatternDiscard(z, "")]], guard: None, body: g.Variable(z, "bar")),
    ]),
  )])
}

type Pair {
  Pair(
    open: String,
    close: String,
  )
}

const curly_brackets = Pair("{", "}")

type ClosingPairAcc {
  ClosingPairAcc(
    pos: Int,
    open: Int,
  )
}

fn closing_position(
  of pair: Pair,
  in str: String,
  after pos: Int,
) -> Result(Int, Nil) {
  let lines =
    str
    |> string.drop_start(pos + 1)
    |> string.split("\n")
    |> list.map(string.append(_, "\n"))

  list.fold_until(lines, ClosingPairAcc(pos: 0, open: 1), fn(acc, line) {
    use <- bool.guard(acc.open <= 0, list.Stop(acc))

    list.fold_until(string.to_graphemes(line), acc, fn(acc, ch) {
      use <- bool.guard(acc.open <= 0, list.Stop(acc))

      case ch == pair.open, ch == pair.close {
        True, _ -> ClosingPairAcc(..acc, open: acc.open + 1)
        _, True -> ClosingPairAcc(..acc, open: acc.open - 1)
        _, _ -> acc
      }
      |> fn(acc) { list.Continue(ClosingPairAcc(..acc, pos: acc.pos + 1)) }
    })
    |> list.Continue
  })
  |> fn(result) {
    case result {
      ClosingPairAcc(pos:, open:) if open <= 0 -> Ok(pos)
      _ -> Error(Nil)
    }
  }
}

type FuncGens {
  FuncGens(
    func: g.Definition(g.Function),
    gens: List(Gen),
  )
}

pub type Gen {
  Gen(
    str: String,
    comment: String,
    indent: Int,
    pos: Int,
    new: Bool,
  )
}

fn build_func_gens(
  func func: g.Definition(g.Function),
  lines lines: List(String),
) -> FuncGens {
  FuncGens(func:, gens: build_gens(lines:, func:))
}

fn build_gens(
  func func: g.Definition(g.Function),
  lines lines: List(String),
) -> List(Gen) {
  let assert Ok(start_re) =
    "^((\\s*)([{]\\s*)?)([/][/][$]\\s*?gen\\s+(.+)$)"
    |> re.from_string

  list.fold(lines, #(None, [], func.definition.location.start), fn(acc, line) {
    let #(gen, gens, pos) = acc
    let next_pos = pos + string.length(line)

    {
      use #(gen_str, comment, has_block, indent, until_comment) <- result.try(
        case re.scan(start_re, line) {
          [re.Match(_, [pre_comment, indent, bracket, Some(comment), Some(gen_str)])] ->
            Ok(#(
              gen_str,
              comment,
              bracket |> option.is_some,
              indent |> option.unwrap("") |> string.length,
              pre_comment |> option.unwrap("") |>  string.length,
            ))

          _ ->
            Error(Nil)
        }
      )

      let gen = Gen(
        str: gen_str,
        comment:,
        indent:,
        pos: pos + until_comment,
        new: !has_block,
      )

      Ok(#(None, [gen, ..gens], next_pos))
    }
    |> result.unwrap(#(gen, gens, next_pos))
  })
  |> fn(t) {
    let #(gen, gens, _pos) = t

    case gen {
      Some(gen) -> [gen, ..gens]
      None -> gens
    }
  }
}

fn gen_span(
  gen gen: Gen,
  src src: String,
) -> g.Span {
  case bracket_pos(gen) {
    None ->
      g.Span(start: gen.pos, end: gen.pos + { gen.comment |> string.length })

    Some(start) -> {
      src
      |> closing_position(in: _, of: curly_brackets, after: start)
      |> fn(result) {
        case result {
          Error(Nil) -> panic as {
            "couldn't find the closing bracket of existing code gen block"
          }

          Ok(end) ->
            g.Span(start:, end: end + start)
        }
      }
    }
  }
}

fn bracket_pos(
  gen gen: Gen,
) -> Option(Int) {
  use <- bool.guard(gen.new, None)
  Some(gen.pos + gen.indent + 1)
}

fn gleam_format_expr(
  expr expr: g.Expression,
  indent indent: Int,
) -> String {
  g.Module([], [], [], [], [g.Definition([], g.Function(
    z, "main", g.Public, [], return: None, body: [g.Expression(
      expr
    )]
  ))])
  |> glance_printer.print
  |> string.split("\n")
  |> list.drop(1)
  |> fn(lines) { list.take(lines, list.length(lines) - 2) }
  |> fn(lines) {
    case indent {
      2 -> lines
      0 -> lines |> list.map(string.drop_start(_, 2))
      1 -> lines |> list.map(string.drop_start(_, 1))
      _ -> {
        let ws = list.repeat(" ", indent - 2) |> string.join("")
        lines |> list.map(string.append(to: ws, suffix: _))
      }
    }
  }
  |> string.join("\n")
}
