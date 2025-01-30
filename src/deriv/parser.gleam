import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/string
import gleam/set
import gleam/regexp.{Match}
import nibble.{do, return}
import nibble/lexer
import glance.{type CustomType}
import deriv/types.{type Derivation, Derivation, type DerivFieldOpt, DerivFieldOpt}

pub fn parse_type_with_derivations(type_: CustomType, src: String) -> Result(#(CustomType, List(Derivation), Dict(String, List(DerivFieldOpt))), Nil) {
  let assert Ok(type_line_re) = regexp.compile("^(pub )?type\\s+?" <> type_.name <> "\\s*?[{]", regexp.Options(case_insensitive: False, multi_line: True))

  case regexp.check(type_line_re, src) {
    False -> Error(Nil)
    True -> {
      let lines_from_type_start_to_eof =
        src
        |> string.split("\n")
        |> list.drop_while(fn(line) {
          !regexp.check(type_line_re, line)
        })

      let lines_from_type_start_except_last =
        lines_from_type_start_to_eof
        |> list.take_while(fn(line) {
          !string.starts_with(line, "}")
        })

      let assert Ok(line_last_for_type) =
        lines_from_type_start_to_eof
        |> list.drop(list.length(lines_from_type_start_except_last))
        |> list.take(1)
        |> list.first

      let lines =
        [
          lines_from_type_start_except_last,
          [line_last_for_type],
        ]
        |> list.flatten

      let derivs =
        parse_derivations_from_inside_type_def_lines(lines)

      let deriv_field_opts =
        parse_all_deriv_field_opts(lines)

      case derivs {
        [] -> Error(Nil)
        ds -> Ok(#(type_, ds, deriv_field_opts))
      }
    }
  }
}

fn parse_derivations_from_inside_type_def_lines(lines: List(String)) -> List(Derivation) {
  lines
  |> list.map(fn(line) {
    case string.split(line, "//$") {
      [_, mc] ->
        parse_derivations(mc)
        |> result.unwrap([])

      _ ->
        []
    }
  })
  |> list.flatten
}

pub fn parse_import_with_derivations(import_: glance.Import, src: String) -> Result(#(glance.Import, List(Derivation)), Nil) {
  let magic_comment =
    src
    |> string.split("\n")
    |> list.find(fn(line) {
      string.starts_with(line, "import ") &&
        string.contains(line, import_.module)
    })
    |> result.map(string.split(_, "//$"))

  case magic_comment {
    Ok([_, mc]) ->
      parse_derivations(mc)
      |> result.map(fn(ds) {
        #(import_, ds)
      })

    _ -> Error(Nil)
  }
}

fn parse_derivations(raw: String) -> Result(List(Derivation), Nil) {
  let assert Ok(re) = regexp.from_string("((\\w+)[(]([^)]+)[)]\\s*)+")

  let raw = string.trim(raw)

  case string.starts_with(raw, "derive") {
    False -> Error(Nil)
    True -> {
      let str =
        raw
        |> string.drop_start(6)
        |> string.trim

      string.split(str, " ")
      |> list.map(fn(d) {
        case regexp.scan(re, d) {
          [Match(submatches: [_, Some(deriv), Some(opts_str)], ..)] -> {
            let opts = string.split(opts_str, ",")

            Ok(Derivation(name: deriv, opts:))
          }

          _ ->
            panic as { "Parse failure on derivation: " <> d }
        }
      })
      |> result.all
    }
  }
}

type DerivFieldOptsAcc {
  DerivFieldOptsAcc(
    constr: Result(String, Nil),
    field: Result(String, Nil),
    opts: Dict(#(String, String), List(DerivFieldOpt)),
  )
}

fn parse_all_deriv_field_opts(lines: List(String)) -> Dict(String, List(DerivFieldOpt)) {
  let assert Ok(constr_re) =
    "^\\s*([A-Z]\\w*)\\s*[(]"
    |> regexp.from_string

  let assert Ok(field_re) =
    "^\\s*([a-z]\\w*)\\s*[:]"
    |> regexp.from_string

  lines
  |> list.fold(DerivFieldOptsAcc(Error(Nil), Error(Nil), dict.new()), fn(acc, line) {
    let constr =
      case regexp.scan(constr_re, line) {
        [Match(_txt, [Some(constr)])] -> Ok(constr)
        _ -> Error(Nil)
      }

    let field =
      case regexp.scan(field_re, line) {
        [Match(_txt, [Some(field)])] -> Ok(field)
        _ -> Error(Nil)
      }

    let acc =
      case constr {
        Ok(_) -> DerivFieldOptsAcc(..acc, constr:)
        _ -> acc
      }

    let acc =
      case field {
        Ok(_) -> DerivFieldOptsAcc(..acc, field:)
        _ -> acc
      }

    case acc.constr, acc.field {
      Ok(constr), Ok(field) ->
        case parse_deriv_field_opts(line) {
          [] -> acc
          new_opts -> {
            let opts =
              acc.opts
              |> dict.upsert(#(constr, field), fn(opts) {
                opts
                |> option.unwrap([])
                |> list.append(new_opts)
              })

            DerivFieldOptsAcc(..acc, opts:)
          }

        }

      _, _ -> acc
    }
  })
  |> fn(acc) {
    acc.opts
    |> dict.to_list
    |> list.map(fn(x) {
      let #(#(_constr, field), opts) = x
      #(field, opts)
    })
    |> dict.from_list
  }
}

type Token {
  LParen
  RParen
  Str(String)
}

fn parse_deriv_field_opts(str: String) -> List(DerivFieldOpt) {
  let assert Ok(re) = regexp.from_string("\\s*[/][/][$]\\s*")

  case regexp.split(re, str) {
    [_, magic_comment] ->
      parse_deriv_field_opts_(magic_comment)

    _ ->
      []
  }
}

fn parse_deriv_field_opts_(str: String) -> List(DerivFieldOpt) {
  let lexer =
    lexer.simple([
      lexer.token("(", LParen),
      lexer.token(")", RParen),
      lexer.identifier("[^()]", "[^()]", set.from_list([ "(", ")" ]), Str),
    ])

  let str_parser = {
    use tok <- nibble.take_map("expected key")
    case tok {
      Str(str) -> Some(str)
      _ -> None
    }
  }

  let deriv_with_opt_parser = {
    use deriv <- do(str_parser)
    use _ <- do(nibble.token(LParen))
    use opt <- do(str_parser)
    use _ <- do(nibble.token(LParen))
    use key <- do(str_parser)
    use _ <- do(nibble.token(LParen))
    use val <- do(str_parser)
    use _ <- do(nibble.token(RParen))
    use _ <- do(nibble.token(RParen))
    use _ <- do(nibble.token(RParen))

    return(DerivFieldOpt(deriv:, opt: Some(opt), key:, val:))
  }

  let deriv_without_opt_parser = {
    use deriv <- do(str_parser)
    use _ <- do(nibble.token(LParen))
    use key <- do(str_parser)
    use _ <- do(nibble.token(LParen))
    use val <- do(str_parser)
    use _ <- do(nibble.token(RParen))
    use _ <- do(nibble.token(RParen))

    return(DerivFieldOpt(deriv:, opt: None, key:, val:))
  }

  // let parser =
  //   nibble.one_of([
  //     deriv_with_opt_parser,
  //     deriv_without_opt_parser,
  //   ])

  case lexer.run(str, lexer) {
    Error(_) -> Error(Nil)
    Ok(tokens) -> {
      // nibble.run(tokens, parser)
      [
        deriv_with_opt_parser,
        deriv_without_opt_parser,
      ]
      |> list.find_map(fn(parser) {
        nibble.run(tokens, parser)
        |> result.map(fn(x) { [x] })
      })
      |> result.map_error(fn(_) { Nil })
    }
  }
  |> result.unwrap([])
}
