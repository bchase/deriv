import gleam/pair
import gleam/option.{type Option, Some, None}
import gleam/io
import gleam/dict
import gleam/result
import gleam/list
import gleam/string
import gleam/regexp.{Match}
import glance.{type CustomType, type TypeAlias}
import deriv/types.{type Derivation, Derivation, DerivField, type DerivFieldOpt, DerivFieldOpt, type DerivFieldOpts}

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

pub fn parse_type_with_derivations(type_: CustomType, src: String) -> Result(#(CustomType, List(Derivation), DerivFieldOpts), Nil) {
  let assert Ok(type_line_re) = regexp.compile("^(pub )?type\\s+?" <> type_.name <> "([(]|\\s|[{])", regexp.Options(case_insensitive: False, multi_line: True))

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

      case parse_derivations_from_inside_type_def_lines(lines) {
        [] -> Error(Nil)
        derivs -> {
          let deriv_field_opts = parse_all_deriv_field_opts(lines)

          Ok(#(type_, derivs, deriv_field_opts))
        }
      }
    }
  }
}

fn type_alias_src_from(
  lines: List(String),
) -> Result(#(TypeAlias, String), Nil) {
  lines
  |> list.fold_until(#(Error(Nil), ""), fn(acc, line) {
    let #(_result, src) = acc
    let src = string.join([src, line], "\n")

    {
      use module <- result.try(glance.module(src) |> result.replace_error(Nil))
      use type_alias <- result.try(module.type_aliases |> list.first())

      Ok(list.Stop(#(Ok(#(type_alias.definition, src)), src)))
    }
    |> result.unwrap(list.Continue(#(Error(Nil), src)))
  })
  |> pair.first
}

fn type_alias_and_derivs_from(
  lines: List(String),
) -> Result(#(TypeAlias, List(Derivation), DerivFieldOpts), Nil) {
  use #(type_alias, src) <- result.try(type_alias_src_from(lines))

  let derivs = parse_derivations_from_inside_type_def_lines(string.split(src, "\n"))

  Ok(#(type_alias, derivs, dict.new()))
}

pub fn parse_type_aliases_with_derivations(type_: TypeAlias, src: String) -> Result(#(TypeAlias, List(Derivation), DerivFieldOpts), Nil) {
  let assert Ok(type_line_re) = regexp.compile("^(pub )?type\\s+?" <> type_.name <> "([(]|\\s|[=])", regexp.Options(case_insensitive: False, multi_line: True))

  case regexp.check(type_line_re, src) {
    False -> Error(Nil)
    True -> {
      let lines_from_type_alias_start_to_eof =
        src
        |> string.split("\n")
        |> list.drop_while(fn(line) {
          !regexp.check(type_line_re, line)
        })

      type_alias_and_derivs_from(lines_from_type_alias_start_to_eof)
    }
  }
}

// fn parse_derivations_for_type_alias(lines: List(String)) -> List(Derivation) {
//   lines
//   |> list.map(fn(line) {
//     case string.split(line, "//$") {
//       [_, mc] ->
//         parse_derivations(mc)
//         |> result.unwrap([])

//       _ ->
//         []
//     }
//   })
//   |> list.flatten
//   |> list.reverse
// }

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
  |> list.reverse
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
  raw
  |> string.trim
  |> string.split(" ")
  |> fn(tokens) {
    case tokens {
      ["derive", name, ..opts] ->
        Ok([Derivation(name:, opts:)])

      _ ->
        Error(Nil)
    }
  }
}

type DerivFieldOptsAcc {
  DerivFieldOptsAcc(
    type_: Result(String, Nil),
    variant: Result(String, Nil),
    field: Result(String, Nil),
    opts: DerivFieldOpts,
  )
}

fn parse_all_deriv_field_opts(lines: List(String)) -> DerivFieldOpts {
  let assert Ok(type_re) =
    "^\\s*(pub\\s+)?type\\s+([A-Z]\\w*)\\s*[{]"
    |> regexp.from_string

  let assert Ok(variant_re) =
    "^\\s*([A-Z]\\w*)\\s*[(]"
    |> regexp.from_string

  let assert Ok(field_re) =
    "^\\s*([a-z]\\w*)\\s*[:]"
    |> regexp.from_string

  lines
  |> list.fold(DerivFieldOptsAcc(Error(Nil), Error(Nil), Error(Nil), dict.new()), fn(acc, line) {
    let type_ =
      case regexp.scan(type_re, line) {
        [Match(_txt, [_, Some(type_)])] -> Ok(type_)
        _ -> Error(Nil)
      }

    let variant =
      case regexp.scan(variant_re, line) {
        [Match(_txt, [Some(variant)])] -> Ok(variant)
        _ -> Error(Nil)
      }

    let field =
      case regexp.scan(field_re, line) {
        [Match(_txt, [Some(field)])] -> Ok(field)
        _ -> Error(Nil)
      }

    let acc =
      case type_ {
        Ok(_) -> DerivFieldOptsAcc(..acc, type_:)
        _ -> acc
      }

    let acc =
      case variant {
        Ok(_) -> DerivFieldOptsAcc(..acc, variant:)
        _ -> acc
      }

    let acc =
      case field {
        Ok(_) -> DerivFieldOptsAcc(..acc, field:)
        _ -> acc
      }

    case acc.type_, acc.variant, acc.field {
      Ok(type_), Ok(variant), Ok(field) ->
        case parse_deriv_field_opts(line) {
          [] -> acc
          new_opts -> {
            let key = DerivField(type_:, variant:, field:)

            let opts =
              acc.opts
              |> dict.upsert(key, fn(opts) {
                opts
                |> option.unwrap([])
                |> list.append(new_opts)
              })

            DerivFieldOptsAcc(..acc, opts:)
          }
        }

      _, _, _ -> acc
    }
  })
  |> fn(acc) {
    acc.opts
  }
}

fn parse_deriv_field_opts(str: String) -> List(DerivFieldOpt) {
  let assert Ok(magic_comment_re) = regexp.from_string("\\s*[/][/][$]\\s*")
  let assert Ok(whitespace_re) = regexp.from_string("\\s+")

  case regexp.split(magic_comment_re, str) {
    [_, magic_comment] ->

      case regexp.split(whitespace_re, magic_comment) {
        [] -> []
        strs -> [DerivFieldOpt(strs)]
      }

    _ ->
      []
  }
}
