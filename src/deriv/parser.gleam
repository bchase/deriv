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
  let lines_from_type_start_to_eof =
    src
    |> string.split("\n")
    |> list.drop_while(fn(line) {
      !string.contains(line, "type " <> type_.name)
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

  let assert Ok(re) = regexp.from_string(
    "^\\s*([_a-z0-9]+)\\s*[:]\\s*(.+)[,]?\\s*[/][/][$]\\s*(.+)$"
  )

  let deriv_field_opts =
    lines_from_type_start_except_last
    |> list.map(fn(line) {
      case regexp.scan(re, line) {
        [Match(_txt, [Some(field_name), Some(_type), Some(magic_comment)])] -> {
          case parse_deriv_field_opt(magic_comment) {
            Error(_) -> Error(Nil)
            Ok(opt) ->
              Ok(#(field_name, [opt]))
          }
        }

        _ ->
          Error(Nil)
      }
    })
    |> result.values
    |> dict.from_list
    // |> io.debug
    // // dict.from_list([#("name", [DerivFieldOpt("json", Some("encode"), "foo", "bar")])])

  // // TODO field magic comments
  // let lines_for_type_def =
  //   list.append(
  //     lines_from_type_start_except_last,
  //     [line_last_for_type],
  //   )
  // io.println(lines_for_type_def |> string.join("\n"))

  case string.split(line_last_for_type, "//$") {
    [_, mc] ->
      parse_derivations(mc)
      |> result.map(fn(ds) {
        #(type_, ds, deriv_field_opts)
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

type Token {
  LParen
  RParen
  Str(String)
}

fn parse_deriv_field_opt(str: String) -> Result(DerivFieldOpt, Nil) {
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
      })
      |> result.map_error(fn(_) { Nil })
    }
  }
}
