import bchase/io
import gleam/set
import gleam/pair
import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/string
import gleam/regexp.{Match} as re
import glance.{type CustomType, type TypeAlias}
import deriv/internal/types.{type Derivation, Derivation, DerivField, type DerivFieldOpt, DerivFieldOpt, type DerivFieldOpts}
import deriv/gen/types.{type AST, type GleamPath} as _
import deriv/internal/glance.{z} as dg
import nibble
import nibble/lexer

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

pub fn parse_type_with_derivations(type_: CustomType, src: String) -> Result(#(CustomType, List(Derivation), DerivFieldOpts), Nil) {
  let assert Ok(type_line_re) =
    // { "^(pub )?type\\s+?" <> type_.name <> "([(]|\\s|[{])" }
    { "^(pub\\s+(opaque\\s+)?)?type\\s+?" <> type_.name <> "([(]|\\s|[{])" }
    |> re.compile(re.Options(case_insensitive: False, multi_line: True))

  case re.check(type_line_re, src) {
    False -> Error(Nil)
    True -> {
      let lines_from_type_start_to_eof =
        src
        |> string.split("\n")
        |> list.drop_while(fn(line) {
          !re.check(type_line_re, line)
        })

      case lines_from_type_start_to_eof {
        [] -> // NOTE: this case runs for types without variants, e.g. `type Foo\n`
          Error(Nil)

        _ -> {
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
            #(derivs, opts) -> {
              let type_opts =
                opts
                |> list.map(fn(opt) { #(DerivField(type_: type_.name, variant: "", field: ""), [opt]) })
                |> dict.from_list

              let deriv_field_opts =
                lines
                |> parse_all_deriv_field_opts
                |> dict.merge(type_opts)

              Ok(#(type_, derivs, deriv_field_opts))
            }
          }
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

  let #(derivs, _opts) = parse_derivations_from_inside_type_def_lines(string.split(src, "\n"))

  Ok(#(type_alias, derivs, dict.new()))
}

pub fn parse_type_aliases_with_derivations(type_: TypeAlias, src: String) -> Result(#(TypeAlias, List(Derivation), DerivFieldOpts), Nil) {
  let assert Ok(type_line_re) = re.compile("^(pub )?type\\s+?" <> type_.name <> "([(]|\\s|[=])", re.Options(case_insensitive: False, multi_line: True))

  case re.check(type_line_re, src) {
    False -> Error(Nil)
    True -> {
      let lines_from_type_alias_start_to_eof =
        src
        |> string.split("\n")
        |> list.drop_while(fn(line) {
          !re.check(type_line_re, line)
        })

      type_alias_and_derivs_from(lines_from_type_alias_start_to_eof)
    }
  }
}

fn parse_derivations_from_inside_type_def_lines(
  lines: List(String),
) -> #(List(Derivation), List(DerivFieldOpt)) {
  let derivs =
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

  let opts =
    lines
    |> list.map(fn(line) {
      case string.split(line, "//$") {
        [_, mc] ->
          parse_type_opts(mc)
          |> result.unwrap([])

        _ ->
          []
      }
    })
    |> list.flatten
    |> list.reverse

  #(derivs, opts)
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

fn parse_derivations(
  raw: String,
) -> Result(List(Derivation), Nil) {
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

fn parse_type_opts(
  raw: String,
) -> Result(List(DerivFieldOpt), Nil) {
  raw
  |> string.trim
  |> string.split(" ")
  |> fn(tokens) {
    case tokens {
      ["derive", ..] ->
        Error(Nil)

      [_, ..] as strs ->
        Ok([DerivFieldOpt(raw:, strs:)])

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
    "^\\s*(pub\\s+)?type\\s+([A-Z]\\w*)(\\s*|[(])"
    |> re.from_string

  let assert Ok(variant_re) =
    "^\\s*([A-Z]\\w*)\\s*[(]?"
    |> re.from_string

  let assert Ok(field_re) =
    "^\\s*([a-z]\\w*)\\s*[:]"
    |> re.from_string

  lines
  |> list.fold(DerivFieldOptsAcc(Error(Nil), Error(Nil), Error(Nil), dict.new()), fn(acc, line) {
    let type_ =
      case re.scan(type_re, line) {
        [Match(_txt, [_, Some(type_), ..])] -> Ok(type_)
        _ -> Error(Nil)
      }

    let variant =
      case re.scan(variant_re, line) {
        [Match(_txt, [Some(variant)])] -> Ok(variant)
        _ -> Error(Nil)
      }

    let field =
      case re.scan(field_re, line) {
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

    case acc.type_, acc.variant {
      Ok(type_), Ok(variant) -> {
        case parse_deriv_field_opts(line) {
          [] -> acc
          new_opts -> {
            let field =
              case acc.field {
                Ok(field) ->
                  field

                Error(Nil) ->
                  ""
              }

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
      }

      _, _ -> acc
    }
  })
  |> fn(acc) {
    acc.opts
  }
}

fn parse_deriv_field_opts(str: String) -> List(DerivFieldOpt) {
  let assert Ok(magic_comment_re) = re.from_string("\\s*[/][/][$]\\s*")
  let assert Ok(whitespace_re) = re.from_string("\\s+")

  case re.split(magic_comment_re, str) {
    [_, magic_comment] ->
      case re.split(whitespace_re, magic_comment) {
        [] -> []
        strs -> [DerivFieldOpt(strs:, raw: magic_comment)]
      }

    _ ->
      []
  }
}

//

type Token {
  Bare(String)
  Colon
  Str(String)
}

fn lexer() -> lexer.Lexer(Token, Nil) {
  lexer.simple([
    lexer.token(":", Colon),

    lexer.identifier("[a-z]", "[-_a-zA-Z0-9./]", set.new(), Bare),
    lexer.string("\"", Str),

    lexer.whitespace(Nil) |> lexer.ignore,
  ])
}

fn parser() {
  use key <- nibble.do(nibble.take_map("key bare", fn(t) {
    case t {
      Bare(key) -> Some(key)
      _ -> None
    }
  }))

  use _ <- nibble.do(nibble.token(Colon))

  use val <- nibble.do(
    nibble.one_of([
      nibble.take_map("val str", fn(t) {
        case t {
          Str(val) -> Some(val)
          _ -> None
        }
      }),
      nibble.take_map("val bare", fn(t) {
        case t {
          Bare(val) -> Some(val)
          _ -> None
        }
      }),
    ])
  )

  nibble.return(#(key, val))
}

pub fn parse_named_params(
  str str: String,
) -> Dict(String, String) {
  {
    use tokens <- result.try(str |> lexer.run(lexer()) |> result.replace_error(Nil))
    use pairs <- result.try(nibble.run(tokens, nibble.many(parser())) |> result.replace_error(Nil))
    Ok(dict.from_list(pairs))
  }
  |> result.unwrap(dict.new())
}

//


pub fn parse_type_gens(
  type_ type_: glance.CustomType,
  src src: String,
  ast ast: AST,
  parse parse: fn(String) -> Result(GleamPath, String),
) -> Result(TypeGen, Nil) {
  use raw <- result.try(parse_raw_type_gens(type_:, src:, ast:))
  Ok(from_raw(raw, parse))
}

pub fn parse_raw_type_gens(
  type_ type_: glance.CustomType,
  src src: String,
  ast ast: AST
) -> Result(RawTypeGen, Nil) {
  use glance.Definition(_, ct) <- result.try(dict.get(ast.custom_types, type_.name))

  use src <- result.try(dg.read_span(src:, span: ct.location))

  use rest <- result.try(src |> string.split("{") |> list.rest)

  let lines =
    rest
    |> string.join("{")
    |> string.split("\n")
    |> list.map(string.trim)

  Ok(parse_type_gens_(lines:))
}

pub type RawTypeGen {
  RawTypeGen(
    gens: List(String),
    opts: Dict(#(String, Option(String)), List(String)),
  )
}

pub type TypeGen {
  TypeGen(
    gens: List(#(GleamPath, String, String)),
    opts: Dict(#(String, Option(String), String), String),
  )
}

pub fn from_raw(
  gen gen: RawTypeGen,
  parse parse: fn(String) -> Result(GleamPath, String),
) -> TypeGen {
  let gens =
    gen.gens
    |> list.filter_map(fn(str) {
      let #(gen_str, rest_str) =
        case string.split(str, " ") {
          [] -> #(str, "")
          [str, ..rest] -> #(str, rest |> string.join(" ") |> string.trim)
        }

      case string.split(gen_str, ".") {
        [] | [_] | [_, _, _, ..] -> {
          io.log_err([
            "failed to parse type gen:",
            string.inspect(gen),
          ])
          Error(Nil)
        }

        [str, func] ->
          case parse(str) {
            Error(err) -> {
              io.log_err([
                "failed to parse type gen gleam module path:",
                err,
              ])
              Error(Nil)
            }

            Ok(path) -> Ok(#(path, func, rest_str))
          }
      }
    })

  let opts =
    gen.opts
    |> dict.to_list
    |> list.flat_map(fn(t) {
      let #(#(var, field), strs) = t

      strs
      |> list.map(fn(str) {
        let #(key, val) =
          case string.split(str, " ") {
            [] -> #(str, "")
            [key, ..rest] -> #(key, rest |> string.join(" "))
          }

        #(#(var, field, key), val)
      })
    })
    // |> list.group(fn(t) {
    //   let #(#(_var, _field, _key), _str) = t
    //   todo
    // })
    |> dict.from_list

  TypeGen(gens:, opts:)
}

type Match {
  MagicComment
  Variant
  Field
}

fn parse_type_gens_(
  lines lines: List(String),
) {
  let assert Ok(magic_comment_re) =
    "^[/][/][$]\\s*(.+)" |> re.from_string

  let magic_comment = fn(str) {
    case re.scan(magic_comment_re, str) {
      [re.Match(_, submatches: [Some(str)])] -> Ok(#(MagicComment, str))
      _ -> Error(Nil)
    }
  }

  let assert Ok(variant_re) =
    "^([A-Z][A-Za-z0-9]+)" |> re.from_string

  let variant = fn(str) {
    case re.scan(variant_re, str) {
      [re.Match(_, submatches: [Some(str)])] -> Ok(#(Variant, str))
      _ -> Error(Nil)
    }
  }

  let assert Ok(field_re) =
    "^([a-z][_a-z0-9]+)" |> re.from_string

  let field = fn(str) {
    case re.scan(field_re, str) {
      [re.Match(_, submatches: [Some(str)])] -> Ok(#(Field, str))
      _ -> Error(Nil)
    }
  }

  let scan = fn(str) {
    [
      magic_comment,
      variant,
      field
    ]
    |> list.find_map(fn(f) { f(str) })
  }

  let opts: List(#(String, Option(String), String)) = []

  list.fold(lines, #(None, None, [], opts), fn(acc, line) {
    let #(var, field, gens, opts) = acc

    case scan(line) {
      Error(Nil) ->
        acc

      Ok(#(MagicComment, str)) ->
        case var {
          None ->
            #(var, field, gens |> list.append([str]), opts)

          Some(var) ->
            #(Some(var), field, gens, opts |> list.append([#(var, field, str)]))
        }

      Ok(#(Variant, var)) ->
        #(Some(var), None, gens, opts)

      Ok(#(Field, field)) ->
        #(var, Some(field), gens, opts)
    }
  })
  |> fn(acc) {
    let #(_var, _field, gens, opts) = acc

    let opts =
      opts
      |> list.group(fn(t) {
        let #(var, field, _str) = t
        #(var, field)
      })
      |> dict.map_values(fn(_, vals) {
        list.map(vals, fn(val) {
          let #(_var, _field, str) = val
          str
        })
      })

    RawTypeGen(gens:, opts:)
  }
}
