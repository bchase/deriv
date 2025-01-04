import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/set
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import gleam/regexp.{Match}
import simplifile
import shellout
import nibble.{do, return}
import nibble/lexer
import deriv/util

fn file_path_to_gleam_module_str(path: String) -> String {
  let assert Ok(leading_src_slash) = regexp.from_string("^src[/]")
  let assert Ok(trailing_dot_gleam) = regexp.from_string("[.]gleam$")

  path
  |> regexp.replace(each: leading_src_slash, in: _, with: "")
  |> regexp.replace(each: trailing_dot_gleam, in: _, with: "")
}

fn build_module_imports(files: List(File)) -> String {
  list.map(files, fn(file) {
    "import MODULE as mINDEX"
    |> string.replace(each: "MODULE", with: file.module)
    |> string.replace(each: "INDEX", with: file.idx |> int.to_string)
  })
  |> string.join("\n")
}

pub fn main() {
  let assert Ok(output) = shellout.command(in: ".", run: "find", with: ["src", "-name", "*.gleam"], opt: [])
  let filepaths =
    output
    |> string.trim
    |> string.split("\n")

  let gen_funcs = build_gen_funcs()

  filepaths
  |> list.index_map(fn(path, idx) {
    let assert Ok(src) = simplifile.read(path)
    let module = file_path_to_gleam_module_str(path)
    let file = File(module: , src:, idx: idx+1)

    let assert Ok(parsed) = glance.module(src)

    parsed.custom_types
    |> list.map(fn(ct) { ct.definition })
    |> list.map(type_with_derivations(_, src))
    |> result.values
    |> list.flat_map(gen_derivations(_, file, gen_funcs))
  })
  |> list.flatten
  |> list.group(fn(gen) {
    #(gen.file.module, gen.deriv.name)
  })
  |> dict.each(fn(group, gens) {
    let #(module, name) = group

    let output_path =
      [
        "src",
        "deriv",
        module,
        name <> ".gleam",
      ]
      |> string.join("/")

    let files =
      gens
      |> list.map(fn(g) { g.file })
      |> list.unique
    let module_imports = build_module_imports(files)

    let derivs = list.map(gens, fn(g) { g.deriv })
    let assert Ok(deriv_imports) = build_deriv_imports(derivs)

    let defs = list.map(gens, fn(gen) { gen.src })

    let output_str =
      [
        module_imports,
        deriv_imports,
      ]
      |> string.join("\n")
      |> fn(imports) { [imports] }
      |> list.append(defs)
      |> string.join("\n\n")

    io.println("// " <> output_path)
    io.println(output_str)

    let dir = string.replace(output_path, {name <> ".gleam"}, "")
    let assert Ok(_) = simplifile.create_directory_all(dir)
    let assert Ok(_) = simplifile.write(output_path, output_str)
  })

  io.debug(parse_deriv_field_opt("foo(bar(baz(asdf/qwer.zxcv)))"))
  io.debug(parse_deriv_field_opt("foo(bar(asdf/qwer.zxcv))"))

  Nil
}

fn build_deriv_imports(derivs: List(Derivation)) -> Result(String, Nil) {
  let deriv_imports = dict.from_list(deriv_imports)

  derivs
  |> list.flat_map(fn(d) {
    list.map(d.opts, fn(opt) {
      #(d.name, opt)
    })
  })
  |> list.unique
  |> list.map(dict.get(deriv_imports, _))
  |> result.values
  |> list.map(string.trim)
  |> fn(strs) {
    case strs {
      [] -> Error(Nil)
      _ -> Ok(string.join(strs, "\n"))
    }
  }
}

const deriv_imports =
  [
    #(#("json", "decode"), "
      import decode/zero.{type Decoder} as decode
    "),
    #(#("json", "encode"), "
      import gleam/json.{type Json}
    "),
  ]

fn gen_derivations(
  x: #(CustomType, List(Derivation)),
  file: File,
  gen_funcs: Dict(String, GenFunc),
) -> List(Gen) {
  let #(type_, derivs) = x

  derivs
  |> list.map(fn(d) {
    case dict.get(gen_funcs, d.name) {
      Error(_) -> Error(Nil)
      Ok(f) -> {
        let src = f(type_, d.opts, file)
        Ok(Gen(file:, deriv: d, src:))
      }
    }
  })
  |> result.values
}

type GenFunc = fn(CustomType, List(String), File) -> String

fn build_gen_funcs() -> Dict(String, GenFunc) {
  [
    #("json", fn(type_, opts, file) {
      let decoders =
        case list.contains(opts, "decode") {
          False -> ""
          True -> gen_json_decoders(type_, file)
        }

      let encoders =
        case list.contains(opts, "encode") {
          False -> ""
          True -> gen_json_encoders(type_, file)
        }

      [
        decoders,
        encoders
      ]
      |> list.filter(fn(str) { str != "" })
      |> string.join("\n\n")
    })
  ]
  |> dict.from_list
}

fn type_with_derivations(type_: CustomType, src: String) -> Result(#(CustomType, List(Derivation)), Nil) {
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
        #(type_, ds)
      })

    _ -> Error(Nil)
  }
}


type File {
  File(
    module: String,
    src: String,
    idx: Int,
  )
}

type Gen {
  Gen(
    file: File,
    deriv: Derivation,
    src: String,
  )
}

type Derivation {
  Derivation(
    name: String,
    opts: List(String),
  )
}

type JsonType {
  JsonType(
    type_: CustomType,
    variant: Variant,
    fields: List(JsonField),
  )
}

type JsonField {
  JsonField(
    name: String,
    func: String,
  )
}

fn to_json_field(field: VariantField) -> Result(JsonField, VariantField) {
  case field {
    LabelledVariantField(NamedType("String", None, []), name) ->
      Ok(JsonField(name:, func: "string"))

    LabelledVariantField(NamedType("Int", None, []), name) ->
      Ok(JsonField(name:, func: "int"))

    LabelledVariantField(NamedType("Bool", None, []), name) ->
      Ok(JsonField(name:, func: "bool"))

    _ ->
      Error(field)
  }
}

fn gen_json_encoders(type_: CustomType, file: File) -> String {
  type_
  |> to_json_types
  |> list.map(fn(jt) {
    let encode_lines =
      jt.fields
      |> list.map(fn(f) {
        "#(\"NAME\", json.FUNC(value.NAME)),"
        |> string.replace(each: "NAME", with: f.name)
        |> string.replace(each: "FUNC", with: f.func)
      })

    [
      "pub fn encode_" <> util.snake_case(type_.name) <> "(value: m" <> int.to_string(file.idx) <> "." <>  type_.name <> ") -> Json {",
      "  json.object([",
           encode_lines |> list.map(indent(_, level: 2)) |> string.join("\n"),
      "  ])",
      "}",
    ]
    |> string.join("\n")
  })
  |> string.join("\n\n")
}

fn gen_json_decoders(type_: CustomType, file: File) -> String {
  to_json_types(type_)
  |> list.map(decoder_func_src(_, file))
  |> string.join("\n")
}

fn to_json_types(type_: CustomType) -> List(JsonType) {
  type_.variants
  |> list.map(to_json_type(type_, _))
}

fn to_json_type(type_: CustomType, variant: Variant) -> JsonType {
  let xs = list.map(variant.fields, to_json_field)

  case result.all(xs) {
    Ok(fields) ->
      JsonType(type_:, variant:, fields:)

    Error(field) -> {
      io.debug(field)
      panic as "Could not process the above field"
    }
  }
}

fn indent(str: String, level level: Int) {
  let pad =
    "  "
    |> list.repeat(level)
    |> string.join("")

  pad <> str
}

fn decoder_func_src(type_: JsonType, file: File) -> String {
  let decode_field_lines =
    type_.fields
    |> list.map(field_decode_line)
    |> list.map(indent(_, level: 1))

  let success =
    decode_success_line(type_, file)
    |> indent(level: 1)

  let func_name = "decoder_" <> util.snake_case(type_.type_.name)

  [
    [
      "pub fn " <> func_name <> "() -> Decoder(m" <> {int.to_string(file.idx)} <> "." <> type_.type_.name <> ") {",
    ],
    decode_field_lines,
    [
      "",
      success,
      "}",
    ],
  ]
  |> list.flatten
  |> string.join("\n")
}

fn decode_success_line(type_: JsonType, file: File) -> String {
  let field_name_args_str =
    type_.fields
    |> list.map(fn(f) { f.name <> ":" })
    |> string.join(", ")

  [
    "decode.success(m",
    int.to_string(file.idx),
    ".",
    type_.variant.name,
    "(",
    field_name_args_str,
    "))",
  ]
  |> string.join("")
}

fn field_decode_line(field: JsonField) -> String {
  [
    "use ",
    field.name,
    " <- decode.field(\"",
    field.name,
    "\", decode.",
    field.func,
    ")",
  ]
  |> string.join("")
}

// MAGIC COMMENT PARSER

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

// DERIV PARSER

type DerivFieldOpt {
  DerivFieldOpt(
    deriv: String,
    opt: Option(String),
    key: String,
    val: String,
  )
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
