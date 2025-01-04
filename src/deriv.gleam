import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type Module, type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import gleam/regexp.{Match}
import simplifile
import shellout

fn file_path_to_gleam_module_str(path: String) -> String {
  let assert Ok(leading_src_slash) = regexp.from_string("^src[/]")
  let assert Ok(trailing_dot_gleam) = regexp.from_string("[.]gleam$")

  path
  |> regexp.replace(each: leading_src_slash, in: _, with: "")
  |> regexp.replace(each: trailing_dot_gleam, in: _, with: "")
}

fn terminal_module_string(module: String) -> String {
  let assert Ok(str) =
    module
    |> string.split("/")
    |> list.last

  str
}

fn gen_imports(files: List(File)) -> String {
  list.map(files, fn(file) {
    "import MODULE as mINDEX"
    |> string.replace(each: "MODULE", with: file.module)
    |> string.replace(each: "INDEX", with: file.idx |> int.to_string)
  })
  |> list.append([
    "import decode/zero.{type Decoder} as decode",
  ])
  |> string.join("\n")
}

fn deriv_output_path() -> String {
  let assert Ok(src) = simplifile.read("gleam.toml")

  let assert Ok(re) = regexp.from_string("name = \"(\\w+)\"")

  let assert [Match(_, [Some(pkg)])] = regexp.scan(re, src)

  "src/PKG/derivs.gleam"
  |> string.replace(each: "PKG", with: pkg)
}

pub fn main() {
  let assert Ok(output) = shellout.command(in: ".", run: "find", with: ["src", "-name", "*.gleam"], opt: [])
  let filepaths =
    output
    |> string.trim
    |> string.split("\n")

  let output_str =
    filepaths
    |> list.index_map(fn(path, idx) {
      let assert Ok(src) = simplifile.read(path)
      let module = file_path_to_gleam_module_str(path)
      let file = File(module: , src:, idx: idx+1)

      let assert Ok(parsed) = glance.module(src)

      let gen_src =
        parsed.custom_types
        |> list.map(fn(ct) { ct.definition })
        |> list.map(type_with_derivations(_, src))
        |> result.partition
        |> fn(x) {
          let #(oks, _errs) = x
          oks
        }
        |> list.map(gen_derivations(_, file))
        |> string.join("\n\n")

      Gen(file: file, src: gen_src)
    })
    |> list.filter(fn(gen) { gen.src != "" })
    |> gen_full_deriv_src
    |> function.tap(fn(src) { io.println(src) })

  let output_path = deriv_output_path()

  // simplifile.create_directory_all
  // let assert Ok(_) = simplifile.write(output_path, output_str)

  Nil
}

fn gen_full_deriv_src(gens: List(Gen)) -> String {
  let imports =
    gens
    |> list.map(fn(gen) { gen.file })
    |> gen_imports

  let derivs_src =
    gens
    |> list.map(fn(gen) { gen.src })

  [imports]
  |> list.append(derivs_src)
  |> string.join("\n\n")
}

fn gen_derivations(x: #(CustomType, List(Derivation)), file: File) -> String {
  let #(type_, derivs) = x

  let ds = derivations() // TODO call once, pass from `main`

  derivs
  |> list.map(fn(d) {
    case dict.get(ds, d.name) {
      Ok(f) -> f(type_, d.opts, file)
      _ -> ""
    }
  })
  |> list.filter(fn(str) { str != "" })
  |> string.join("\n\n")
}

fn derivations() -> Dict(String, fn(CustomType, List(String), File) -> String) {
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

// fn types_with_magic_comments(module: Module, src: String) -> List(#(CustomType, String)) {
// }

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

  // let lines_for_type_def =
  //   list.append(
  //     lines_from_type_start_except_last,
  //     [line_last_for_type],
  //   )

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
      "pub fn encode_" <> type_.name <> "(value: m" <> int.to_string(file.idx) <> "." <>  type_.name <> ") -> Json {",
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
  |> list.map(to_json_type)
}

fn to_json_type(variant: Variant) -> JsonType {
  let xs = list.map(variant.fields, to_json_field)

  case result.all(xs) {
    Ok(fields) ->
      JsonType(variant:, fields:)

    Error(field) -> {
      io.debug(field)
      panic as "Could not process the above field"
    }
  }
}

// fn decoder_src(type_: JsonType) -> String {
//   let xs = list.map(type_.variant.fields, to_json_field)

//   case result.all(xs) {
//     Ok(json_fields) ->
//       decoder_func_src(variant, json_fields)

//     Error(field) -> {
//       io.debug(field)
//       panic as "Could not process the above field"
//     }
//   }
// }

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

  let func_name = "decoder_" <> type_.variant.name

  [
    [
      "pub fn " <> func_name <> "() -> Decoder(m" <> {int.to_string(file.idx)} <> "." <> type_.variant.name <> ") {",
      // TODO `Decoder(TYPE)` rather than `Decoder(VARIANT)`
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
