import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import gleam/regexp
import simplifile
import shellout
import deriv/util
import deriv/types.{type File, File, type Output, Output, type Gen, Gen, type Derivation, Derivation}
import deriv/parser

type GenFunc = fn(CustomType, List(String), File) -> String
fn all_gen_funcs() -> Dict(String, GenFunc) {
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

pub fn main() {
  let filepaths = find_project_src_gleam_filepaths()

  filepaths
  |> gen_derivs()
  |> write_to_files()
}

fn find_project_src_gleam_filepaths() -> List(String) {
  let assert Ok(output) = shellout.command(in: ".", run: "find", with: ["src", "-name", "*.gleam"], opt: [])

  output
  |> string.trim
  |> string.split("\n")
}

// GEN DERIVS

fn gen_derivs(filepaths: List(String)) -> List(Gen) {
  let gen_funcs = all_gen_funcs()

  filepaths
  |> list.index_map(fn(path, idx) {
    let file = read_file(path, idx)

    file
    |> parse_types_and_derivations
    |> gen_type_derivs(file, gen_funcs)
  })
  |> list.flatten
}

fn read_file(filepath: String, idx: Int) -> File {
  let assert Ok(src) = simplifile.read(filepath)
  let module = file_path_to_gleam_module_str(filepath)
  File(module: , src:, idx: idx+1)
}

fn file_path_to_gleam_module_str(path: String) -> String {
  let assert Ok(leading_src_slash) = regexp.from_string("^src[/]")
  let assert Ok(trailing_dot_gleam) = regexp.from_string("[.]gleam$")

  path
  |> regexp.replace(each: leading_src_slash, in: _, with: "")
  |> regexp.replace(each: trailing_dot_gleam, in: _, with: "")
}

fn parse_types_and_derivations(file: File) -> List(#(CustomType, List(Derivation))) {
  let assert Ok(parsed) = glance.module(file.src)

  parsed.custom_types
  |> list.map(fn(ct) { ct.definition })
  |> list.map(parser.parse_type_with_derivations(_, file.src))
  |> result.values
}

fn gen_type_derivs(
  xs: List(#(CustomType, List(Derivation))),
  file: File,
  gen_funcs: Dict(String, GenFunc),
) -> List(Gen) {
  xs
  |> list.flat_map(gen_type_derivs_(_, file, gen_funcs))
}

fn gen_type_derivs_(
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

// WRITE TO FILES

fn write_to_files(xs: List(Gen)) -> Nil {
  xs
  |> list.group(fn(gen) {
    Output(module: gen.file.module, deriv: gen.deriv.name)
  })
  |> dict.each(fn(output, gens) {
    let output_path = output_path(output)
    let output_src = build_output_src(gens)

    io.println("// " <> output_path)
    io.println(output_src)

    let dir = string.replace(output_path, {output.deriv <> ".gleam"}, "")
    let assert Ok(_) = simplifile.create_directory_all(dir)
    let assert Ok(_) = simplifile.write(output_path, output_src)
  })
}

fn output_path(output: Output) -> String {
  [
    "src",
    "deriv",
    output.module,
    output.deriv <> ".gleam",
  ]
  |> string.join("/")
}

fn build_output_src(gens: List(Gen)) -> String {
  let module_imports = build_module_imports(gens)
  let deriv_imports = build_deriv_imports(gens)

  let defs = list.map(gens, fn(gen) { gen.src })

  [
    module_imports,
    deriv_imports,
  ]
  |> list.filter(fn(str) { str != "" })
  |> string.join("\n")
  |> fn(imports) { [imports] }
  |> list.append(defs)
  |> string.join("\n\n")
}

fn build_module_imports(gens: List(Gen)) -> String {
  let files =
    gens
    |> list.map(fn(g) { g.file })
    |> list.unique

  list.map(files, fn(file) {
    "import MODULE as mINDEX"
    |> string.replace(each: "MODULE", with: file.module)
    |> string.replace(each: "INDEX", with: file.idx |> int.to_string)
  })
  |> string.join("\n")
}

fn build_deriv_imports(gens: List(Gen)) -> String {
  let derivs = list.map(gens, fn(g) { g.deriv })

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
  |> result.unwrap("")
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
           encode_lines |> list.map(util.indent(_, level: 2)) |> string.join("\n"),
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

fn decoder_func_src(type_: JsonType, file: File) -> String {
  let decode_field_lines =
    type_.fields
    |> list.map(field_decode_line)
    |> list.map(util.indent(_, level: 1))

  let success =
    decode_success_line(type_, file)
    |> util.indent(level: 1)

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
