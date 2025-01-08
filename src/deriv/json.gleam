import gleam/option.{None}
import gleam/dict
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/regexp
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import deriv/types.{type Imports, type File, File, type Gen}
import deriv/util

pub fn gen(type_: CustomType, opts: List(String), file: File) -> String {
  let gen_funcs_for_opts =
    [
      #("decode", gen_json_decoders),
      #("encode", gen_json_encoders),
    ]
    |> dict.from_list

  opts
  |> list.map(dict.get(gen_funcs_for_opts, _))
  |> result.values
  |> list.map(fn(f) { f(type_, file)})
  |> string.join("\n\n")
}

fn needs_util_import(gens: List(Gen)) -> Bool {
  let assert Ok(contains_decoder_uuid) = regexp.from_string("decoder_uuid")
  let assert Ok(contains_encode_uuid) = regexp.from_string("encode_uuid")

  let checks =
    [
      contains_decoder_uuid,
      contains_encode_uuid,
    ]

  list.any(gens, fn(gen) {
    list.any(checks, fn(re) {
      regexp.check(re, gen.src)
    })
  })
}

pub const imports: Imports =
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
    type_: String,
  )
}

fn gen_json_decoders(type_: CustomType, file: File) -> String {
  to_json_types(type_)
  |> list.map(decoder_func_src(_, file))
  |> string.join("\n")
}

fn gen_json_encoders(type_: CustomType, file: File) -> String {
  type_
  |> to_json_types
  |> list.map(fn(jt) {
    let encode_lines =
      jt.fields
      |> list.map(fn(f) {
        let encode_func =
          case f.type_ {
            "Int" -> "json.int"
            "String" -> "json.string"
            "Bool" -> "json.bool"
            "Uuid" -> "util.encode_uuid"
            _ -> panic as { "Unsupported field type for JSON decode: " <> f.type_ }
          }

        "#(\"NAME\", FUNC(value.NAME)),"
        |> string.replace(each: "NAME", with: f.name)
        |> string.replace(each: "FUNC", with: encode_func)
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

fn to_json_field(field: VariantField) -> Result(JsonField, VariantField) {
  case field {
    LabelledVariantField(NamedType("Uuid", None, []), name) ->
      Ok(JsonField(name:, type_: "Uuid"))

    LabelledVariantField(NamedType("String", None, []), name) ->
      Ok(JsonField(name:, type_: "String"))

    LabelledVariantField(NamedType("Int", None, []), name) ->
      Ok(JsonField(name:, type_: "Int"))

    LabelledVariantField(NamedType("Bool", None, []), name) ->
      Ok(JsonField(name:, type_: "Bool"))

    _ ->
      Error(field)
  }
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
  let decode_func =
    case field.type_ {
      "Int" -> "decode.int"
      "String" -> "decode.string"
      "Bool" -> "decode.bool"
      "Uuid" -> "util.decoder_uuid"
      _ -> panic as { "Unsupported field type for JSON decode: " <> field.type_ }
    }

  [
    "use ",
    field.name,
    " <- decode.field(\"",
    field.name,
    "\", ",
    decode_func,
    ")",
  ]
  |> string.join("")
}
