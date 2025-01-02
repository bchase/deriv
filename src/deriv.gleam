import gleam/option.{None}
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import simplifile

pub type Foo {
  Foo(
    id: Int,
    name: String,
    active: Bool,
  )
}

pub fn main() {
  let assert Ok(source) = simplifile.read("src/deriv.gleam")
  let assert Ok(parsed) = glance.module(source)

  // io.debug(source)
  // io.debug(parsed.custom_types)

  parsed.custom_types
  |> list.map(fn(ct) { ct.definition })
  |> list.map(decoders_src)
  |> list.each(fn(d) { io.println(d) })
}

pub type JsonField {
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

fn decoders_src(type_: CustomType) -> String {
  type_.variants
  |> list.map(decoder_src)
  |> string.join("\n\n")
}

fn decoder_src(variant: Variant) -> String {
  let xs = list.map(variant.fields, to_json_field)

  case result.all(xs) {
    Ok(json_fields) ->
      decoder_func_src(variant, json_fields)

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

fn decoder_func_src(variant: Variant, fields: List(JsonField)) -> String {
  let decode_field_lines =
    fields
    |> list.map(field_decode_line)
    |> list.map(indent(_, level: 1))

  let return =
    decode_success_line(variant, fields)
    |> indent(level: 1)

  let func_name = "decoder_" <> variant.name

  [
    [
      "pub fn " <> func_name <> "() -> Decoder(" <> variant.name <> ") {",
    ],
    decode_field_lines,
    [
      "",
      return,
      "}",
    ],
  ]
  |> list.flatten
  |> string.join("\n")
}

fn decode_success_line(variant: Variant, fields: List(JsonField)) -> String {
  let field_name_args_str =
    fields
    |> list.map(fn(f) { f.name <> ":" })
    |> string.join(", ")

  [
    "decode.success(",
    variant.name,
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
