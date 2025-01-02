import gleam/option.{None}
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type Module, type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
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

  // parsed.custom_types
  // |> list.map(fn(ct) { ct.definition })
  // |> list.map(to_json_types)
  // |> list.flatten
  // |> list.map(decoder_func_src)

  // parsed.custom_types
  // |> list.map(fn(ct) { ct.definition })
  // |> list.map(decoders_src)
  // |> list.each(fn(d) { io.println(d) })

  parsed.custom_types
  |> list.map(fn(ct) { ct.definition })
  |> list.map(type_with_magic_comments(_, source))
  |> result.partition
  |> fn(x) {
    let #(oks, _errs) = x
    oks
  }
  |> io.debug
}

// fn types_with_magic_comments(module: Module, src: String) -> List(#(CustomType, String)) {
// }

fn type_with_magic_comments(type_: CustomType, src: String) -> Result(#(CustomType, String), Nil) {
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

  let lines_for_type_def =
    list.append(
      lines_from_type_start_except_last,
      [line_last_for_type],
    )

  case string.split(line_last_for_type, "//$") {
    [_, mc] -> Ok(#(type_, string.trim(mc)))
    _ -> Error(Nil)
  }
}


pub type JsonType {
  JsonType(
    variant: Variant,
    fields: List(JsonField),
  )
} //$ derive json(decode, encode)

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

fn decoder_func_src(type_: JsonType) -> String {
  let decode_field_lines =
    type_.fields
    |> list.map(field_decode_line)
    |> list.map(indent(_, level: 1))

  let return =
    decode_success_line(type_.variant, type_.fields)
    |> indent(level: 1)

  let func_name = "decoder_" <> type_.variant.name

  [
    [
      "pub fn " <> func_name <> "() -> Decoder(" <> type_.variant.name <> ") {",
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
