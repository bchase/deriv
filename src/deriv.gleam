import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type Module, type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import gleam/regexp.{Match}
import simplifile

pub type Foo {
  Foo(
    id: Int,
    name: String,
    active: Bool,
  )
} //$ derive json(decode)

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
  |> list.map(type_with_derivations(_, source))
  |> result.partition
  |> fn(x) {
    let #(oks, _errs) = x
    oks
  }
  |> list.map(gen_derivations)
  |> list.each(fn(str) { io.println(str) })
}

fn gen_derivations(x: #(CustomType, List(Derivation))) -> String {
  let #(type_, derivs) = x

  let ds = derivations() // TODO call once, pass from `main`

  derivs
  |> list.map(fn(d) {
    case dict.get(ds, d.name) {
      Ok(f) -> f(type_, d.opts)
      _ -> ""
    }
  })
  |> list.filter(fn(str) { str != "" })
  |> string.join("\n\n")
}

fn derivations() -> Dict(String, fn(CustomType, List(String)) -> String) {
  [
    #("json", fn(type_, opts) {
      let decoders =
        case list.contains(opts, "decode") {
          False -> ""
          True -> gen_json_decoders(type_)
        }

      let encoders =
        case list.contains(opts, "encode") {
          False -> ""
          True -> gen_json_encoders(type_)
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

  let lines_for_type_def =
    list.append(
      lines_from_type_start_except_last,
      [line_last_for_type],
    )

  case string.split(line_last_for_type, "//$") {
    [_, mc] ->
      parse_derivations(mc)
      |> result.map(fn(ds) {
        #(type_, ds)
      })

    _ -> Error(Nil)
  }
}


pub type JsonType {
  JsonType(
    variant: Variant,
    fields: List(JsonField),
  )
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

fn gen_json_encoders(type_: CustomType) -> String {
  "encode"
}

fn gen_json_decoders(type_: CustomType) -> String {
  to_json_types(type_)
  |> list.map(decoder_func_src)
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

fn decoder_func_src(type_: JsonType) -> String {
  let decode_field_lines =
    type_.fields
    |> list.map(field_decode_line)
    |> list.map(indent(_, level: 1))

  let success =
    decode_success_line(type_.variant, type_.fields)
    |> indent(level: 1)

  let func_name = "decoder_" <> type_.variant.name

  [
    [
      "pub fn " <> func_name <> "() -> Decoder(" <> type_.variant.name <> ") {",
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

// MAGIC COMMENT PARSER

pub type Derivation {
  Derivation(
    name: String,
    opts: List(String),
  )
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
