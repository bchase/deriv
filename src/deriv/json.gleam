import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, NamedType}
import deriv/types.{type Import, Import, type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen}
import deriv/util

pub fn gen(type_: CustomType, deriv: Derivation, field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> Gen {
  let opts = deriv.opts

  let gen_funcs_for_opts =
    [
      #("decode", gen_json_decoders),
      #("encode", gen_json_encoders),
    ]
    |> dict.from_list

  let imports =
    gen_imports(opts, type_)

  let other_src =
    opts
    |> list.map(dict.get(gen_funcs_for_opts, _))
    |> result.values
    |> list.map(fn(f) { f(type_, field_opts, file)})
    |> string.join("\n\n")

  let src =
    case type_.variants {
      [] ->
        panic as "`CustomType` has no `variants`"

      [_invariant] ->
        [other_src]

      _multi_variants -> {
        let multi_variant_type_decoders_src =
          decoder_funcs_for_multi_variant_type(type_, field_opts, file)

        let dummy_func_src =
          dummy_func_src(type_, file)

        [
          multi_variant_type_decoders_src,
          dummy_func_src,
          other_src,
        ]
      }
    }
    |> string.join("\n\n")


  Gen(file:, deriv:, src:, imports:)
}

fn gen_imports(opts: List(String), type_: CustomType) -> List(Import) {
  let json_imports =
    [
      #("decode", [
        // import decode/zero.{type Decoder} as decode
        Import(
          module: "decode/zero",
          types: ["Decoder"],
          constructors: [],
          alias: Some("decode"),
        ),
      ]),
      #("encode", [
        // import gleam/json.{type Json}
        Import(
          module: "gleam/json",
          types: ["Json"],
          constructors: [],
          alias: None,
        ),
      ]),
      ]
    |> dict.from_list

  opts
  |> list.unique
  |> list.map(fn(opt) {
    dict.get(json_imports, opt)
  })
  |> result.values
  |> list.flatten
  |> fn(imports) {
    case needs_util_import(type_) {
      False -> imports
      True -> {
        let util_import =
          // import deriv/util
          Import(
            module: "deriv/util",
            types: [],
            constructors: [],
            alias: None,
          )

        list.append(imports, [util_import])
      }
    }
  }
}

fn needs_util_import(type_: CustomType) -> Bool {
  is_multi_variant(type_) || uses_uuid(type_)
}

fn is_multi_variant(type_: CustomType) -> Bool {
  list.length(type_.variants) > 1
}

fn uses_uuid(type_: CustomType) -> Bool {
  type_
  |> to_json_types // TODO duplicate work
  |> list.any(fn(json_type) {
    list.any(json_type.fields, fn(field) {
      field.type_ == "Uuid"
    })
  })
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
    type_: String,
  )
}

fn gen_json_decoders(type_: CustomType, field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> String {
  to_json_types(type_)
  |> list.map(decoder_func_src(_, field_opts, file))
  |> string.join("\n")
}

fn gen_json_encoders(type_: CustomType, all_field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> String {
  type_
  |> to_json_types
  |> list.map(fn(jt) {
    let encode_lines =
      jt.fields
      |> list.map(fn(f) {
        let field_opts =
          all_field_opts
          |> dict.get(f.name)
          |> result.unwrap([])

        let json_field_name = json_field_name(f, field_opts)

        let encode_func =
          case f.type_ {
            "Int" -> "json.int"
            "String" -> "json.string"
            "Bool" -> "json.bool"
            "Uuid" -> "util.encode_uuid"
            _ -> panic as { "Unsupported field type for JSON decode: " <> f.type_ }
          }

        "#(\"JSON\", FUNC(value.GLEAM)),"
        |> string.replace(each: "JSON", with: json_field_name)
        |> string.replace(each: "FUNC", with: encode_func)
        |> string.replace(each: "GLEAM", with: f.name)
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

fn decoder_func_name(type_: JsonType) -> String {
  "decoder_" <> type_variant_snake_case(type_)
}

fn type_variant_snake_case(type_: JsonType) -> String {
  case type_.type_.variants {
    [] ->
      panic as "`CustomType` has no `variants`"

    [invariant] ->
      case invariant.name == type_.type_.name {
        True -> type_variant_snake_case_without_type(type_)
        False -> type_variant_snake_case_with_type(type_)
      }

    _multi_variant ->
      type_variant_snake_case_with_type(type_)
  }
}

// TODO consolidate?
fn type_variant_snake_case_with_type_(type_: CustomType, var: Variant) -> String {
  util.snake_case(type_.name) <> "_" <> util.snake_case(var.name)
}
fn type_variant_snake_case_with_type(type_: JsonType) -> String {
  util.snake_case(type_.type_.name) <> "_" <> util.snake_case(type_.variant.name)
}

fn type_variant_snake_case_without_type(type_: JsonType) -> String {
  util.snake_case(type_.variant.name)
}

fn decoder_func_src(type_: JsonType, all_field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> String {
  let func_name = decoder_func_name(type_)

  case type_.type_.variants {
    [] -> panic as "`CustomType` has no `variants`"

    [_invariant] ->
      decoder_func_src_invariant(
        func_name,
        type_,
        all_field_opts,
        file,
      )

    _multi_variant ->
      decoder_func_src_multi_variant(
        func_name,
        type_,
        all_field_opts,
        file,
      )
  }
}

fn util_call_for_dummy_gleam_type(type_: String) -> String {
  case type_ {
    "String" -> "util.dummy_string()"
    "Int" -> "util.dummy_int()"
    "Bool" -> "util.dummy_bool()"
    "Uuid" -> "util.dummy_uuid()"
    _ -> panic as { "Unsupported type: `" <> type_ <> "`" }
  }
}

fn dummy_func_src(type_: CustomType, file: File) -> String {
  let func_name = "dummy_" <> util.snake_case(type_.name)

  let return = "m" <> int.to_string(file.idx) <> "." <> type_.name

  let variant =
    case type_.variants {
      [] ->
        panic as "`CustomType` has no `variants`"

      [var] ->
        var

      [var, ..] ->
        var
    }
  let variant_str =
    "m" <> int.to_string(file.idx) <> "." <> variant.name

  let field_assignments_src =
    variant.fields
    |> list.map(fn(field) {
      case field {
        LabelledVariantField(NamedType(type_, None, []), name) ->
          name <> ": " <> util_call_for_dummy_gleam_type(type_) <> ","

        LabelledVariantField(NamedType(_, _, _), _) as var -> {
          io.debug(var) // what is this case?
          panic as "handling this kind of `glance.LabelledVariantField` is unimplemented"
        }

        _ ->
          panic as "handling `glance.UnlabelledVariantField` is unimplemented"
      }
    })
    |> list.map(util.indent(_, level: 2))
    |> string.join("\n")

  [
    // fn dummy_t() -> m1.T {
    "fn " <> func_name <> "() -> " <> return <> " {",
    //   m1.Var1(
    { variant_str <> "(" } |> util.indent(level: 1),
    field_assignments_src,
    //     var1: util.dummy_string(),
    { ")" } |> util.indent(level: 1),
    "}",
  ]
  |> string.join("\n")
}

fn util_decode_type_field_call_src(
  type_: JsonType,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
  indent_level indent_level: Int,
) -> String {
  let decode_body: String = decoder_body_src(type_, all_field_opts, file, indent_level: indent_level + 1)

  let json_field = "_type"
  let dummy_func_name = dummy_func_name(type_.type_)

  let open_line = "util.decode_type_field(variant: \"" <> type_.variant.name <> "\", json_field: \"" <> json_field <> "\", fail_dummy: " <> dummy_func_name <> "(), pass: {"
  let close_line = "})"

  [
    open_line,
    close_line,
  ]
  |> list.map(util.indent(_, level: indent_level))
  |> list.intersperse(decode_body)
  |> string.join("\n")
}

fn dummy_func_name(type_: CustomType) -> String {
  "dummy_" <> util.snake_case(type_.name)
}

fn decoder_funcs_for_multi_variant_type(
  type_: CustomType,
  _all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
) {
  let type_snake_case = util.snake_case(type_.name)

  let func_def = "pub fn decoder_" <> type_snake_case <> "() -> Decoder(m" <> {int.to_string(file.idx)} <> "." <> type_.name <> ")"

  let failure_decoder =
    "decode.failure(DUMMY(), \"No `Decoder(TYPE)` succeeded\")"
    |> string.replace(each: "DUMMY", with: dummy_func_name(type_))
    |> string.replace(each: "TYPE", with: type_.name)

  let variant_decoder_funcs_list_lines =
    type_.variants
    |> list.map(fn(v) {
      let f = type_variant_snake_case_with_type_(type_, v)

      { "decoder_" <> f <> "()," }
      |> util.indent(level: 2)
    })
    |> string.join("\n")

  [
    func_def <> " {",
    failure_decoder |> util.indent(level: 1),
    "|> decode.one_of([" |> util.indent(level: 1),
    variant_decoder_funcs_list_lines,
    "])" |> util.indent(level: 1),
    "}",
  ]
  |> string.join("\n")
}

fn decoder_func_src_multi_variant(
  func_name: String,
  type_: JsonType,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
) -> String {
  let func_def = "fn " <> func_name <> "() -> Decoder(m" <> {int.to_string(file.idx)} <> "." <> type_.type_.name <> ")"

  let decode_body: String = util_decode_type_field_call_src(type_, all_field_opts, file, indent_level: 1)

  [
    func_def <> " {",
    decode_body,
    "}",
  ]
  |> string.join("\n")
}

fn decoder_func_src_invariant(
  func_name: String,
  type_: JsonType,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
) -> String {
  let func_def = "pub fn " <> func_name <> "() -> Decoder(m" <> {int.to_string(file.idx)} <> "." <> type_.type_.name <> ")"

  let decode_body: String = decoder_body_src(type_, all_field_opts, file, indent_level: 1)

  [
    func_def <> " {",
    decode_body,
    "}",
  ]
  |> string.join("\n")
}

fn decoder_body_src(
  type_: JsonType,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
  indent_level indent_level: Int,
) -> String {
  let lines =
    type_
    |> decode_field_lines(all_field_opts)
    |> list.map(util.indent(_, level: indent_level))

  let success =
    type_
    |> decode_success_line(file)
    |> util.indent(level: indent_level)

  [
    lines |> string.join("\n"),
    success,
  ]
  |> string.join("\n\n")
}

fn decode_field_lines(type_: JsonType, all_field_opts: Dict(String, List(DerivFieldOpt))) -> List(String) {
  type_.fields
  |> list.map(fn(field) {
    let field_opts =
      all_field_opts
      |> dict.get(field.name)
      |> result.unwrap([])

    field_decode_line(field, field_opts)
  })
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

fn json_field_name(field: JsonField, field_opts: List(DerivFieldOpt)) -> String {
  field_opts
  |> list.find(fn(f) { f.deriv == "json" && f.key == "named" })
  |> fn(x) {
    case x {
      Error(_) -> field.name
      Ok(field_opt) -> field_opt.val
    }
  }
}

fn field_decode_line(field: JsonField, field_opts: List(DerivFieldOpt)) -> String {
  let json_field_name = json_field_name(field, field_opts)

  let decode_func =
    case field.type_ {
      "Int" -> "decode.int"
      "String" -> "decode.string"
      "Bool" -> "decode.bool"
      "Uuid" -> "util.decoder_uuid()"
      _ -> panic as { "Unsupported field type for JSON decode: " <> field.type_ }
    }

  [
    "use ",
    field.name,
    " <- decode.field(\"",
    json_field_name,
    "\", ",
    decode_func,
    ")",
  ]
  |> string.join("")
}
