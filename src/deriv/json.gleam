import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, UnlabelledVariantField, NamedType, type Import, Import, UnqualifiedImport, Module, Definition, CustomType, Public, Variant, Function, FieldAccess, Variable, Span, Expression, Call, UnlabelledField, Block, Use, BinaryOperator, Pipe, PatternVariable, ShorthandField, String, FunctionParameter, Tuple, Named, List, type Definition, type Function, type Span, type Expression, type Statement, type Type}
import deriv/types.{type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen} as deriv
import deriv/util

pub fn gen(type_: CustomType, deriv: Derivation, field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> Gen {
  let opts = deriv.opts

  let gen_funcs_for_opts =
    [
      // #("decode", gen_json_decoders),
      // #("encode", gen_json_encoders),
    ]
    |> dict.from_list

  let imports =
    gen_imports(opts, type_)

  let other_funcs =
    opts
    |> list.map(dict.get(gen_funcs_for_opts, _))
    |> result.values
    |> list.flat_map(fn(f) { f(type_, field_opts, file)})

  let multi_variant_type_decoders_funcs =
    case type_.variants {
      [] ->
        panic as "`CustomType` has no `variants`"

      [_invariant] ->
        []

      _multi_variants ->
        [decoder_func_for_multi_variant_type(type_, field_opts, file)]
    }

  let funcs =
    [
      multi_variant_type_decoders_funcs,
      other_funcs,
    ]
    |> list.flatten

  let src =
    funcs
    |> list.map(fn(f) { f.src })
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
}

fn gen_imports(opts: List(String), type_: CustomType) -> List(Import) {
  let json_imports =
    [
      #("decode", [
        // import decode.{type Decoder}
        Import(
          module: "decode",
          alias: None,
          unqualified_types: [
            UnqualifiedImport(
              name: "Decoder",
              alias: None,
            ),
          ],
          unqualified_values: [],
        ),
      ]),
      #("encode", [
        // import gleam/json.{type Json}
        Import(
          module: "gleam/json",
          alias: None,
          unqualified_types: [
            UnqualifiedImport(
              name: "Json",
              alias: None,
            ),
          ],
          unqualified_values: [],
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
    imports
    |> list.append(
      case needs_util_import(type_) {
        False -> []
        True -> {
          // import deriv/util
          [
            Import(
              module: "deriv/util",
              alias: None,
              unqualified_types: [],
              unqualified_values: [],
            )
          ]
        }
      }
    )
    |> list.append(
      case needs_list_import(type_) {
        False -> []
        True -> {
          // import gleam/list
          [
            Import(
              module: "gleam/list",
              alias: None,
              unqualified_types: [],
              unqualified_values: [],
            )
          ]
        }
      }
    )
  }
}

fn needs_util_import(type_: CustomType) -> Bool {
  // is_multi_variant(type_) || uses_uuid(type_)
  uses_uuid(type_)
}

fn needs_list_import(type_: CustomType) -> Bool {
  uses_list(type_)
}

// fn is_multi_variant(type_: CustomType) -> Bool {
//   list.length(type_.variants) > 1
// }

fn uses_uuid(type_: CustomType) -> Bool {
  type_.variants
  |> list.any(fn(var) {
    list.any(var.fields, fn(field) {
      let field = variant_field(field)
      let type_ = jtype(field.type_)
      type_.name == "Uuid"
    })
  })
}

fn uses_list(type_: CustomType) -> Bool {
  type_.variants
  |> list.any(fn(var) {
    list.any(var.fields, fn(field) {
      let field = variant_field(field)
      let type_ = jtype(field.type_)
      type_.name |> string.starts_with("List")
    })
  })
}

type JsonType {
  JsonType(
    type_: CustomType,
    variant: Variant,
    fields: List(VariantField),
  )
}

// type JsonField {
//   JsonField(
//     name: String,
//     type_: VariantField,
//   )
// }

// fn gen_json_decoders(type_: CustomType, field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> List(deriv.Function) {
//   to_json_types(type_)
//   |> list.map(decoder_func_src(_, field_opts, file))
// }

fn encode_func_for_basic_type(type_: String) -> Result(String, Nil) {
  case type_ {
    "Int" -> Ok("json.int")
    "Float" -> Ok("json.float")
    "String" -> Ok("json.string")
    "Bool" -> Ok("json.bool")
    "Uuid" -> Ok("util.encode_uuid")
    _ -> Error(Nil)
  }
}

// fn gen_json_encoders(type_: CustomType, all_field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> List(deriv.Function) {
//   type_
//   |> to_json_types
//   |> list.map(fn(jt) {
//     let encode_lines =
//       jt.fields
//       |> list.map(fn(f) {
//         let field_opts =
//           todo
//           // all_field_opts
//           // |> dict.get(f.name)
//           // |> result.unwrap([])

//         let json_field_name = json_field_name(f, field_opts)

//         let encode_func =
//           // f.type_
//           todo
//           |> encode_func_for_basic_type
//           |> result.map(fn(func) { func <> "(value.GLEAM)" })
//           |> result.map_error(fn(_) {
//             // case f.type_ {
//             case todo {
//               "Option "<> type_ ->
//                 case encode_func_for_basic_type(type_) {
//                   Ok(func) ->
//                     "json.nullable(value.GLEAM, FUNC)"
//                     |> string.replace(each: "FUNC", with: func)

//                   Error(Nil) -> {
//                     panic as "not fully implemented (call FUNC with VALUE)"

//                     // let func = util.snake_case(type_)

//                     // "json.nullable(value.GLEAM, FUNC)"
//                     // |> string.replace(each: "FUNC", with: func)
//                   }
//                 }

//               "List "<> type_ ->
//                 case encode_func_for_basic_type(type_) {
//                   Ok(func) ->
//                     "json.preprocessed_array(list.map(value.GLEAM, FUNC))"
//                     |> string.replace(each: "FUNC", with: func)

//                   Error(Nil) -> {
//                     panic as "not fully implemented (call FUNC with VALUE)"

//                     // let func = util.snake_case(type_)

//                     // "json.preprocessed_array(list.map(value.GLEAM, FUNC))"
//                     // |> string.replace(each: "FUNC", with: func)
//                   }
//                 }

//               type_ -> "encode_" <> util.snake_case(type_) <> "(value.GLEAM)"
//             }
//           })
//           |> result.unwrap_both

//         "#(\"JSON\", FUNC),"
//         |> string.replace(each: "JSON", with: json_field_name)
//         |> string.replace(each: "FUNC", with: encode_func)
//         // |> string.replace(each: "GLEAM", with: f.name)
//         |> string.replace(each: "GLEAM", with: todo)
//       })

//     let func_name = "encode_" <> util.snake_case(type_.name)

//     let qualified_type = qualified_type(type_, file)

//     [
//       "pub fn " <> func_name <> "(value: " <> qualified_type <> ") -> Json {",
//       "  json.object([",
//            encode_lines |> list.map(util.indent(_, level: 2)) |> string.join("\n"),
//       "  ])",
//       "}",
//     ]
//     |> string.join("\n")
//     |> deriv.Function(name: func_name, src: _)
//   })
// }

// fn to_json_field(field: VariantField) -> Result(JsonField, VariantField) {
//   case field {
//     LabelledVariantField(NamedType("Option", _, [NamedType(type_, _, [])]), name) ->
//       Ok(JsonField(name:, type_: "Option " <> type_))

//     LabelledVariantField(NamedType("List", _, [NamedType(type_, _, [])]), name) ->
//       Ok(JsonField(name:, type_: "List " <> type_))

//     LabelledVariantField(NamedType("Uuid", _, []), name) ->
//       Ok(JsonField(name:, type_: "Uuid"))

//     LabelledVariantField(NamedType("String", _, []), name) ->
//       Ok(JsonField(name:, type_: "String"))

//     LabelledVariantField(NamedType("Int", _, []), name) ->
//       Ok(JsonField(name:, type_: "Int"))

//     LabelledVariantField(NamedType("Float", _, []), name) ->
//       Ok(JsonField(name:, type_: "Float"))

//     LabelledVariantField(NamedType("Bool", _, []), name) ->
//       Ok(JsonField(name:, type_: "Bool"))

//     LabelledVariantField(NamedType(type_, _, []), name) ->
//       Ok(JsonField(name:, type_: type_))

//     x -> {
//       io.debug(x)
//       Error(field)
//     }
//   }
// }

fn to_json_types(type_: CustomType) -> List(JsonType) {
  type_.variants
  |> list.map(to_json_type(type_, _))
}

fn to_json_type(type_: CustomType, variant: Variant) -> JsonType {
  // let xs = list.map(variant.fields, to_json_field)
  // case result.all(variant.fields) {
  case todo {
    Ok(fields) ->
      JsonType(type_:, variant:, fields:)

    Error(field) -> {
      io.debug(field)
      panic as "Could not process the above field"
    }
  }
}

fn decoder_func_name(type_: CustomType) -> String {
  "decoder_" <> type_variant_snake_case(type_)
}

fn type_variant_snake_case(type_: CustomType) -> String {
  case type_.variants {
    [] ->
      panic as "`CustomType` has no `variants`"

    [invariant] ->
      case invariant.name == type_.name {
        True -> util.snake_case(invariant.name)
        False -> util.snake_case(type_.name) <> "_" <> util.snake_case(invariant.name)
      }

    _multi_variant ->
      // util.snake_case(type_.name) <> "_" <> util.snake_case(var.name)
      todo as "each variant"
  }
}

fn decoder_func_src(type_: CustomType, all_field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> List(deriv.Function) {
  let func_name = decoder_func_name(type_)

  case type_.variants {
    [] -> panic as "`CustomType` has no `variants`"

    [invariant] -> {
      let src =
        decoder_func_src_(
          True,
          func_name,
          type_,
          invariant,
          all_field_opts,
          file,
        )

      [ deriv.Function(name: func_name, src:) ]
    }

    multi_variants -> {
      multi_variants
      |> list.map(fn(variant) {
        let src =
          decoder_func_src_(
            False,
            func_name,
            type_,
            variant,
            all_field_opts,
            file,
          )

        deriv.Function(name: func_name, src:)
      })
    }
  }
}

fn decoder_func_for_multi_variant_type(
  type_: CustomType,
  _all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
) -> deriv.Function {
  let type_snake_case = util.snake_case(type_.name)

  let qualified_type = qualified_type(type_, file)

  let func_name = "decoder_" <> type_snake_case
  let func_def = "pub fn " <> func_name <> "() -> Decoder(" <> qualified_type <> ")"

  let variant_decoder_funcs_list_lines =
    type_.variants
    |> list.map(fn(v) {
      let f = util.snake_case(type_.name) <> "_" <> util.snake_case(v.name)

      { "decoder_" <> f <> "()," }
      |> util.indent(level: 2)
    })
    |> string.join("\n")

  let src =
    [
      func_def <> " {",
      "decode.one_of([" |> util.indent(level: 1),
      variant_decoder_funcs_list_lines,
      "])" |> util.indent(level: 1),
      "}",
    ]
    |> string.join("\n")

  deriv.Function(name: func_name, src:)
}

type DecodeLines {
  DecodeLines(
    params: List(String),
    fields: List(String),
  )
}

fn decode_lines_for_fields(
  variant: Variant,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
) -> DecodeLines {
  variant.fields
  |> list.map(fn(field) {
    let vfield = variant_field(field)

    let field_opts =
      all_field_opts
      |> dict.get(vfield.name)
      |> result.unwrap([])

    let json_field_name = json_field_name(field, field_opts)

    #(
      decode_line_param(field),
      decode_line_field(field, json_field_name),
    )
  })
  |> list.unzip
  |> fn(xs) {
    let #(params, fields) = xs
    DecodeLines(params:, fields:)
  }
}

fn decode_line_param(field: VariantField) -> String {
  let field = variant_field(field)

  "use NAME <- decode.parameter"
  |> string.replace(each: "NAME", with: field.name)
}
fn decode_line_field(field: VariantField, json_name: String) -> String {
  let field = variant_field(field)
  let type_ = jtype(field.type_)
  let decoder = decoder_line(type_.name)

  "|> decode.field(\"NAME\", DECODER)"
  |> string.replace(each: "NAME", with: json_name)
  |> string.replace(each: "DECODER", with: decoder)
}

fn qualified_type(type_: CustomType, file: File) -> String {
  case file.idx {
    Some(idx) -> "m" <> {int.to_string(idx)} <> "." <> type_.name
    None -> type_.name
  }
}

fn decoder_func_src_(
  public: Bool,
  func_name: String,
  type_: CustomType,
  variant: Variant,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
  file: File,
) -> String {
  let decode_lines = decode_lines_for_fields(variant, all_field_opts)

  let type_const_line = decode_success_line(variant, file)

  let decode_block_body =
    [
      decode_lines.params |> list.map(util.indent(_, level: 2)) |> string.join("\n"),
      type_const_line |> util.indent(level: 2),
    ]
    |> string.join("\n\n")

  let decode_field_pipeline =
    decode_lines.fields |> list.map(util.indent(_, level: 1)) |> string.join("\n")

  let decode_body =
    [
      "  decode.into({" ,
      decode_block_body,
      "  })" ,
      decode_field_pipeline,
    ]
    |> string.join("\n")

  let pub_ =
    case public {
      True -> "pub "
      False -> ""
    }

  let qualified_type = qualified_type(type_, file)

  let func_def = pub_ <> "fn " <> func_name <> "() -> Decoder(" <> qualified_type <> ")"

  [
    func_def <> " {",
    decode_body,
    "}",
  ]
  |> string.join("\n")
}


type VarField {
  VarField(
    name: String,
    type_: Type,
  )
}
fn variant_field(field: VariantField) -> VarField {
  case field {
    LabelledVariantField(label:, item:) -> VarField(name: label, type_: item)
    UnlabelledVariantField(..) -> panic as "Not implemented: `glance.UnlabelledVariantField`"
  }
}
type JType {
  JType(
    name: String,
    module: Option(String),
    parameters: List(JType),
  )
}
fn jtype(type_: Type) -> JType {
  case type_ {
    NamedType(name:, module:, parameters: ps) ->
      JType(name:, module:, parameters: list.map(ps, jtype))

    _ -> {
      io.debug(type_)
      panic as "Not implemented for `glance.Type` constructor printed above"
    }
  }
}
fn decode_success_line(variant: Variant, file: File) -> String {
  let field_name_args_str =
    variant.fields
    |> list.map(fn(f) {
      let f = variant_field(f)
      f.name <> ":"
    })
    |> string.join(", ")

  case file.idx {
    Some(idx) ->
      "mIDX.VAR(FIELDS)"
      |> string.replace(each: "IDX", with: int.to_string(idx))
      |> string.replace(each: "VAR", with: variant.name)
      |> string.replace(each: "FIELDS", with:field_name_args_str)

    None ->
      "VAR(FIELDS)"
      |> string.replace(each: "VAR", with: variant.name)
      |> string.replace(each: "FIELDS", with:field_name_args_str)
  }
}

fn json_field_name(field: VariantField, field_opts: List(DerivFieldOpt)) -> String {
  field_opts
  |> list.find(fn(f) { f.deriv == "json" && f.key == "named" })
  |> fn(x) {
    case x, variant_field(field) {
      Ok(field_opt), _ -> field_opt.val
      Error(_), field -> field.name
    }
  }
}

fn decoder_func_for_basic_type(type_: String) -> Result(String, Nil) {
  case type_ {
    "Int" -> Ok("decode.int")
    "Float" -> Ok("decode.float")
    "String" -> Ok("decode.string")
    "Bool" -> Ok("decode.bool")
    "Uuid" -> Ok("util.decoder_uuid()")
    _ -> Error(Nil)
  }
}

fn decoder_line(field_type: String) -> String {
  case field_type {
    "Option " <> type_ ->
      case decoder_func_for_basic_type(type_) { // TODO not needed now?
        Ok(func) ->
          "decode.optional(FUNC)"
          |> string.replace(each: "FUNC", with: func)

        Error(_) -> {
          let func = decoder_line(type_)

          "decode.optional(FUNC)"
          |> string.replace(each: "FUNC", with: func)
        }
      }
    "List " <> type_ ->
      case decoder_func_for_basic_type(type_) { // TODO not needed now?
        Ok(func) ->
          "decode.list(FUNC)"
          |> string.replace(each: "FUNC", with: func)

        Error(_) -> {
          let func = decoder_line(type_)

          "decode.list(FUNC)"
          |> string.replace(each: "FUNC", with: func)
        }
      }
    "Int" -> "decode.int"
    "Float" -> "decode.float"
    "String" -> "decode.string"
    "Bool" -> "decode.bool"
    "Uuid" -> "util.decoder_uuid()"
    type_ -> "decoder_" <> util.snake_case(type_) <> "()"
  }
}

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }


pub fn encode_func() -> Definition(Function) {
  let input_type = "Foo"

  let name = "encode_foo"

  let parameters = [FunctionParameter(None, Named("value"), Some(NamedType(input_type, None, [])))]
  let return = Some(NamedType("Json", None, []))

  let body = [
    Expression(
      Call(
        function: FieldAccess(Variable("json"), "object"),
        arguments: [
          UnlabelledField(
            List(
              [
                Tuple([
                  String("uuid"),
                  Call(
                    function: FieldAccess(container: Variable("util"), label: "encode_uuid"),
                    arguments: [UnlabelledField(FieldAccess(Variable("value"), "uuid"))],
                  )
                ]),

                Tuple([
                  String("int_id"),
                  Call(
                    function: FieldAccess(Variable("json"), "int"),
                    arguments: [UnlabelledField(FieldAccess(Variable("value"), "id"))]
                  ),
                ]),

                Tuple([
                  String("name"),
                  Call(
                    function: FieldAccess(Variable("json"), "string"),
                    arguments: [UnlabelledField(FieldAccess(Variable("value"), "name"))]
                  )
                ]),

                Tuple([
                  String("active"),
                  Call(
                    function: FieldAccess(Variable("json"), "bool"),
                    arguments: [UnlabelledField(FieldAccess(Variable("value"), "active"))]
                   )
                ]),

                Tuple([
                  String("ratio"),
                  Call(
                    function: FieldAccess(Variable("json"), "float"),
                    arguments: [UnlabelledField(FieldAccess(Variable("value"), "ratio"))]
                  )
                ]),

                Tuple([
                  String("words"),
                  Call(
                    function: FieldAccess(Variable("json"), "preprocessed_array"),
                    arguments: [
                    UnlabelledField(Call(FieldAccess(Variable("list"), "map"),
                      [
                        UnlabelledField(FieldAccess(Variable("value"), "words")),
                        UnlabelledField(FieldAccess(Variable("json"), "string")),
                      ]))
                    ]
                  )
                ])
              ], None
            )
          )
        ]
      )
    )
  ]

  Definition([],
    Function(
      location: dummy_location(),
      publicity: Public,
      name:,
      parameters:,
      return:,
      body:,
    )
  )
}

fn dummy_location() -> Span {
  Span(-1, -1)
}

pub fn decoder_func(
  type_ type_: String,
  variant variant: Option(String),
  constr constr: String,
) -> Definition(Function) {
  let snake_case = util.snake_case(type_ <> option.unwrap(variant, ""))

  let name = "decoder_" <> snake_case

  let parameters: List(glance.FunctionParameter) = []
  let return: Option(Type) = Some(NamedType("Decoder", None, [NamedType(type_, None, [])]))

  let pipe_exprs: List(#(String, Option(String), Expression)) =
    [
      #("uuid", None, Call(FieldAccess(Variable("util"), "decoder_uuid"), [])),
      #("id", Some("int_id"), FieldAccess(Variable("decode"), "int")),
      #("name", None, FieldAccess(Variable("decode"), "string")),
      #("active", None, FieldAccess(Variable("decode"), "bool")),
      #("ratio", None, FieldAccess(Variable("decode"), "float")),
      #("words", None, Call(FieldAccess(Variable("decode"), "list"), [UnlabelledField(FieldAccess(Variable("decode"), "string"))])),

      // TODO optional/nullable
    ]

  let fields =
    pipe_exprs
    |> list.map(fn(x) {
      let #(field, _, _) = x
      field
    })

  let use_exprs =
    fields
    |> list.map(fn(field) {
      Use([PatternVariable(field)], FieldAccess(Variable("decode"), "parameter"))
    })

  let constr_args = fields |> list.map(ShorthandField)

  let decode_into_call: Expression =
    Call(FieldAccess(Variable("decode"), "into"), [UnlabelledField(
      Block(use_exprs |> list.append([
        Expression(Call(
          function: Variable(constr),
          arguments: constr_args,
        ))
      ]))
    )])

  let body: List(Statement) =
    list.fold(pipe_exprs, decode_into_call, fn(acc, x) {
      let #(field, json_field, expr) = x

      let json_field = json_field |> option.unwrap(field)

      let call = Call(FieldAccess(Variable("decode"), "field"), [UnlabelledField(String(json_field)), UnlabelledField(expr)])

      BinaryOperator(Pipe, acc, call)
    })
    |> fn(expr) { [Expression(expr)] }

  Definition([],
    Function(
      location: dummy_location(),
      publicity: Public,
      name:,
      parameters:,
      return:,
      body:,
    )
  )
}

pub fn foo() {
  Module([], [], [], [], [
  // funcs
  ])
}
