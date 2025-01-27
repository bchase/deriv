import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, UnlabelledVariantField, NamedType, type Import, Import, UnqualifiedImport, Module, Definition, CustomType, Public, Variant, Function, FieldAccess, Variable, Span, Expression, Call, UnlabelledField, Block, Use, BinaryOperator, Pipe, PatternVariable, ShorthandField, String, FunctionParameter, Tuple, Named, List, type Definition, type Function, type Span, type Expression, type Statement, type Type, Private, Clause, Case, PatternAssignment, PatternConstructor}
import glance_printer
import shellout
import deriv/types.{type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen} as deriv
import deriv/util

import gleam/json.{type Json}

pub fn gen(type_: CustomType, deriv: Derivation, field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> Gen {
  let opts = deriv.opts

  let gen_funcs_for_opts =
    [
      // #("decode", gen_json_decoders),
      #("encode", gen_json_encoders),
    ]
    |> dict.from_list

  let imports =
    gen_imports(opts, type_)

  let other_funcs =
    opts
    |> list.map(dict.get(gen_funcs_for_opts, _))
    |> result.values
    |> list.flat_map(fn(f) { f(type_, field_opts, file)})

  // let multi_variant_type_decoders_funcs =
  //   case type_.variants {
  //     [] ->
  //       panic as "`CustomType` has no `variants`"

  //     [_invariant] ->
  //       []

  //     _multi_variants ->
  //       [decoder_func_for_multi_variant_type(type_, field_opts, file)]
  //   }

  let funcs =
    [
      // multi_variant_type_decoders_funcs,
      other_funcs,
    ]
    |> list.flatten

  let src =
    funcs
    |> list.map(util.func_str)
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


fn gen_json_decoders(type_: CustomType, field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> List(deriv.Function) {
  decoder_func_src(type_, field_opts, file)
}

fn encode_func_str(type_: JType) -> String {
  encode_func_str_(type_, call_with_value: True)
}
fn encode_func_str_(type_: JType, call_with_value call_with_value: Bool) -> String {
  let src =
    case type_.name, type_.parameters {
      "Int", [] -> "json.int"
      "Float", [] -> "json.float"
      "String", [] -> "json.string"
      "Bool", [] -> "json.bool"
      "Uuid", [] -> "util.encode_uuid"
      "Option", [param] -> {
        let func = encode_func_str_(JType(param.name, None, []), call_with_value: False)
        "json.nullable(value.GLEAM, FUNC)"
        |> string.replace(each: "FUNC", with: func)
      }
      "List", [param] -> {
        let func = encode_func_str_(JType(param.name, None, []), call_with_value: False)
        "json.preprocessed_array(list.map(value.GLEAM, FUNC))"
        |> string.replace(each: "FUNC", with: func)
      }
      type_name, [] ->
        "encode_" <> util.snake_case(type_name) <> "(value.GLEAM)"
      _, _ -> {
        io.debug(type_)
        panic as "Not yet implemented for type printed above"
      }
    }

  case call_with_value && !string.contains(src, "GLEAM") {
    True -> src <> "(value.GLEAM)"
    False -> src
  }
}

fn gen_json_encoders(type_: CustomType, all_field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> List(Definition(Function)) {
  // TODO qualify imports using `file` (`idx`)
  encode_type_func(type_, all_field_opts)
  |> fn(func) { [ func ] }
}

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

fn decoder_func_name(type_: CustomType, variant: Variant) -> String {
  "decoder_" <> type_variant_snake_case(type_, variant)
}

fn type_variant_snake_case(type_: CustomType, variant: Variant) -> String {
  case type_.variants {
    [] ->
      panic as "`CustomType` has no `variants`"

    [_invariant] ->
      util.snake_case(variant.name)

    _multi_variant ->
      util.snake_case(type_.name) <> "_" <> util.snake_case(variant.name)
  }
}

fn decoder_func_src(type_: CustomType, all_field_opts: Dict(String, List(DerivFieldOpt)), file: File) -> List(deriv.Function) {
  case type_.variants {
    [] -> panic as "`CustomType` has no `variants`"

    [invariant] -> {
      let func_name = decoder_func_name(type_, invariant)

      let src =
        decoder_func_src_(
          True,
          func_name,
          type_,
          invariant,
          all_field_opts,
          file,
        )

      // [ deriv.Function(name: func_name, src:) ]
      todo
    }

    multi_variants -> {
      multi_variants
      |> list.map(fn(variant) {
        let func_name = decoder_func_name(type_, variant)

        let src =
          decoder_func_src_(
            False,
            func_name,
            type_,
            variant,
            all_field_opts,
            file,
          )

        // deriv.Function(name: func_name, src:)
        todo
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

  // deriv.Function(name: func_name, src:)

  todo
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
    let field = variant_field(field)

    let field_opts =
      all_field_opts
      |> dict.get(field.name)
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

fn decode_line_param(field: VarField) -> String {
  "use NAME <- decode.parameter"
  |> string.replace(each: "NAME", with: field.name)
}
fn decode_line_field(field: VarField, json_name: String) -> String {
  let type_ = jtype(field.type_)
  let decoder = decoder_line(type_)

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

fn json_field_name(field: VarField, field_opts: List(DerivFieldOpt)) -> String {
  field_opts
  |> list.find(fn(f) { f.deriv == "json" && f.key == "named" })
  |> fn(x) {
    case x, field {
      Ok(field_opt), _ -> field_opt.val
      Error(_), field -> field.name
    }
  }
}

fn decoder_line(field_type: JType) -> String {
  case field_type.name, field_type.parameters {
    "Option", [param] -> {
      let func = decoder_line(param)

      "decode.optional(FUNC)"
      |> string.replace(each: "FUNC", with: func)
    }
    "List", [param] -> {
      let func = decoder_line(param)

      "decode.list(FUNC)"
      |> string.replace(each: "FUNC", with: func)
    }
    "Int", [] -> "decode.int"
    "Float", [] -> "decode.float"
    "String", [] -> "decode.string"
    "Bool", [] -> "decode.bool"
    "Uuid", [] -> "util.decoder_uuid()"
    _type, [] -> "decoder_" <> util.snake_case(field_type.name) <> "()"
    _, _ -> {
      io.debug(field_type)
      panic as "Not yet implemented for type printed above"
    }
  }
}

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

fn unparameterized_type_encode_expr(type_name: String, wrap wrap: Option(fn(Expression) -> Expression)) -> Expression {
  let expr =
    case type_name {
      "Int" -> FieldAccess(Variable("json"), "int")
      "Float" -> FieldAccess(Variable("json"), "float")
      "String" -> FieldAccess(Variable("json"), "string")
      "Bool" -> FieldAccess(Variable("json"), "bool")
      "Uuid" -> FieldAccess(Variable("util"), "encode_uuid")
      _ -> FieldAccess(Variable("util"), "encode_" <> util.snake_case(type_name))
    }

  case wrap {
    None -> expr
    Some(f) -> f(expr)
  }
}

fn encode_field(field: VarField) -> Expression {
  let ftype = jtype(field.type_)

  case ftype.parameters {
    [] ->
      unparameterized_type_encode_expr(ftype.name, Some(fn(func_expr) {
        Call(
          function: func_expr,
          arguments: [
            UnlabelledField(FieldAccess(Variable("value"), field.name)),
          ]
        )
      }))

    [JType(name: param_type_name, module: None, parameters: [])] ->
      case ftype.name {
        "Option" -> {
          let param_type_encoder = unparameterized_type_encode_expr(param_type_name, None)

          Call(
            function: FieldAccess(Variable("json"), "nullable"),
            arguments: [
              UnlabelledField(FieldAccess(Variable("value"), field.name)),
              UnlabelledField(param_type_encoder),
            ]
          )
        }
        "List" -> {
          let param_type_encoder = unparameterized_type_encode_expr(param_type_name, None)

          Call(
            function: FieldAccess(Variable("json"), "preprocessed_array"),
            arguments: [
              UnlabelledField(Call(FieldAccess(Variable("list"), "map"),
                [
                  UnlabelledField(FieldAccess(Variable("value"), field.name)),
                  UnlabelledField(param_type_encoder),
                ])
              ),
            ]
          )
        }
        _ -> {
          io.debug(ftype)
          panic as "Not yet implemented for type printed above"
        }
      }

    _ -> {
      io.debug(ftype)
      panic as "Not yet implemented for type printed above"
    }
  }
}

fn encode_variant_json_object_expr(
  variant: Variant,
  all_field_opts all_field_opts: Dict(String, List(DerivFieldOpt)),
) -> Expression {
  let encode_lines =
    variant.fields
    |> list.map(fn(field) {
      let field = variant_field(field)

      let field_opts =
        all_field_opts
        |> dict.get(field.name)
        |> result.unwrap([])

      let json_field = json_field_name(field, field_opts)

      let encode_expr = encode_field(field)

      Tuple([String(json_field), encode_expr])
    })

  Call(
    function: FieldAccess(Variable("json"), "object"),
    arguments: [ UnlabelledField(List(encode_lines, None)) ],
  )
}

fn encode_type_func(
  type_: CustomType,
  all_field_opts all_field_opts: Dict(String, List(DerivFieldOpt)),
) -> Definition(Function) {
  let name = "encode_" <> util.snake_case(type_.name)

  let parameters = [FunctionParameter(None, Named("value"), Some(NamedType(type_.name, None, [])))]
  let return = Some(NamedType("Json", None, []))

  let encode_variant_clause_exprs =
    type_.variants
    |> list.map(fn(variant) {
      let encode_json_object_expr = encode_variant_json_object_expr(variant, all_field_opts)

      Clause([[PatternAssignment(PatternConstructor(None, variant.name, [], True), "value")]], None,
        encode_json_object_expr
      )
    })

  let body =
  // [ Expression(encode_variant_json_object_expr(variant, all_field_opts)) ]
    Expression(
      Case(
        [Variable("value")],
        encode_variant_clause_exprs,
      )
    )
    |> fn(expr) { [ expr ] }

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


pub type T {
  X(foo: String)
  Y(bar: Int)
}

fn encode_t(value: T) -> Json {
  case value {
    X(..) as value ->
      json.object([
        #("foo", json.string(value.foo)),
      ])

    Y(..) as value ->
      json.object([
        #("bar", json.int(value.bar)),
      ])
  }
}

fn foo() {
  Definition([],
    Function(
      location: Span(967, 1203),
      name: "encode_t",
      publicity: Private,
      parameters: [FunctionParameter(None, Named("value"), Some(NamedType("T", None, [])))],
      return: Some(NamedType("Json", None, [])),
      body: [
        Expression(
          Case(
            [Variable("value")],
            [
              Clause([[PatternAssignment(PatternConstructor(None, "X", [], True), "value")]], None,
                Call(FieldAccess(Variable("json"), "object"), [UnlabelledField(List([Tuple([String("foo"), Call(FieldAccess(Variable("json"), "string"), [UnlabelledField(FieldAccess(Variable("value"), "foo"))])])], None))])
              ),
              Clause([[PatternAssignment(PatternConstructor(None, "Y", [], True), "value")]], None,
                Call(FieldAccess(Variable("json"), "object"), [UnlabelledField(List([Tuple([String("bar"), Call(FieldAccess(Variable("json"), "int"), [UnlabelledField(FieldAccess(Variable("value"), "bar"))])])], None))])
              ),
            ]
          )
        )
      ],
    )
  )
}
// fn encode_t_(value: T) -> Json {
//   case value {
//     X(foo:) ->
//       json.object([
//         #("foo", json.string(foo)),
//       ])

//     Y(bar:) ->
//       json.object([
//         #("bar", json.int(bar)),
//       ])
//   }
// }

fn multi_variant_type_encoder(type_: CustomType) -> Definition(Function) {

}

pub fn hardcode_encode_func() -> Definition(Function) {
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

fn decoder_variant_func(
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
