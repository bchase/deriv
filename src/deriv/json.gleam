import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, UnlabelledVariantField, NamedType, type Import, Import, UnqualifiedImport, Definition, CustomType, Public, Variant, Function, FieldAccess, Variable, Span, Expression, Call, UnlabelledField, Block, Use, BinaryOperator, Pipe, PatternVariable, ShorthandField, String, FunctionParameter, Tuple, Named, List, type Definition, type Function, type Span, type Expression, type Statement, type Type, Clause, Case, PatternAssignment, PatternConstructor}
import deriv/types.{type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen}
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

  let funcs =
    opts
    |> list.map(dict.get(gen_funcs_for_opts, _))
    |> result.values
    |> list.flat_map(fn(f) { f(type_, field_opts, file)})

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


fn gen_json_decoders(
  type_: CustomType,
  field_opts: Dict(String, List(DerivFieldOpt)),
  _file: File,
) -> List(Definition(Function)) {
  decoder_type_func(type_, field_opts)
}

fn gen_json_encoders(type_: CustomType, all_field_opts: Dict(String, List(DerivFieldOpt)), _file: File) -> List(Definition(Function)) {
  // TODO qualify imports using `file` (`idx`)
  encode_type_func(type_, all_field_opts)
  |> fn(func) { [ func ] }
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
      _ -> Variable("encode_" <> util.snake_case(type_name))
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

fn dummy_location() -> Span {
  Span(-1, -1)
}

fn decoder_type_func(
  type_: CustomType,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
) -> List(Definition(Function)) {
  let variant_funcs =
    type_.variants
    |> list.map(decoder_type_variant_func(type_, _, all_field_opts))

  let decoder_call_exprs =
    variant_funcs
    |> list.map(fn(func) {
      Call(Variable(util.func_name(func)), [])
    })

  let body =
    [
      Expression(Call(FieldAccess(Variable("decode"), "one_of"), [
        UnlabelledField(List(decoder_call_exprs, None))
      ]))
    ]

  let type_func =
    Definition([], Function(
      location: dummy_location(),
      publicity: Public,
      name: "decoder_" <> util.snake_case(type_.name),
      parameters: [],
      return: Some(NamedType("Decoder", None, [NamedType(type_.name, None, [])])),
      body: body,
    ))

  list.append([type_func], variant_funcs)
}

fn decoder_type_variant_func_name(type_: CustomType, variant: Variant) -> String {
  "decoder_" <> util.snake_case(type_.name) <> "_" <> util.snake_case(variant.name)
}

fn decode_field_expr(
  field: VariantField,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
) -> #(String, Option(String), Expression) {
  let field = variant_field(field)

  let t = jtype(field.type_)

  let expr =
    case t.parameters {
      [] -> unparameterized_type_decode_expr(t.name)
      [JType(parameters: [], ..) as param] ->
        case t.name {
          "List" -> {
            let inner =
              param.name
              |> unparameterized_type_decode_expr
              |> UnlabelledField

            Call(FieldAccess(Variable("decode"), "list"), [inner])
          }

          "Option" -> {
            let inner =
              param.name
              |> unparameterized_type_decode_expr
              |> UnlabelledField

            Call(FieldAccess(Variable("decode"), "optional"), [inner])
          }

          _ -> {
            io.debug(field)
            panic as "unimplemented"
          }
        }
      _ -> {
        io.debug(field)
        panic as "unimplemented"
      }
    }

  let json_field_name =
    all_field_opts
    |> dict.get(field.name)
    |> result.map(json_field_name(field, _))
    |> option.from_result

  #(field.name, json_field_name, expr)
}

fn decoder_type_variant_func(
  type_: CustomType,
  variant: Variant,
  all_field_opts: Dict(String, List(DerivFieldOpt)),
) -> Definition(Function) {
  let name = decoder_type_variant_func_name(type_, variant)

  let parameters: List(glance.FunctionParameter) = []
  let return: Option(Type) = Some(NamedType("Decoder", None, [NamedType(type_.name, None, [])]))

  let pipe_exprs: List(#(String, Option(String), Expression)) =
    variant.fields
    |> list.map(decode_field_expr(_, all_field_opts))

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
          function: Variable(variant.name),
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

fn unparameterized_type_decode_expr(
  type_name: String,
) -> Expression {
  case type_name {
    "Int" -> FieldAccess(Variable("decode"), "int")
    "Float" -> FieldAccess(Variable("decode"), "float")
    "String" -> FieldAccess(Variable("decode"), "string")
    "Bool" -> FieldAccess(Variable("decode"), "bool")
    "Uuid" -> Call(FieldAccess(Variable("util"), "decoder_uuid"), [])
    _ -> Call(Variable("decoder_" <> util.snake_case(type_name)), [])
  }
}
