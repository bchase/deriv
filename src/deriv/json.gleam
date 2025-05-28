import gleam/option.{type Option, Some, None}
import gleam/dict
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, UnlabelledVariantField, NamedType, VariableType, type Import, Import, UnqualifiedImport, Definition, CustomType, Public, Variant, Function, type FunctionParameter, type Field, FieldAccess, Variable, Span, Expression, Call, UnlabelledField, Block, Use, BinaryOperator, Pipe, PatternVariable, PatternDiscard, ShorthandField, String, FunctionParameter, Tuple, Named, List, type Definition, type Function, type Span, type Expression, type Statement, type Type, Clause, Case, PatternAssignment, PatternConstructor, Fn, FnParameter, FnCapture, FunctionType, type TypeAlias}
import deriv/types.{type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen, type DerivFieldOpts, type ModuleReader, DerivFieldOpt} as deriv
import deriv/util.{type BirlTimeKind, BirlTimeISO8601, BirlTimeUnixMicro, BirlTimeUnixMilli, BirlTimeUnix, BirlTimeHTTP, BirlTimeNaive}

// TODO refactor
//   - `unparameterized_type_decode_expr` should be able to be replaced with `type_decode_expr`
//   - `unparameterized_type_encode_expr` should be able to be replaced with `type_encode_expr`

const deriv_variant_json_key = "_var"

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  _module_reader: ModuleReader,
) -> Gen {
  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/json` "

    deriv.Type(type_:) -> {
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
  }
}

fn gen_imports(opts: List(String), type_: CustomType) -> List(Import) {
  let json_imports =
    [
      #("decode", [
        // import decode.{type Decoder}
        Import(
          module: "gleam/dynamic/decode",
          alias: None,
          unqualified_types: [
            UnqualifiedImport(
              name: "Decoder",
              alias: None,
            ),
          ],
          unqualified_values: [],
        ),
      ] |> list.append({
        case util.are_any_fields_options(type_) {
          True -> [util.none_constr_import()]
          False -> []
        }
      })),
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
  is_multi_variant(type_) || uses_uuid(type_) || uses_birl_time(type_)
}

fn needs_list_import(type_: CustomType) -> Bool {
  uses_list(type_)
}

fn is_multi_variant(type_: CustomType) -> Bool {
  list.length(type_.variants) > 1
}

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

fn uses_birl_time(type_: CustomType) -> Bool {
  type_.variants
  |> list.any(fn(var) {
    list.any(var.fields, fn(field) {
      let field = variant_field(field)
      let type_ = jtype(field.type_)
      type_.name == "Time"
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
  field_opts: DerivFieldOpts,
  _file: File,
) -> List(Definition(Function)) {
  decoder_type_func(type_, field_opts)
}

fn gen_json_encoders(
  type_: CustomType,
  all_field_opts: DerivFieldOpts,
  _file: File,
) -> List(Definition(Function)) {
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

    VariableType(name:) ->
      JType(name:, module:None, parameters: [])

    _ -> {
      io.debug(type_)
      panic as "Not implemented for `glance.Type` constructor printed above"
    }
  }
}

fn json_field_name(field: VarField, field_opts: List(DerivFieldOpt)) -> String {
  field_opts
  |> list.find_map(fn(opt) {
    case opt.strs {
      ["json", "named", val] -> Ok(val)
      _ -> Error(Nil)
    }
  })
  |> fn(x) {
    case x, field {
      Ok(val), _ -> val
      Error(_), field -> field.name
    }
  }
}

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

fn type_encode_expr(
  type_: JType,
  field: VarField,
  birl_time_kind: BirlTimeKind,
  encode_param encode_param: Option(Field(Expression)),
  wrap wrap: Option(fn(Expression) -> Expression)
) -> Expression {
  let arg =
    case encode_param {
      Some(encode_param) ->
        encode_param

      None ->
        UnlabelledField(FieldAccess(Variable("value"), field.name))
    }

  let expr =
    case type_.name, type_.parameters {
      "Int", _ -> FieldAccess(Variable("json"), "int")
      "Float", _ -> FieldAccess(Variable("json"), "float")
      "String", _ -> FieldAccess(Variable("json"), "string")
      "Bool", _ -> FieldAccess(Variable("json"), "bool")
      "Uuid", _ -> FieldAccess(Variable("util"), "encode_uuid")
      "Time", _ -> birl_time_encode_expr(birl_time_kind)
      "Option", params -> {
        let params =
          [
            arg,
          ]
          |> list.append({
            params
            |> list.map(type_encode_expr(_, field, birl_time_kind, wrap: None, encode_param: Some(
              UnlabelledField(Variable("_"))
            )))
            |> list.map(UnlabelledField)
          })

        Call(FieldAccess(Variable("json"), "nullable"), params)
      }
      "List", params -> {
        let params =
          [
            arg,
          ]
          |> list.append({
            params
            |> list.map(type_encode_expr(_, field, birl_time_kind, wrap: None, encode_param: Some(
              UnlabelledField(Variable("_"))
            )))
            |> list.map(UnlabelledField)
          })

        Call(FieldAccess(Variable("json"), "array"), params)
      }
      // _, [] -> panic as {
      //   "`deriv/json.type_encode_expr` doesn't know what to do with type: "
      //     <> type_.name <> "\n" <> string.inspect(type_)
      // }
      _, params -> {
        let encoder_name = "encode_" <> util.snake_case(type_.name)
        case params {
          [] ->
            Variable(encoder_name)

          params -> {
            let params =
              [
                arg,
              ]
              |> list.append({
                params
                |> list.map(type_encode_expr(_, field, birl_time_kind, wrap: None, encode_param: None))
                |> list.map(UnlabelledField)
              })

            Call(Variable(encoder_name), params)
          }
        }
      }
    }

  case wrap {
    None -> expr
    Some(f) -> f(expr)
  }
}

fn unparameterized_type_encode_expr(
  type_name: String,
  birl_time_kind: BirlTimeKind,
  wrap wrap: Option(fn(Expression) -> Expression)
) -> Expression {
  let expr =
    case type_name {
      "Int" -> FieldAccess(Variable("json"), "int")
      "Float" -> FieldAccess(Variable("json"), "float")
      "String" -> FieldAccess(Variable("json"), "string")
      "Bool" -> FieldAccess(Variable("json"), "bool")
      "Uuid" -> FieldAccess(Variable("util"), "encode_uuid")
      "Time" -> birl_time_encode_expr(birl_time_kind)
      _ -> Variable("encode_" <> util.snake_case(type_name))
    }

  case wrap {
    None -> expr
    Some(f) -> f(expr)
  }
}

fn encode_field(
  type_: CustomType,
  variant: Variant,
  field: VarField,
  all_field_opts: DerivFieldOpts,
) -> Expression {
  let ftype = jtype(field.type_)

  let birl_time_kind = util.birl_time_kind(type_, variant, field.name, all_field_opts)

  case ftype, ftype.parameters {
    JType("Option", _, [JType("List", _, [JType(_, _, []) as param])]), _ -> {
      let param_type_encoder = unparameterized_type_encode_expr(param.name, birl_time_kind, None)

      Call(
        function: FieldAccess(Variable("json"), "nullable"),
        arguments: [
          UnlabelledField(FieldAccess(Variable("value"), field.name)),
          UnlabelledField(
            FnCapture(None, FieldAccess(Variable("json"), "array"), [], [
              UnlabelledField(param_type_encoder),
            ]),
          ),
        ],
      )
    }
    _, [] ->
      unparameterized_type_encode_expr(ftype.name, birl_time_kind, Some(fn(func_expr) {
        Call(
          function: func_expr,
          arguments: [
            UnlabelledField(FieldAccess(Variable("value"), field.name)),
          ]
        )
      }))

    _, [JType(name: param_type_name, module: None, parameters: [])] ->
      case ftype.name {
        "Option" -> {
          let param_type_encoder = unparameterized_type_encode_expr(param_type_name, birl_time_kind, None)

          Call(
            function: FieldAccess(Variable("json"), "nullable"),
            arguments: [
              UnlabelledField(FieldAccess(Variable("value"), field.name)),
              UnlabelledField(param_type_encoder),
            ]
          )
        }
        "List" -> {
          let param_type_encoder = unparameterized_type_encode_expr(param_type_name, birl_time_kind, None)

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

    _, [
      JType(name: key_param_type_name, module: None, parameters: []),
      JType(name: val_param_type_name, module: None, parameters: []),
    ] ->
      case ftype.name, key_param_type_name {
        "Dict", "String" -> {
          let val_param_type_encoder = unparameterized_type_encode_expr(val_param_type_name, birl_time_kind, None)

          Call(
            function: FieldAccess(Variable("json"), "dict"),
            arguments: [
              UnlabelledField(FieldAccess(Variable("value"), field.name)),
              UnlabelledField(Fn([FnParameter(Named("str"), None)], None, [Expression(Variable("str"))])),
              UnlabelledField(val_param_type_encoder),
            ]
          )
        }
        _, _ ->
          type_encode_expr(ftype, field, birl_time_kind, None, None)
      }

    _, _ ->
      type_encode_expr(ftype, field, birl_time_kind, None, None)
  }
}

fn encode_variant_json_object_expr(
  type_: CustomType,
  variant: Variant,
  all_field_opts all_field_opts: DerivFieldOpts,
) -> Expression {
  let encode_lines =
    variant.fields
    |> list.map(fn(field) {
      let field = variant_field(field)

      let json_field_name =
        all_field_opts
        |> util.get_field_opts(type_, variant, field.name)
        |> json_field_name(field, _)

      let encode_expr = encode_field(type_, variant, field, all_field_opts)

      case string.split(json_field_name, ".") {
        [] -> panic

        [_simple_field_name] ->
          Tuple([String(json_field_name), encode_expr])

        [top_level_field_name, ..rest] -> {
          rest
          |> list.reverse
          |> fn(fs) {
            case fs {
              [last_field_name, ..other_field_names] -> {
                // let terminal_expr = Tuple([String(last_field_name), encode_expr])
                let terminal_expr =
                  // Tuple([String(last_field_name), encode_expr])
                  Call(
                    function: FieldAccess(Variable("json"), "object"),
                    arguments: [
                      UnlabelledField(List(
                        [Tuple([String(last_field_name), encode_expr])],
                        None,
                      )),
                    ],
                  )

                let expr =
                  list.fold(other_field_names, terminal_expr, fn(acc_expr, json_field_name) {
                    Call(
                      function: FieldAccess(Variable("json"), "object"),
                      arguments: [
                        UnlabelledField(List(
                          [Tuple([String(json_field_name), acc_expr])],
                          None,
                        )),
                      ],
                    )
                  })

                Tuple([String(top_level_field_name), expr])
              }

              _ -> panic
            }
          }
        }
      }
    })

  let encode_lines =
    case is_multi_variant(type_) {
      True ->
        Tuple([String(deriv_variant_json_key), Call(
          function: FieldAccess(Variable("json"), "string"),
          arguments: [
            UnlabelledField(String(variant.name)),
          ],
        )
        ])
        |> list.wrap
        |> list.append(encode_lines)

      False ->
        encode_lines
    }

  Call(
    function: FieldAccess(Variable("json"), "object"),
    arguments: [ UnlabelledField(List(encode_lines, None)) ],
  )
}

// Ok(Module([Definition([], Import("gleam/json", None, [UnqualifiedImport("Json", None)], []))], [Definition([], CustomType("Field", Public, False, ["key", "val"], [Variant("Field", [LabelledVariantField(VariableType("key"), "key"), LabelledVariantField(VariableType("val"), "val")])]))], [], [], [Definition([], Function("encode_field", Public, [FunctionParameter(None, Named("value"), Some(NamedType("Field", None, [VariableType("key"), VariableType("val")]))), FunctionParameter(None, Named("encode_key"), Some(FunctionType([VariableType("key")], NamedType("Json", None, [])))), FunctionParameter(None, Named("encode_val"), Some(FunctionType([VariableType("val")], NamedType("Json", None, []))))], Some(NamedType("Json", None, [])), [Expression(Case([Variable("value")], [Clause([[PatternAssignment(PatternConstructor(None, "Field", [], True), "value")]], None, Call(FieldAccess(Variable("json"), "object"), [UnlabelledField(List([Tuple([String("key"), Call(Variable("encode_key"), [UnlabelledField(FieldAccess(Variable("value"), "key"))])]), Tuple([String("val"), Call(Variable("encode_val"), [UnlabelledField(FieldAccess(Variable("value"), "val"))])])], None))]))]))], Span(391, 669)))]))

fn encode_type_func(
  type_: CustomType,
  all_field_opts all_field_opts: DerivFieldOpts,
) -> Definition(Function) {
  let name = "encode_" <> util.snake_case(type_.name)

  let parameters =
    [FunctionParameter(None, Named("value"), Some(NamedType(type_.name, None,
      type_.parameters
      |> list.map(VariableType)
    )))]
    |> list.append({
      type_.parameters
      |> list.map(fn(param_type_name) {
        FunctionParameter(None, Named("encode_" <> param_type_name),
          Some(FunctionType([VariableType(param_type_name)], NamedType("Json", None, [])))
        )
      })
    })

  let return = Some(NamedType("Json", None, []))

  let encode_variant_clause_exprs =
    type_.variants
    |> list.map(fn(variant) {
      let encode_json_object_expr = encode_variant_json_object_expr(type_, variant, all_field_opts)

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

type TypeParams {
  TypeParams(
    decoder_func_params: List(FunctionParameter),
    decoder_calls: List(Field(Expression)),
    decoder_inner_type_params: List(Type),
    decoder_names: List(String),
  )
}

fn type_params(
  type_parameters: List(String),
) -> TypeParams {
  let decoder_names =
    type_parameters
    |> list.map(fn(param_type_name) {
      "decoder_" <> param_type_name
    })

  let decoder_calls =
    decoder_names
    |> list.map(fn(decoder_name) {
      Variable(decoder_name) |> UnlabelledField
    })

  let decoder_inner_type_params =
    type_parameters
    |> list.map(VariableType)

  let decoder_func_params =
    type_parameters
    |> list.map(fn(param_type_name) {
      let decoder_type = NamedType("Decoder", None, [VariableType(param_type_name)])
      let param_name = Named("decoder_" <> param_type_name)
      FunctionParameter(None, param_name, Some(decoder_type))
    })

  TypeParams(
    decoder_func_params:,
    decoder_inner_type_params:,
    decoder_calls:,
    decoder_names:,
  )
}

fn decoder_type_func(
  type_: CustomType,
  all_field_opts: DerivFieldOpts,
) -> List(Definition(Function)) {
  let variant_funcs =
    type_.variants
    |> list.map(decoder_type_variant_func(type_, _, all_field_opts))

  let TypeParams(
    decoder_func_params:,
    decoder_inner_type_params:,
    decoder_calls:,
    ..
  ) = type_params(type_.parameters)

  let body = {
    let #(first_decoder_call_expr, rest_decoder_call_exprs) =
      variant_funcs
      |> list.map(fn(func) {
        Call(Variable(util.func_name(func)), decoder_calls)
      })
      |> fn(exprs) {
        case exprs {
          [first, ..rest] -> #(first, rest)
          _ -> panic as { "No decoder expressions generated for: " <> string.inspect(type_) }
        }
      }

    [
      Expression(Call(FieldAccess(Variable("decode"), "one_of"), [
        UnlabelledField(first_decoder_call_expr),
        UnlabelledField(List(rest_decoder_call_exprs, None)),
      ]))
    ]
  }

  let return = Some(NamedType("Decoder", None, [NamedType(type_.name, None, decoder_inner_type_params)]))

  let type_func =
    Definition([], Function(
      location: dummy_location(),
      publicity: Public,
      name: "decoder_" <> util.snake_case(type_.name),
      parameters: decoder_func_params,
      return:,
      body: body,
    ))

  list.append([type_func], variant_funcs)
}

fn decoder_type_variant_func_name(type_: CustomType, variant: Variant) -> String {
  "decoder_" <> util.snake_case(type_.name) <> "_" <> util.snake_case(variant.name)
}

fn decode_field_expr(
  type_: CustomType,
  variant: Variant,
  field: VariantField,
  all_field_opts: DerivFieldOpts,
  local_decoders: List(String),
) -> #(String, Bool, Option(String), Expression) {
  let field = variant_field(field)

  let t = jtype(field.type_)

  let birl_time_kind = util.birl_time_kind(type_, variant, field.name, all_field_opts)

  let opts = util.get_field_opts(all_field_opts, type_, variant, field.name)

  let expr =
    case specifies_decoder(opts), t, t.parameters {
      Some(decoder_name), _, _ ->
        Call(function: Variable(decoder_name), arguments: [])

      _, JType("Option", _, [JType("List", _, [JType(_, _, []) as param])]), _ -> {
        let inner =
          param.name
          |> unparameterized_type_decode_expr(birl_time_kind, opts, local_decoders)
          |> UnlabelledField

        Call(FieldAccess(Variable("decode"), "optional"), [
          UnlabelledField(Call(FieldAccess(Variable("decode"), "list"), [inner]))
        ])
      }
      _, _, [] -> unparameterized_type_decode_expr(t.name, birl_time_kind, opts, local_decoders)
      _, _, [JType(parameters: [], ..) as param] ->
        case t.name {
          "List" -> {
            let inner =
              param.name
              |> unparameterized_type_decode_expr(birl_time_kind, opts, local_decoders)
              |> UnlabelledField

            Call(FieldAccess(Variable("decode"), "list"), [inner])
          }

          "Option" -> {
            let inner =
              param.name
              |> unparameterized_type_decode_expr(birl_time_kind, opts, local_decoders)
              |> UnlabelledField

            Call(FieldAccess(Variable("decode"), "optional"), [inner])
          }

          _ -> {
            io.debug(field)
            panic as "unimplemented"
          }
        }
      _no_decoder_specified, _jtype_parameterized, [
        JType(parameters: [], ..) as key_param,
        JType(parameters: [], ..) as val_param,
      ] ->
        case t.name, key_param.name {
          "Dict", "String" -> {
            let val_decode_expr = unparameterized_type_decode_expr(val_param.name, birl_time_kind, opts, local_decoders)

            Call(
              function: FieldAccess(Variable("decode"), "dict"),
              arguments: [
                UnlabelledField(FieldAccess(Variable("decode"), "string")),
                UnlabelledField(val_decode_expr),
              ]
            )
          }

          _, _ ->
            type_decode_expr(t, birl_time_kind, opts, local_decoders)
        }
      _, _, _ ->
        type_decode_expr(t, birl_time_kind, opts, local_decoders)
    }

  let json_field_name =
    all_field_opts
    |> util.get_field_opts(type_, variant, field.name)
    |> json_field_name(field, _)

  let is_option =
    case field.type_ {
      NamedType(name:, ..) if name == "Option" -> True
      _ -> False
    }

  #(field.name, is_option, Some(json_field_name), expr) // TODO always `Some`
}

// fn decode_field_expr_for_parameterized_type(
//   t: JType,
//   // field: VarField,
// ) -> Expression {
// // JType("Field", None, [JType("String", None, []), JType("String", None, [])])
// // VarField("scalar", NamedType("Field", None, [NamedType("String", None, []), NamedType("String", None, [])]))
//   case t.name, t.module, t.parameters {
//     _, _, _ -> todo
//   }
//   // Call(FieldAccess(Variable("decode"), "list"), [inner])
// }

fn decoder_type_variant_func(
  type_: CustomType,
  variant: Variant,
  all_field_opts: DerivFieldOpts,
) -> Definition(Function) {
  let name = decoder_type_variant_func_name(type_, variant)

  let TypeParams(
    decoder_func_params:,
    decoder_inner_type_params:,
    decoder_names:,
    ..
  ) = type_params(type_.parameters)

  let parameters: List(glance.FunctionParameter) = decoder_func_params
  let return: Option(Type) = Some(NamedType("Decoder", None, [NamedType(type_.name, None, decoder_inner_type_params)]))

  let pipe_exprs: List(#(String, Bool, Option(String), Expression)) =
    variant.fields
    |> list.map(decode_field_expr(type_, variant, _, all_field_opts, decoder_names))

  let use_decode_field_exprs: List(Statement) =
    list.fold(pipe_exprs, [], fn(acc, x) {
      let #(field, is_option, json_field, expr) = x

      let json_field = json_field |> option.unwrap(field)

      let call =
        case string.split(json_field, ".") {
          [] -> panic

          [json_field] ->
            case is_option {
              True ->
                Use(patterns: [PatternVariable(field)], function: {
                  Call(
                    function: FieldAccess(Variable("decode"), "optional_field"),
                    arguments: [
                      UnlabelledField(String(json_field)),
                      UnlabelledField(Variable("None")),
                      UnlabelledField(expr),
                    ])
                  })

              False ->
                Use(patterns: [PatternVariable(field)], function: {
                  Call(
                    function: FieldAccess(Variable("decode"), "field"),
                    arguments: [
                      UnlabelledField(String(json_field)),
                      UnlabelledField(expr),
                    ])
                  })
            }

          json_fields -> {
            let json_fields =
              json_fields
              |> list.map(fn(str) { String(str) })

            case is_option {
              True ->
                Call(
                  function: FieldAccess(Variable("decode"), "then"),
                  arguments: [
                    Call(
                      function: FieldAccess(Variable("decode"), "optionally_at"),
                      arguments: [
                        UnlabelledField(List(json_fields, None)),
                        UnlabelledField(Variable("None")),
                        UnlabelledField(expr),
                      ]
                    )
                    |> UnlabelledField
                  ]
                )
                |> Use(patterns: [PatternVariable(field)], function: _)

              False ->
                Call(
                  function: FieldAccess(Variable("decode"), "subfield"),
                  arguments: [
                    UnlabelledField(List(json_fields, None)),
                    UnlabelledField(expr),
                  ]
                )
                |> Use(patterns: [PatternVariable(field)], function: _)
            }
          }
        }

      list.append(acc, [call])
    })

  let constr_args =
    pipe_exprs
    |> list.map(fn(x) {
      let #(field, _, _, _) = x
      field
    })
    |> list.map(ShorthandField)

  let decode_success_call: Statement =
    Call(FieldAccess(Variable("decode"), "success"), [
      UnlabelledField(
        Call(
          function: Variable(variant.name),
          arguments: constr_args,
        )
      )
    ])
    |> Expression

  let use_decode_multi_var_type_exprs =
    case is_multi_variant(type_) {
      True -> [
        Call(
          function: FieldAccess(Variable("decode"), "field"),
          arguments: [
            UnlabelledField(String("_var")),
            UnlabelledField(Call(FieldAccess(Variable("util"), "is"), [
              UnlabelledField(String(variant.name)),
            ])),
          ]
        )
        |> Use(patterns: [PatternDiscard("deriv_var_constr")], function: _)
      ]
      False -> []
    }

  let body: List(Statement) =
    list.flatten([
      use_decode_multi_var_type_exprs,
      use_decode_field_exprs,
      [decode_success_call]
    ])

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

// TODO priv
pub fn decoder_type_alias_func(
  type_alias: TypeAlias,
) -> Definition(Function) {
  let name = "decoder_" <> util.snake_case(type_alias.name)

  let TypeParams(
    decoder_func_params:,
    decoder_inner_type_params:,
    ..
  ) = type_params(type_alias.parameters)

  let parameters: List(glance.FunctionParameter) = decoder_func_params
  let return: Option(Type) = Some(NamedType("Decoder", None, [NamedType(type_alias.name, None, decoder_inner_type_params)]))

  let birl_time_kind = BirlTimeISO8601 // TODO (?) allow deriv opt?

  let body: List(Statement) =
    [Expression(
      type_decode_expr(jtype(type_alias.aliased), birl_time_kind, [], [])
    )]

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
  birl_time_kind: BirlTimeKind,
  _opts: List(DerivFieldOpt),
  local_decoders: List(String),
) -> Expression {
  case type_name {
    "Int" -> FieldAccess(Variable("decode"), "int")
    "Float" -> FieldAccess(Variable("decode"), "float")
    "String" -> FieldAccess(Variable("decode"), "string")
    "Bool" -> FieldAccess(Variable("decode"), "bool")
    "Uuid" -> Call(FieldAccess(Variable("util"), "decoder_uuid"), [])
    "Time" -> birl_time_decode_expr(birl_time_kind)
    _ -> {
      let decoder_name = "decoder_" <> util.snake_case(type_name)
      case decoder_name |> list.contains(local_decoders, _) {
        True ->
          Variable(decoder_name)

        False ->
          Call(Variable(decoder_name), [])
      }
    }
  }
}

fn type_decode_expr(
  type_: JType,
  birl_time_kind: BirlTimeKind,
  opts: List(DerivFieldOpt),
  local_decoders: List(String),
) -> Expression {
  // unparameterized_type_decode_expr_(opts)
  // |> result.map_error(fn(_nil) {
    case type_.name, type_.parameters {
      "Int", _ -> FieldAccess(Variable("decode"), "int")
      "Float", _ -> FieldAccess(Variable("decode"), "float")
      "String", _ -> FieldAccess(Variable("decode"), "string")
      "Bool", _ -> FieldAccess(Variable("decode"), "bool")
      "Uuid", _ -> Call(FieldAccess(Variable("util"), "decoder_uuid"), [])
      "Time", _ -> birl_time_decode_expr(birl_time_kind)
      "Dict", params -> {
        let params =
          params
          |> list.map(type_decode_expr(_, birl_time_kind, opts, local_decoders))
          |> list.map(UnlabelledField)

        Call(FieldAccess(Variable("decode"), "dict"), params)
      }
      "List", params -> {
        let params =
          params
          |> list.map(type_decode_expr(_, birl_time_kind, opts, local_decoders))
          |> list.map(UnlabelledField)

        Call(FieldAccess(Variable("decode"), "list"), params)
      }
      "Option", params -> {
        let params =
          params
          |> list.map(type_decode_expr(_, birl_time_kind, opts, local_decoders))
          |> list.map(UnlabelledField)

        Call(FieldAccess(Variable("decode"), "optional"), params)
      }
      _, [] -> panic as {
        "`deriv/json.type_decode_expr` doesn't know what to do with type: "
          <> type_.name <> "\n" <> string.inspect(type_)
      }
      _, params -> {
        let decoder_name = "decoder_" <> util.snake_case(type_.name)

        case decoder_name |> list.contains(local_decoders, _) {
          True ->
            Variable(decoder_name)

          False -> {
            let params =
              params
              |> list.map(type_decode_expr(_, birl_time_kind, opts, local_decoders))
              |> list.map(UnlabelledField)

            Call(Variable(decoder_name), params)
          }
        }
      }
    }
  // })
  // |> result.unwrap_both
}



// fn unparameterized_type_decode_expr_(
//   opts: List(DerivFieldOpt),
// ) -> Result(Expression, Nil) {
//   opts
//   |> list.reverse
//   |> list.find_map(fn(x) {
//     case x {
//       DerivFieldOpt(strs: ["json", "decoder", decoder_name]) -> Ok(decoder_name)
//       _ -> Error(Nil)
//     }
//   })
//   |> result.map(fn(decoder_name) {
//     Call(function: Variable(decoder_name), arguments: [])
//   })
// }

fn specifies_decoder(
  opts: List(DerivFieldOpt),
) -> Option(String) {
  opts
  |> list.reverse
  |> list.find_map(fn(x) {
    case x {
      DerivFieldOpt(strs: ["json", "decoder", decoder_name]) -> Ok(decoder_name)
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

fn birl_time_decode_expr(
  birl_time_kind: BirlTimeKind,
) -> Expression {
  birl_time_kind
  |> fn(kind) {
    case kind {
      BirlTimeISO8601 -> "decoder_birl_parse"
      BirlTimeNaive -> "decoder_birl_from_naive"
      BirlTimeHTTP -> "decoder_birl_from_http"
      BirlTimeUnix -> "decoder_birl_from_unix"
      BirlTimeUnixMilli -> "decoder_birl_from_unix_milli"
      BirlTimeUnixMicro -> "decoder_birl_from_unix_micro"
    }
  }
  |> fn(func) {
    Call(FieldAccess(Variable("util"), func), [])
  }
}

fn birl_time_encode_expr(
  birl_time_kind: BirlTimeKind,
) -> Expression {
  birl_time_kind
  |> fn(kind) {
    case kind {
      BirlTimeISO8601 -> "encode_birl_to_iso8601"
      BirlTimeNaive -> "encode_birl_to_naive"
      BirlTimeHTTP -> "encode_birl_to_http"
      BirlTimeUnix -> "encode_birl_to_unix"
      BirlTimeUnixMilli -> "encode_birl_to_unix_milli"
      BirlTimeUnixMicro -> "encode_birl_to_unix_micro"
    }
  }
  |> fn(func) {
    FieldAccess(Variable("util"), func)
  }
}
