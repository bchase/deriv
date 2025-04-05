import gleam/option.{type Option, Some, None}
import gleam/dict
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, UnlabelledVariantField, NamedType, type Import, Import, UnqualifiedImport, Definition, CustomType, Public, Variant, Function, FieldAccess, Variable, Span, Expression, Call, UnlabelledField, Block, Use, BinaryOperator, Pipe, PatternVariable, ShorthandField, String, FunctionParameter, Tuple, Named, List, type Definition, type Function, type Span, type Expression, type Statement, type Type, Clause, Case, PatternAssignment, PatternConstructor, Fn, FnParameter, FnCapture}
import deriv/types.{type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen, type DerivFieldOpts, type ModuleReader, DerivFieldOpt}
import deriv/util.{type BirlTimeKind, BirlTimeISO8601, BirlTimeUnixMicro, BirlTimeUnixMilli, BirlTimeUnix, BirlTimeHTTP, BirlTimeNaive}

pub fn gen(
  type_: CustomType,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  _module_reader: ModuleReader,
) -> Gen {
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
  uses_uuid(type_) || uses_birl_time(type_)
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
        _, _ -> {
          io.debug(ftype)
          panic as "Not yet implemented for type printed above"
        }
      }

    _, _ -> {
      io.debug(ftype)
      panic as "Not yet implemented for type printed above"
    }
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

  Call(
    function: FieldAccess(Variable("json"), "object"),
    arguments: [ UnlabelledField(List(encode_lines, None)) ],
  )
}

fn encode_type_func(
  type_: CustomType,
  all_field_opts all_field_opts: DerivFieldOpts,
) -> Definition(Function) {
  let name = "encode_" <> util.snake_case(type_.name)

  let parameters = [FunctionParameter(None, Named("value"), Some(NamedType(type_.name, None, [])))]
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

fn decoder_type_func(
  type_: CustomType,
  all_field_opts: DerivFieldOpts,
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
  type_: CustomType,
  variant: Variant,
  field: VariantField,
  all_field_opts: DerivFieldOpts,
) -> #(String, Option(String), Expression) {
  let field = variant_field(field)

  let t = jtype(field.type_)

  let birl_time_kind = util.birl_time_kind(type_, variant, field.name, all_field_opts)

  let opts = util.get_field_opts(all_field_opts, type_, variant, field.name)

  let expr =
    case t, t.parameters {
      JType("Option", _, [JType("List", _, [JType(_, _, []) as param])]), _ -> {
        let inner =
          param.name
          |> unparameterized_type_decode_expr(birl_time_kind, opts)
          |> UnlabelledField

        Call(FieldAccess(Variable("decode"), "optional"), [
          UnlabelledField(Call(FieldAccess(Variable("decode"), "list"), [inner]))
        ])
      }
      _, [] -> unparameterized_type_decode_expr(t.name, birl_time_kind, opts)
      _, [JType(parameters: [], ..) as param] ->
        case t.name {
          "List" -> {
            let inner =
              param.name
              |> unparameterized_type_decode_expr(birl_time_kind, opts)
              |> UnlabelledField

            Call(FieldAccess(Variable("decode"), "list"), [inner])
          }

          "Option" -> {
            let inner =
              param.name
              |> unparameterized_type_decode_expr(birl_time_kind, opts)
              |> UnlabelledField

            Call(FieldAccess(Variable("decode"), "optional"), [inner])
          }

          _ -> {
            io.debug(field)
            panic as "unimplemented"
          }
        }
      _, [
        JType(parameters: [], ..) as key_param,
        JType(parameters: [], ..) as val_param,
      ] ->
        case t.name, key_param.name {
          "Dict", "String" -> {
            let val_decode_expr = unparameterized_type_decode_expr(val_param.name, birl_time_kind, opts)

            Call(
              function: FieldAccess(Variable("decode"), "dict"),
              arguments: [
                UnlabelledField(FieldAccess(Variable("decode"), "string")),
                UnlabelledField(val_decode_expr),
              ]
            )
          }

          _, _ -> {
            io.debug(field)
            panic as "unimplemented"
          }
        }
      _, _ -> {
        io.debug(field)
        panic as "unimplemented"
      }
    }

  let json_field_name =
    all_field_opts
    |> util.get_field_opts(type_, variant, field.name)
    |> json_field_name(field, _)

  #(field.name, Some(json_field_name), expr) // TODO always `Some`
}

fn decoder_type_variant_func(
  type_: CustomType,
  variant: Variant,
  all_field_opts: DerivFieldOpts,
) -> Definition(Function) {
  let name = decoder_type_variant_func_name(type_, variant)

  let parameters: List(glance.FunctionParameter) = []
  let return: Option(Type) = Some(NamedType("Decoder", None, [NamedType(type_.name, None, [])]))

  let pipe_exprs: List(#(String, Option(String), Expression)) =
    variant.fields
    |> list.map(decode_field_expr(type_, variant, _, all_field_opts))

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

      let call =
        case string.split(json_field, ".") {
          [] -> panic
          [json_field] ->
            Call(FieldAccess(Variable("decode"), "field"), [UnlabelledField(String(json_field)), UnlabelledField(expr)])
          json_fields -> {
            let json_fields =
              json_fields
              |> list.map(fn(str) { String(str) })

            Call(
              function: FieldAccess(Variable("decode"), "subfield"),
              arguments: [
                UnlabelledField(List(json_fields, None)),
                UnlabelledField(expr),
              ]
            )
          }
        }

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
  birl_time_kind: BirlTimeKind,
  opts: List(DerivFieldOpt),
) -> Expression {
  unparameterized_type_decode_expr_(opts)
  |> result.map_error(fn(_nil) {
    case type_name {
      "Int" -> FieldAccess(Variable("decode"), "int")
      "Float" -> FieldAccess(Variable("decode"), "float")
      "String" -> FieldAccess(Variable("decode"), "string")
      "Bool" -> FieldAccess(Variable("decode"), "bool")
      "Uuid" -> Call(FieldAccess(Variable("util"), "decoder_uuid"), [])
      "Time" -> birl_time_decode_expr(birl_time_kind)
      _ -> Call(Variable("decoder_" <> util.snake_case(type_name)), [])
    }
  })
  |> result.unwrap_both
}

fn unparameterized_type_decode_expr_(
  opts: List(DerivFieldOpt),
) -> Result(Expression, Nil) {
  opts
  |> list.reverse
  |> list.find_map(fn(x) {
    case x {
      DerivFieldOpt(strs: ["json", "decoder", decoder_name]) -> Ok(decoder_name)
      _ -> Error(Nil)
    }
  })
  |> result.map(fn(decoder_name) {
    Call(function: Variable(decoder_name), arguments: [])
  })
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
