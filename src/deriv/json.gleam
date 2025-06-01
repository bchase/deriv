import gleam/option.{type Option, Some, None}
import gleam/dict
import gleam/result
import gleam/list
import gleam/string
import gleam/regexp
import gleam/io
import glance.{type CustomType, type Variant, type VariantField, LabelledVariantField, UnlabelledVariantField, NamedType, VariableType, type Import, Import, UnqualifiedImport, Definition, CustomType, Public, Variant, Function, type FunctionParameter, type Field, FieldAccess, Variable, Span, Expression, Call, UnlabelledField, Block, Use, BinaryOperator, Pipe, PatternVariable, PatternDiscard, ShorthandField, String, FunctionParameter, Tuple, Named, List, type Definition, type Function, type Span, type Expression, type Statement, type Type, Clause, Case, PatternAssignment, PatternConstructor, Fn, FnParameter, FnCapture, FunctionType, type TypeAlias, TypeAlias}
import deriv/types.{type File, type Derivation, type DerivFieldOpt, File, type Gen, Gen, type DerivFieldOpts, type ModuleReader, DerivFieldOpt} as deriv
import deriv/util.{type BirlTimeKind, BirlTimeISO8601, BirlTimeUnixMicro, BirlTimeUnixMilli, BirlTimeUnix, BirlTimeHTTP, BirlTimeNaive}

// TODO refactor
//   - `unparameterized_type_decode_expr` should be able to be replaced with `type_decode_expr`
//   - `unparameterized_type_encode_expr` should be able to be replaced with `type_encode_expr`
//   - `dict_key_decoder` for anything other than `String` should import `util`

const deriv_variant_json_key = "_var"

type Context {
  Context(
    deriv: Derivation,
    all_field_opts: DerivFieldOpts,
    file: File,
    module_reader: ModuleReader,
    type_aliases: List(TypeAlias),
  )
}

fn to_gen_func(
  f: fn(deriv.Type, Context) -> List(Definition(Function)),
  deriv: Derivation,
  module_reader: ModuleReader,
) -> fn(deriv.Type, DerivFieldOpts, File) -> List(Definition(Function)) {
  fn(
    type_: deriv.Type,
    field_opts: DerivFieldOpts,
    file: File,
  ) {
    f(type_, Context(file:, deriv:, module_reader:, all_field_opts: field_opts, type_aliases: type_aliases_in(file:)))
  }
}

pub fn gen(
  type_: deriv.Type,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  let opts = deriv.opts

  let gen_funcs_for_opts =
    [
      #("decode", gen_json_decoders),
      #("encode", gen_json_encoders |> to_gen_func(deriv, module_reader)),
    ]
    |> dict.from_list

  let imports =
    case type_ {
      deriv.Type(type_:) ->
        gen_imports(opts, type_)

      deriv.TypeAlias(..) ->
        default_imports
        |> dict.from_list
        |> fn(imports) {
          opts
          |> list.fold([], fn(acc, opt) {
            imports
            |> dict.get(opt)
            |> result.unwrap([])
            |> list.append(acc, _)
          })
        }
    }

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
  // TODO use `default_imports`
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

const default_imports =
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

// fn contains_non_string_key_dict(src: String) -> Bool {
//   let assert Ok(re) =
//     "Dict[(]\\s*(Int|Float|Bool|([a-z0-9_]+[.])?Uuid)\\s*[,]"
//     |> regexp.compile(regexp.Options(case_insensitive: False, multi_line: True))

//   src
//   |> regexp.check(re, _)
// }

fn type_aliases_in(
  file file: File,
) -> List(TypeAlias) {
  let assert Ok(glance.Module(type_aliases:, ..)) =
    glance.module(file.src)

  type_aliases
  |> list.map(fn(ta) { ta.definition })
}

fn gen_json_decoders(
  type_: deriv.Type,
  field_opts: DerivFieldOpts,
  file: File,
) -> List(Definition(Function)) {
  let type_aliases = type_aliases_in(file:)

  case type_ {
    deriv.TypeAlias(type_alias:)  ->
      decoder_type_alias_func(type_alias, field_opts, type_aliases)
      |> list.wrap

    deriv.Type(type_:)  ->
      decoder_type_func(type_, field_opts, type_aliases)
  }
}

fn gen_json_encoders(
  type_: deriv.Type,
  ctx: Context,
) -> List(Definition(Function)) {
  let type_aliases = type_aliases_in(file: ctx.file)

  // TODO qualify imports using `file` (`idx`)
  case type_ {
    deriv.TypeAlias(type_alias:)  ->
      encode_type_alias_func(type_alias, ctx)
      |> fn(func) { [ func ] }

    deriv.Type(type_:)  ->
      encode_type_func(type_, ctx)
      |> fn(func) { [ func ] }
  }
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

fn type_encode_expr(
  type_: JType,
  type_alias: Option(Bool),
  encode_func_name_override: Option(String),
  birl_time_kind: BirlTimeKind,
  type_aliases: List(TypeAlias),
  encode_arg encode_arg: Field(Expression),
  wrap wrap: Option(fn(Expression) -> Expression)
) -> Expression {
  let type_alias =
    type_alias
    |> option.lazy_unwrap(fn() {
      let type_alias_names =
        type_aliases
        |> list.map(fn(ta) { ta.name })

      type_alias_names |> list.contains(type_.name)
    })

  let handle_type_aliases =
    fn(expr) {
      case type_alias {
        True -> Call(expr, [encode_arg])
        False -> expr
      }
    }

  let encode_with_params_or_override =
    fn(expr) {
      let params =
        case encode_func_name_override {
          Some(encode_func_name_override) ->
            [
              encode_arg,
              UnlabelledField(Variable(encode_func_name_override)),
            ]

          None ->
            [
              encode_arg,
            ]
            |> list.append({
              type_.parameters
              |> list.map(type_encode_expr(_, None, encode_func_name_override, birl_time_kind, type_aliases, wrap: None, encode_arg: {
                UnlabelledField(Variable("_"))
              }))
              |> list.map(UnlabelledField)
            })
        }

      Call(expr, params)
    }

  let expr =
    case type_.name, encode_func_name_override {
      "Int", Some(encode_func_name_override) |
      "Float", Some(encode_func_name_override) |
      "String", Some(encode_func_name_override) |
      "Bool", Some(encode_func_name_override) |
      "Uuid", Some(encode_func_name_override) |
      "Time", Some(encode_func_name_override) -> {
        // FieldAccess(Variable("json"), type_.name |> string.lowercase)
        Variable(encode_func_name_override)
        |> handle_type_aliases
      }

      "Int", None |
      "Float", None |
      "String", None |
      "Bool", None -> {
        FieldAccess(Variable("json"), type_.name |> string.lowercase)
        |> handle_type_aliases
      }

      "Uuid", None ->
        FieldAccess(Variable("util"), "encode_uuid")
        |> handle_type_aliases

      "Time", None ->
        birl_time_encode_expr(birl_time_kind)
        |> handle_type_aliases

      "Option", _ ->
        encode_with_params_or_override(FieldAccess(Variable("json"), "nullable"))

      "List", _ ->
        encode_with_params_or_override(FieldAccess(Variable("json"), "array"))

      "Dict", _ -> {
        let _str_keys_only =
          case type_.parameters {
            [JType(name: "String", parameters: [], ..), _] -> Nil
            [JType(name: "Int", parameters: [], ..), _] -> Nil
            [JType(name: "Float", parameters: [], ..), _] -> Nil
            [JType(name: "Bool", parameters: [], ..), _] -> Nil
            [JType(name: "Uuid", parameters: [], ..), _] -> Nil
            _ ->  panic as {
              "`json.dict` only supports `String`, `Int`, `Float`, `Bool`, and `Uuid` keys, but got: " <> string.inspect(type_)
            }
          }

        let params =
          [
            encode_arg,
            UnlabelledField(FieldAccess(Variable("string"), "inspect"))
          ]
          |> list.append({
            type_.parameters
            |> list.map(type_encode_expr(_, None, encode_func_name_override, birl_time_kind, type_aliases, wrap: None, encode_arg: {
              UnlabelledField(Variable("_"))
            }))
            |> list.map(UnlabelledField)
            |> list.rest
            |> result.unwrap([])
          })

        Call(FieldAccess(Variable("json"), "dict"), params)
      }

      _, _ -> {
        let encoder_name = "encode_" <> util.snake_case(type_.name)

        case type_alias {
          True ->
            encode_with_params_or_override(Variable(encoder_name))

          False -> {
            let encoder_name =
              encode_func_name_override
              |> option.unwrap(encoder_name)

            case type_.parameters {
              [] ->
                Variable(encoder_name)
                |> handle_type_aliases

              params -> {
                let params =
                  [
                    encode_arg,
                  ]
                  |> list.append({
                    params
                    |> list.map(type_encode_expr(_, None, encode_func_name_override, birl_time_kind, type_aliases, wrap: None, encode_arg:))
                    |> list.map(UnlabelledField)
                  })

                Call(Variable(encoder_name), params)
              }
            }
          }
        }
      }
    }

  case wrap {
    None -> expr
    Some(f) -> f(expr)
  }
}

// fn unparameterized_type_encode_expr(
//   type_name: String,
//   birl_time_kind: BirlTimeKind,
//   wrap wrap: Option(fn(Expression) -> Expression)
// ) -> Expression {
//   let expr =
//     case type_name {
//       "Int" -> FieldAccess(Variable("json"), "int")
//       "Float" -> FieldAccess(Variable("json"), "float")
//       "String" -> FieldAccess(Variable("json"), "string")
//       "Bool" -> FieldAccess(Variable("json"), "bool")
//       "Uuid" -> FieldAccess(Variable("util"), "encode_uuid")
//       "Time" -> birl_time_encode_expr(birl_time_kind)
//       _ -> Variable("encode_" <> util.snake_case(type_name))
//     }

//   case wrap {
//     None -> expr
//     Some(f) -> f(expr)
//   }
// }

fn encode_field(
  type_: CustomType,
  variant: Variant,
  field: VarField,
  ctx: Context,
) -> Expression {
  let all_field_opts = ctx.all_field_opts

  let ftype = jtype(field.type_)

  let birl_time_kind = util.birl_time_kind(type_, variant, field.name, all_field_opts)

  let encode_func_name_override =
    ctx.all_field_opts
    |> util.get_field_opts(type_, variant, field.name)
    |> specifies_encode_func

  case ftype, ftype.parameters {
    JType("Option", _, [JType("List", _, [JType(_, _, []) as param])]), _ -> {
      // let param_type_encoder = unparameterized_type_encode_expr(param.name, birl_time_kind, None)
      let param_type_encoder = type_encode_expr(param, None, encode_func_name_override,  birl_time_kind, ctx.type_aliases, wrap: None, encode_arg: {
        UnlabelledField(Variable("value"))
      })

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
    _, [] -> {
      // unparameterized_type_encode_expr(ftype.name, birl_time_kind, Some(fn(func_expr) {
      //   Call(
      //     function: func_expr,
      //     arguments: [
      //       UnlabelledField(FieldAccess(Variable("value"), field.name)),
      //     ]
      //   )
      // }))
      type_encode_expr(ftype, None, encode_func_name_override, birl_time_kind, ctx.type_aliases,
        encode_arg: {
          UnlabelledField(Variable("value"))
        },
        wrap: Some(fn(func_expr) {
          Call(
            function: func_expr,
            arguments: [
              UnlabelledField(FieldAccess(Variable("value"), field.name)),
            ]
          )
        },
      ))
    }
    // _, [JType(name: param_type_name, module: None, parameters: [])] ->
    //   case ftype.name {
    //     "Option" | "List" -> {
    //       type_encode_expr(ftype, Some(False), birl_time_kind, ctx.type_aliases, wrap: None,
    //         encode_arg: {
    //           UnlabelledField(FieldAccess(Variable("value"), field.name))
    //         },
    //       )
    //     }
    //     _ -> {
    //       // io.debug(ftype)
    //       // panic as "Not yet implemented for type printed above"
    //       Call(Variable("encode_" <> util.snake_case(ftype.name)), [])
    //     }
    //   }

    _, [
      JType(name: key_param_type_name, module: None, parameters: []),
      JType(name: val_param_type_name, module: None, parameters: []),
    ] -> {
      case ftype.name, key_param_type_name {
        "Dict", "String" -> {
          type_encode_expr(ftype, Some(False), encode_func_name_override, birl_time_kind, ctx.type_aliases, wrap: None,
            encode_arg: {
              UnlabelledField(FieldAccess(Variable("value"), field.name))
            },
          )
        }
        _, _ ->
          type_encode_expr(ftype, None, encode_func_name_override, birl_time_kind, ctx.type_aliases, wrap: None, encode_arg: {
            UnlabelledField(FieldAccess(Variable("value"), field.name))
          })
      }
    }
    _, _ -> {
      type_encode_expr(ftype, None, encode_func_name_override, birl_time_kind, ctx.type_aliases, wrap: None, encode_arg: {
        UnlabelledField(FieldAccess(Variable("value"), field.name))
      })
    }
  }
}

fn encode_variant_json_object_expr(
  type_: CustomType,
  variant: Variant,
  ctx: Context,
) -> Expression {
  let encode_lines =
    variant.fields
    |> list.map(fn(field) {
      let field = variant_field(field)

      let json_field_name =
        ctx.all_field_opts
        |> util.get_field_opts(type_, variant, field.name)
        |> json_field_name(field, _)

      let encode_expr = encode_field(type_, variant, field, ctx)

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

fn encode_type_func(
  type_: CustomType,
  ctx: Context,
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
      let encode_json_object_expr = encode_variant_json_object_expr(type_, variant, ctx)

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

fn encode_type_alias_func(
  type_alias: TypeAlias,
  ctx: Context,
) -> Definition(Function) {
  let name = "encode_" <> util.snake_case(type_alias.name)

  let parameters =
    [FunctionParameter(None, Named("value"), Some(NamedType(type_alias.name, None,
      type_alias.parameters
      |> list.map(VariableType)
    )))]
    |> list.append({
      type_alias.parameters
      |> list.map(fn(param_type_name) {
        FunctionParameter(None, Named("encode_" <> param_type_name),
          Some(FunctionType([VariableType(param_type_name)], NamedType("Json", None, [])))
        )
      })
    })

  let return = Some(NamedType("Json", None, []))

  // let encode_variant_clause_exprs =
  //   type_.variants
  //   |> list.map(fn(variant) {
  //     let encode_json_object_expr = encode_variant_json_object_expr(type_, variant, all_field_opts)

  //     Clause([[PatternAssignment(PatternConstructor(None, variant.name, [], True), "value")]], None,
  //       encode_json_object_expr
  //     )
  //   })

  let birl_time_kind = BirlTimeISO8601 // TODO (?) allow deriv opt?
  let expr =
    type_encode_expr(jtype(type_alias.aliased), Some(True), None, birl_time_kind, ctx.type_aliases, wrap: None, encode_arg: {
      UnlabelledField(Variable("value"))
    })

  let body =
    Expression(expr)
    |> list.wrap

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
  type_aliases: List(TypeAlias),
) -> List(Definition(Function)) {
  let variant_funcs =
    type_.variants
    |> list.map(decoder_type_variant_func(type_, _, all_field_opts, type_aliases))

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
  type_aliases: List(TypeAlias),
) -> #(String, Bool, Option(String), Expression) {
  let field = variant_field(field)

  let t = jtype(field.type_)

  let birl_time_kind = util.birl_time_kind(type_, variant, field.name, all_field_opts)

  let opts = util.get_field_opts(all_field_opts, type_, variant, field.name)

  let expr = type_decode_expr(t, type_aliases, birl_time_kind, opts, local_decoders)

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
  type_aliases: List(TypeAlias),
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
    |> list.map(decode_field_expr(type_, variant, _, all_field_opts, decoder_names, type_aliases))

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
  _opt: DerivFieldOpts,
  type_aliases: List(TypeAlias),
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
      type_decode_expr(jtype(type_alias.aliased), type_aliases, birl_time_kind, [], [])
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

// fn unparameterized_type_decode_expr(
//   type_name: String,
//   birl_time_kind: BirlTimeKind,
//   _opts: List(DerivFieldOpt),
//   local_decoders: List(String),
// ) -> Expression {
//   case type_name {
//     "Int" -> FieldAccess(Variable("decode"), "int")
//     "Float" -> FieldAccess(Variable("decode"), "float")
//     "String" -> FieldAccess(Variable("decode"), "string")
//     "Bool" -> FieldAccess(Variable("decode"), "bool")
//     "Uuid" -> Call(FieldAccess(Variable("util"), "decoder_uuid"), [])
//     "Time" -> birl_time_decode_expr(birl_time_kind)
//     _ -> {
//       let decoder_name = "decoder_" <> util.snake_case(type_name)
//       case decoder_name |> list.contains(local_decoders, _) {
//         True ->
//           Variable(decoder_name)

//         False ->
//           Call(Variable(decoder_name), [])
//       }
//     }
//   }
// }

fn dict_key_decoder(
  type_: JType,
) -> Expression {
  case type_.parameters {
    [] ->
      case type_.name {
        "String" ->
          FieldAccess(Variable("decode"), "string")

        "Int" | "Float" | "Bool" | "Uuid" ->
          Call(FieldAccess(Variable("util"), { "decoder_" <> string.lowercase(type_.name) <> "_string" }), [])

        _ -> panic as { "`dict_key_decoder` doesn't know what to do with type: " <> string.inspect(type_)}
      }

    _ ->
      panic as { "`dict_key_decoder` doesn't know what to do with type: " <> string.inspect(type_)}
  }
}

fn type_decode_expr(
  type_: JType,
  type_aliases: List(TypeAlias),
  birl_time_kind: BirlTimeKind,
  opts: List(DerivFieldOpt),
  local_decoders: List(String),
) -> Expression {
  let decoder_name_override =
    opts
    |> list.filter_map(fn(opt) {
      case opt {
        DerivFieldOpt(strs: ["json", "decoder", decoder_name ]) -> Ok(decoder_name)
        _ -> Error(Nil)
      }
    })
    |> list.first
    |> option.from_result

  let decoder_with_params_or_override =
    fn(expr, params) {
      let params =
        case decoder_name_override {
          Some(decoder_name_override) ->
            params
            |> list.map(fn(_) {
              UnlabelledField(Call(Variable(decoder_name_override), []))
            })

          None ->
            params
            |> list.map(type_decode_expr(_, type_aliases, birl_time_kind, opts, local_decoders))
            |> list.map(UnlabelledField)
        }

      Call(expr, params)
    }

  case decoder_name_override, type_.name, type_.parameters {
    Some(decoder_name), "List", _ ->
      Call(FieldAccess(Variable("decode"), "list"), [
        UnlabelledField(Call(Variable(decoder_name), [])),
      ])

    // Some(decoder_name), "Option", [JType("List", _, _)] ->
    //   Call(FieldAccess(Variable("decode"), "optional"), [
    //     UnlabelledField(Call(FieldAccess(Variable("decode"), "list"), [
    //       UnlabelledField(Variable("_")),
    //       UnlabelledField(Call(Variable(decoder_name), [])),
    //     ])),
    //   ])

    Some(decoder_name), _, _ ->
      Call(Variable(decoder_name), [])

    _, "Int", _ -> FieldAccess(Variable("decode"), "int")
    _, "Float", _ -> FieldAccess(Variable("decode"), "float")
    _, "String", _ -> FieldAccess(Variable("decode"), "string")
    _, "Bool", _ -> FieldAccess(Variable("decode"), "bool")
    _, "Uuid", _ -> Call(FieldAccess(Variable("util"), "decoder_uuid"), [])
    _, "Time", _ -> birl_time_decode_expr(birl_time_kind)
    _, "Dict", [key_type, val_type] -> {
      Call(FieldAccess(Variable("decode"), "dict"), [
        UnlabelledField(dict_key_decoder(key_type)),
        UnlabelledField(type_decode_expr(val_type, type_aliases, birl_time_kind, opts, local_decoders)),
      ])
    }
    _, "List", params -> {
      // let params =
      //   params
      //   |> list.map(type_decode_expr(_, type_aliases, birl_time_kind, opts, local_decoders))
      //   |> list.map(UnlabelledField)

      // Call(FieldAccess(Variable("decode"), "list"), params)
      case
        decoder_with_params_or_override(FieldAccess(Variable("decode"), "list"), params),
        opts |> list.any(fn(opt) { opt == DerivFieldOpt(strs: ["json", "decode", "default", "empty"]) })
      {
        expr, False ->
          expr

        expr, True ->
          Call(FieldAccess(Variable("decode"), "one_of"), [
            UnlabelledField(expr),
            UnlabelledField(List([
             Call(FieldAccess(Variable("decode"), "success"), [UnlabelledField(List([], None))]),
            ], None)),
          ])
      }
    }
    _, "Option", params -> {
      // let params =
      //   params
      //   |> list.map(type_decode_expr(_, type_aliases, birl_time_kind, opts, local_decoders))
      //   |> list.map(UnlabelledField)

      // Call(FieldAccess(Variable("decode"), "optional"), params)
      decoder_with_params_or_override(FieldAccess(Variable("decode"), "optional"), params)
    }
    _, type_name, [] ->
      case string.lowercase(type_name) == type_name {
        True ->
          Variable("decoder_" <> type_name)

        False ->
          case attempt_to_resolve_type_alias(type_name, type_aliases) {
            Ok(TypeAlias(aliased: NamedType(..) as type_, ..)) ->
              // panic as string.inspect(type_alias)
              type_decode_expr(jtype(type_), type_aliases, birl_time_kind, opts, local_decoders)

            _ ->
              // panic as {
              //   "`deriv/json.type_decode_expr` doesn't know what to do with type: "
              //     <> type_.name <> "\n" <> string.inspect(type_)
              // }
              Call(Variable("decoder_" <> util.snake_case(type_.name)), [])
          }
      }
    _, _, params -> {
      let decoder_name = "decoder_" <> util.snake_case(type_.name)

      case decoder_name |> list.contains(local_decoders, _) {
        True ->
          Variable(decoder_name)

        False -> {
          let params =
            params
            |> list.map(type_decode_expr(_, type_aliases, birl_time_kind, opts, local_decoders))
            |> list.map(UnlabelledField)

          Call(Variable(decoder_name), params)
        }
      }
    }
  }
}

fn attempt_to_resolve_type_alias(
  type_name: String,
  type_aliases: List(TypeAlias),
) -> Result(TypeAlias, Nil) {
  type_aliases
  |> list.find(fn(ta) { ta.name == type_name })
}

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

fn specifies_encode_func(
  opts: List(DerivFieldOpt),
) -> Option(String) {
  opts
  |> list.reverse
  |> list.find_map(fn(x) {
    case x {
      DerivFieldOpt(strs: ["json", "encode", encode_func_name]) -> Ok(encode_func_name)
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
