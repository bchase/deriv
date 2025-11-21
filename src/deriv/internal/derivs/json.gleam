import gleam/float
import gleam/int
import gleam/regexp
import deriv/internal/parser
import gleam/pair
import gleam/bool
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/string
import glance as g
import deriv/internal/glance.{x, string, list, term, call, call_, dot, tuple, pipe, identity_func} as _
import deriv/internal/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type DerivFieldOpt, type ModuleReader, type Context, Context} as deriv
import deriv/internal/common.{gtype}

const deriv_variant_json_key = "_var"

pub fn gen(
  type_: deriv.Type,
  ctx: Context,
) -> Gen {
  let imports = gen_imports(type_, ctx)

  let gen_funcs_for_opts =
    [
      #("decode", gen_json_decoders),
      #("encode", gen_json_encoders),
      #("properties", gen_json_properties),
    ]
    |> dict.from_list

  let funcs =
    ctx.deriv.opts
    |> list.map(dict.get(gen_funcs_for_opts, _))
    |> result.values
    |> list.flat_map(fn(f) { f(type_, ctx) })

  let src =
    funcs
    |> list.map(common.func_str)
    |> string.join("\n\n")

  Gen(file: ctx.file, deriv: ctx.deriv, imports:, funcs:, types: [], src:, meta: dict.new())
}

fn alias_to_type(
  type_alias ta: g.TypeAlias,
) -> Type {
  case ta.aliased {
    g.NamedType(..) ->
      Type(
        publicity: ta.publicity,
        type_: ta.aliased,
        params: ta.parameters,
        pascal_case: ta.name,
        snake_case: ta.name |> common.snake_case,
        variants: [],
      )

    _ ->
      panic as "not implemented"
  }
}


fn gen_json_properties(
  type_: deriv.Type,
  ctx: Context,
) -> List(g.Definition(g.Function)) {
  case type_ {
    deriv.TypeAlias(..)  ->
      panic as "`json properties` not implemented for type aliases"

    deriv.Type(type_:)  ->
      json_properties_func(type_, ctx)
      |> list.wrap
  }
}

fn json_properties_func(
  type_: g.CustomType,
  ctx: Context,
) -> g.Definition(g.Function) {
  let name = "json_properties_for_" <> common.snake_case(type_.name)

  let variant_tuples =
    type_.variants
    |> list.map(fn(variant) {
      let property_strings_list =
        variant_props(type_:, variant:, ctx:, acc: [])
        |> list.map(fn(var) {
          list.map(var, fn(props) {
            string.join(props, ".")
          })
        })
        |> list.flatten
        |> list.map(g.String(x, _))
        |> g.List(x, _, None)

      #(
        variant.name,
        property_strings_list
      )
   })

  let #(body, return) =
    case variant_tuples {
      [#(_variant, list_expr)] ->
        #(
          {
            list_expr
            |> g.Expression
            |> fn(expr) { [ expr ] }
          },
          Some(gtype("List", [gtype("String", [])])),
        )

      _ -> {
        let variant_tuples =
          variant_tuples
          |> list.map(fn(t) {
            let #(variant, list_expr) = t

            g.Tuple(x, [
              g.String(x, variant),
              list_expr,
            ])
          })
        #(
          {
            g.List(x, variant_tuples, None)
            |> g.BinaryOperator(x, name: g.Pipe, left: _, right: {
              g.FieldAccess(x, g.Variable(x, "dict"), "from_list")
            })
            |> g.Expression
            |> fn(expr) { [ expr ] }
          },
          Some(gtype("Dict", [
            gtype("String", []),
            gtype("List", [gtype("String", [])]),
          ])),
        )
      }
    }

  g.Definition([],
    g.Function(x,
      publicity: g.Public,
      name:,
      parameters: [],
      return:,
      body:,
    )
  )
}

type PropertiesOpt {
  PropertyNamed(name: String)
  PropertyIgnored
}

fn variant_props(
  type_ type_: g.CustomType,
  variant variant: g.Variant,
  ctx ctx: Context,
  acc acc: List(String),
) -> List(List(List(String))) {
  variant.fields
  |> list.map(fn(field) {
    case field, field.item {
      g.UnlabelledVariantField(..), _ ->
        panic as "`json properties` doesn't implement `UnlabelledVariantField`s"

      g.LabelledVariantField(label: field, ..), t -> {
        let opts =
          ctx.opts
          |> common.get_field_opts(type_, variant, field)
          |> list.filter_map(fn(opt) {
            case opt.strs {
              ["json", "named", name] -> Ok(PropertyNamed(name:))
              ["json", "properties", "ignore"] -> Ok(PropertyIgnored)
              _ -> Error(Nil)
            }
          })

        let is_ignored =
          opts
          |> list.any(fn(opt) {
            case opt {
              PropertyIgnored -> True
              _ -> False
            }
          })

        // TODO use new `common.get_field_opt` after json rewrite merge
        let override =
          opts
          |> list.filter_map(fn(opt) {
            case opt {
              PropertyNamed(name:) -> Ok(name)
              _ -> Error(Nil)
            }
          })
          |> fn(opts) {
            case opts {
              [] -> None
              [x] -> Some(x)
              _ -> {
                io.println_error(type_ |> string.inspect)
                io.println_error(variant |> string.inspect)
                io.println_error(field |> string.inspect)
                panic as { "`json properties` collided for `json named` on: " <> field }
              }
            }
          }

        case is_ignored, override, t {
          True, _, _ ->
            []

          _, Some(property), _ ->
            acc
            |> list.append([property])
            |> list.wrap

          _, None, g.NamedType(..) as t ->
            case t.name, t.parameters {
              "Dict", [_, _] ->
                panic as "`json properties` not yet implemented for `Dict`"

              "String", [] |
              "Int", [] |
              "Float", [] |
              "Bool", [] ->
                acc
                |> list.append([field])
                |> list.wrap

              "Option", [g.NamedType(name: type_name, parameters:, ..)] |
              "List", [g.NamedType(name: type_name, parameters:, ..)] |
              type_name, parameters -> {
                case t.name, type_name, parameters {
                  "Option", "String", [] |
                  "Option", "Int", [] |
                  "Option", "Float", [] |
                  "Option", "Bool", [] |
                  "List", "String", [] |
                  "List", "Int", [] |
                  "List", "Float", [] |
                  "List", "Bool", [] ->
                    acc
                    |> list.append([field])
                    |> list.wrap

                  _, _, _ -> {
                    let ident = ctx.file.module <> "." <>  type_name
                    case common.fetch_custom_type(ident, ctx.module_reader) {
                      Error(_err) -> {
                        // io.println_error(err |> string.inspect)
                        // panic as {
                        //   "`json properties` couldn't resolve `" <> type_name <> "` " <>
                        //   "as `" <> ident <> "`.\n" <> " `json properties` doesn't yet support " <>
                        //   "type aliases or types in other modules."
                        // }
                        acc
                        |> list.append([field])
                        |> list.wrap
                      }

                      Ok(#(_, g.Definition(_, g.CustomType(variants: [variant], ..) as t))) -> {
                        case parser.parse_type_with_derivations(t, ctx.file.src) {
                          Error(Nil) -> panic as {
                            "`json properties` failed to parse type: " <> string.inspect(t)
                          }

                          Ok(#(t, _derivs, opts)) -> {
                            let ctx = Context(..ctx, opts: opts)

                            variant_props(type_: t, variant:, ctx:, acc: acc |> list.append([field]))
                            |> list.flatten
                          }
                        }
                      }

                      Ok(#(_, g.Definition(_, _))) -> {
                        panic as {
                          "`json properties` not yet implemented for nested multi-variant types -- " <>
                          variant.name
                        }
                      }
                    }
                  }
                }
              }
            }

          _, None, _ -> {
            panic as {
              "`json properties` expected a `glance.NamedType` but got: " <>
              field <> " " <> string.inspect(t)
            }
          }
        }
      }
    }
  })
}

fn gen_json_decoders(
  type_: deriv.Type,
  ctx: Context,
) -> List(g.Definition(g.Function)) {
  case type_ {
    deriv.TypeAlias(type_alias:) -> {
      let t = type_alias |> alias_to_type

      let name =
        type_alias.name
        |> common.snake_case
        |> string.append(to: "decoder_", suffix: _)

      g.Function(x,
        name:,
        publicity: t.publicity,
        parameters: {
          type_alias.parameters
          |> list.map(fn(param) {
            g.FunctionParameter(
              label: None,
              name: { "decoder_" <> param } |> g.Named,
              type_: Some(g.NamedType(x, module: None, name: "Decoder", parameters: [
                g.VariableType(x, name: param)
              ])),
            )
          })
        },
        return: Some(g.NamedType(x, module: None, name: "Decoder", parameters: [
          g.NamedType(x, module: None, name: type_alias.name, parameters: {
            type_alias.parameters
            |> list.map(fn(param) {
              g.VariableType(x, name: param)
            })
          }),
        ])),
        body: [
          decoder_call(type_: to_t(t.type_), field: None, opts: ctx.opts, inner: dict.new(), top_level: True) |> g.Expression,
        ],
      )
      |> list.wrap
      |> list.map(g.Definition([], _))
    }

    deriv.Type(type_:) -> {
      let is_multi_variant = type_ |> common.is_multi_variant

      let type_ = to_type(type_:, opts: ctx.opts)

      [
        type_decoder_func(type_:),
        ..{
          type_.variants
          |> list.map(variant_decoder_func(type_:, variant: _, opts: ctx.opts, is_multi_variant:))
        }
      ]
      |> list.map(g.Definition([], _))
    }
  }
}

fn gen_json_encoders(
  type_: deriv.Type,
  ctx: Context,
) -> List(g.Definition(g.Function)) {
  case type_ {
    deriv.TypeAlias(type_alias:) -> {
      let t = type_alias |> alias_to_type

      let type_ = t.type_ |> to_t
      // let type__ = t.type_ |> to_type_(publicity: t.publicity)

      let name =
        type_alias.name
        |> common.snake_case
        |> string.append(to: "encode_", suffix: _)

      g.Function(x,
        name:,
        publicity: t.publicity,
        parameters: [
          g.FunctionParameter(name: g.Named("value"), label: None, type_: Some(type_alias.aliased)),
          ..{
            type_alias.parameters
            |> list.map(fn(param) {
              g.FunctionParameter(name: g.Named("encode_" <> param), label: None, type_: Some(
                g.FunctionType(x,
                  parameters: [g.VariableType(x, name: param)],
                  return: g.NamedType(x, module: None, name: "Json", parameters: []),
                )
              ))
            })
          }
        ],
        return: Some(g.NamedType(x, module: None, name: "Json", parameters: [])),
        body: [
          encode_call(type_:, field: None, opts: ctx.opts, discard_value: False, inner: dict.new()) |> g.Expression,
        ],
      )
      |> list.wrap
      |> list.map(g.Definition([], _))
    }

    deriv.Type(type_:) -> {
      let is_multi_variant = type_ |> common.is_multi_variant

      let type_ = to_type(type_:, opts: ctx.opts)

      [
        type_encode_func(type_:, opts: ctx.opts, is_multi_variant:),
      ]
      |> list.map(g.Definition([], _))
    }
  }
}

fn gen_imports(
  type_: deriv.Type,
  ctx: Context,
) -> List(g.Import) {
  [
    decode_imports(type_:, ctx:),
    encode_imports(type_:, opts: ctx.deriv.opts),
    properties_imports(type_:, opts: ctx.deriv.opts),
  ]
  |> list.flatten
}

fn properties_imports(
  opts opts: List(String),
  type_ _type_: deriv.Type,
) -> List(g.Import) {
  use <- bool.guard(!{opts |> list.contains("properties")}, return: [])

  case opts |> list.contains("properties") {
    False -> []
    True -> [ common.dict_import_with_class() ]
  }
}

fn decode_imports(
  type_ type_: deriv.Type,
  ctx ctx: Context,
) -> List(g.Import) {
  use <- bool.guard(!{ctx.deriv.opts |> list.contains("decode")}, return: [])

  let standard = [
    common.import__(
      module: "gleam/dynamic/decode",
      as_: None,
      values: [],
      types: ["Decoder"],
    )
  ]

  let assert Ok(json_guard_re) = "^json\\s+guard" |> regexp.from_string

  let needs_util =
    case type_ {
      deriv.Type(type_:) -> [
        type_ |> common.are_any_fields_options,
        ctx.opts |> common.any_raw_field_options_match(json_guard_re),
      ]

      deriv.TypeAlias(type_alias: _) -> [
      ]
    }
    |> list.any(fn(b) {b})

  let util_import = {
    use <- bool.guard(!needs_util, return: [])

    [
      common.import__(
        module: "deriv/util",
        as_: Some("deriv"),
        values: [],
        types: [],
      )
    ]
  }

  standard
  |> list.append(util_import)
}

fn encode_imports(
  opts opts: List(String),
  type_ type_: deriv.Type,
) -> List(g.Import) {
  use <- bool.guard(!{opts |> list.contains("encode")}, return: [])

  let standard = [
    common.import__(
      module: "gleam/json",
      as_: None,
      values: [],
      types: ["Json"],
    )
  ]

  let needs_util_import =
    case type_ {
      deriv.Type(type_:) -> [
        type_ |> common.is_multi_variant,
        // type_ |> common.are_any_fields_options,
        type_ |> common.are_any_fields_non_string_basic_type_dict_keys,
      ]

      deriv.TypeAlias(type_alias:) -> [
        type_alias.aliased |> common.has_non_string_basic_type_dict_keys,
      ]
    }
    |> list.any(fn(bool) { bool })

  let util_import = {
    use <- bool.guard(!needs_util_import, return: [])

    [
      common.import__(
        module: "deriv/util",
        as_: Some("deriv"),
        values: [],
        types: [],
      )
    ]
  }

  standard
  |> list.append(util_import)
}

//

type Type {
  Type(
    publicity: g.Publicity,
    type_: g.Type,
    params: List(String),
    pascal_case: String,
    snake_case: String,
    variants: List(Variant),
  )
}

type Variant {
  Variant(
    pascal_case: String,
    snake_case: String,
    fields: List(Field),
  )
}

type Field {
  Field(
    gleam: String,
    json: List(String),
    type_: T,
    variant_pascal_case: String,
    type_pascal_case: String,
  )
}

type T {
  T(
    name: String,
    params: List(T),
  )
}

fn t_to_str(
  t t: T,
) -> String {
  let params =
    t.params
    |> list.map(t_to_str)
    |> string.join(", ")

  t.name <> "(" <> params <> ")"
}

fn to_glance_type(
  type_ type_: g.CustomType,
) -> g.Type {
  let parameters =
    type_.parameters
    |> list.map(fn(param) {
      g.VariableType(x, name: param)
    })

  g.NamedType(x,
    name: type_.name,
    module: None,
    parameters:,
  )
}

fn to_type(
  type_ type_: g.CustomType,
  opts opts: DerivFieldOpts,
) -> Type {
  Type(
    publicity: type_.publicity,
    type_: type_ |> to_glance_type,
    params: type_.parameters,
    pascal_case: type_.name,
    snake_case: type_.name |> common.snake_case,
    variants: {
      type_.variants
      |> list.map(to_decode_variant(
        type_:,
        variant: _,
        opts:,
      ))
    },
  )
}

fn to_decode_variant(
  type_ type_: g.CustomType,
  variant variant: g.Variant,
  opts opts: DerivFieldOpts,
) -> Variant {
  Variant(
    pascal_case: variant.name,
    snake_case: variant.name |> common.snake_case,
    fields: {
      variant.fields
      |> list.map(to_decode_field(
        type_:,
        variant:,
        field: _,
        opts:,
      ))
    },
  )
}
fn to_decode_field(
  type_ custom_type: g.CustomType,
  variant variant: g.Variant,
  field field: g.VariantField,
  opts opts: DerivFieldOpts,
) -> Field {
  case field {
    g.UnlabelledVariantField(..) -> {
      io.println("")
      io.println("`derive json` ISSUE WITH THIS TYPE:")
      io.println(string.inspect(custom_type))
      io.println("")
      io.println("`derive json` ON THIS FIELD:")
      io.println(string.inspect(field))
      io.println("")

      panic as {
        "`derive json` only understands variants with named fields, but encountered the type printed above"
      }
    }

    g.LabelledVariantField(label:, item: type_) -> {
      let field =
        Field(
          gleam: label,
          json: [label],
          type_: type_ |> to_t,
          type_pascal_case: custom_type.name,
          variant_pascal_case: variant.name,
        )


      let json =
        get_field_opt(field:, opts:, desc: "json named", matching: fn(opt) {
          case opt.strs {
            ["json", "named", path] ->
              Ok(path |> string.split("."))

            _ ->
              Error(Nil)
          }
        })
        |> result.unwrap([label])

      Field(..field, json:)
    }
  }
}

fn to_t(
  type_ type_: g.Type,
) -> T {
  case type_ {
    // g.VariableType(..) |
    g.TupleType(..) |
    g.FunctionType(..) |
    g.HoleType(..) -> {
      io.println("")
      io.println("`derive json` ISSUE WITH THIS TYPE:")
      io.println(string.inspect(type_))
      io.println("")

      panic as {
        "`derive json` only understands `glance.NamedType`s, but encountered the type printed above"
      }
    }

    g.VariableType(name:, ..) ->
      T(name:, params: [])

    g.NamedType(name:, parameters:, ..) ->
      T(
        name:,
        params: {
          parameters
          |> list.map(to_t(type_: _))
        },
      )
  }
}

fn get_field_opt(
  field field: Field,
  opts opts: DerivFieldOpts,
  desc desc: String,
  matching matching: fn(DerivFieldOpt) -> Result(t, Nil),
) -> Result(t, Nil) {
  common.get_field_opt(
    opts:,
    type_: field.type_pascal_case,
    variant: field.variant_pascal_case,
    field: field.gleam,
    err_msg: string.join([
      "`deriv` found multiple `",
      desc,
      "` opts for: ",
      field.type_pascal_case,
      " ",
      field.variant_pascal_case,
      ".",
      field.gleam,
    ], ""),
    matching:,
  )
}

fn get_field_opts(
  field field: Field,
  opts opts: DerivFieldOpts,
  matching matching: fn(List(String)) -> Result(t, Nil),
) -> List(t) {
  common.get_field_opts_(
    opts:,
    type_: field.type_pascal_case,
    variant: field.variant_pascal_case,
    field: field.gleam,
  )
  |> list.filter_map(fn(dfo) {
    matching(dfo.strs)
  })
}

// DECODER FUNC GEN

fn type_decoder_func(
  type_ type_: Type,
) -> g.Function {
  case type_.variants {
    [variant, ..variants] ->
      type_decoder_func_(type_:, variant:, variants:)

    _ ->
      panic as { "`derive json decode` doesn't know what to do for types with no variants" }
  }
}

fn decoder_func_params_for_var_types(
  params params: List(String),
) -> List(g.FunctionParameter) {
  params
  |> list.filter(fn(param) {
    param
    |> string.first
    |> result.map(fn(str) {
      string.lowercase(str) == str
    })
    |> result.unwrap(False)
  })
  |> list.map(fn(var_param) {
    g.FunctionParameter(
      name: { "decoder_" <> var_param } |> g.Named,
      label: None,
      type_: Some(g.NamedType(x, module: None, name: "Decoder", parameters: [
        g.NamedType(x, module: None, name: var_param, parameters: []),
      ])),
    )
  })
}

fn type_decoder_func_(
  type_ type_: Type,
  variant variant: Variant,
  variants variants: List(Variant),
) -> g.Function {
  let call_decoder = fn(variant) {
    variant
    |> variant_decoder_name(type_:, variant: _)
    |> term
    |> call(
      type_.params
      |> list.map(fn(param) {
        term("decoder_" <> param)
      })
    )
  }

  g.Function(x,
    name: "decoder_" <> type_.snake_case,
    publicity: type_.publicity,
    parameters: decoder_func_params_for_var_types(type_.params),
    return: Some(decoder_return_type(type_:)),
    body: {
      "decode"
      |> dot("one_of")
      |> call([
        variant |> call_decoder,
        variants |> list.map(call_decoder) |> list,
      ])
      |> g.Expression
      |> list.wrap
    },
  )
}

fn variant_decoder_name(
  type_ type_: Type,
  variant variant: Variant,
) -> String {
  "decoder_" <> type_.snake_case <> "_" <> variant.snake_case
}

fn decoder_return_type(
  type_ type_: Type,
) -> g.Type {
  g.NamedType(x, module: None,
    name: "Decoder",
    parameters: [type_.type_],
  )
}

fn variant_decoder_func(
  type_ type_: Type,
  variant variant: Variant,
  opts opts: DerivFieldOpts,
  is_multi_variant is_multi_variant: Bool,
) -> g.Function {
  let use_lines =
    variant.fields
    |> list.map(use_decode_field_line(field: _, opts:))

  let use_lines =
    // pass from above
    case is_multi_variant {
      False -> use_lines
      True -> [
        g.Use(x,
          patterns: [g.PatternDiscard(x, name: "deriv_var_constr") |> g.UsePattern(pattern: _, annotation: None)],
          function: {
            "decode" |> dot("field") |> call([
              string(deriv_variant_json_key),
              "deriv" |> dot("is") |> call([
                string(variant.pascal_case),
              ]),
            ])
          },
        ),
        ..use_lines
      ]
    }

  g.Function(x,
    name: variant_decoder_name(type_:, variant:),
    publicity: type_.publicity,
    parameters: decoder_func_params_for_var_types(type_.params),
    return: Some(decoder_return_type(type_:)),
    body: {
      use_lines
      |> list.append([decode_success(variant:)])
    },
  )
}

fn use_decode_field_line(
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Statement {
  let field_gleam_name =
    g.UsePattern(
      pattern: g.PatternVariable(x, field.gleam),
      annotation: None,
    )

  let decode_field_call =
    decode_field_call(field:, opts:)

  g.Use(x,
    patterns: [field_gleam_name],
    function: decode_field_call,
  )
}

fn decoder_call(
  type_ type_: T,
  field field: Option(Field),
  opts opts: DerivFieldOpts,
  inner inner: Dict(Int, String),
  top_level top_level: Bool,
) -> g.Expression {
  let type_name =
    type_.name |> common.snake_case

  let is_list_or_option =
    case top_level, field {
      True, Some(Field(type_:, ..)) ->
        case type_.name, type_.params {
          "List", [_] |
          "Option", [_] -> True

          _, _ -> False
        }

      _, _ ->
        False

    }

  let decoder_override =
    case field {
      None ->
        Error(Nil)

      Some(field) ->
        get_field_opt(field:, opts:, desc: "json decoder", matching: fn(opt) {
          case is_list_or_option, opt.strs {
            True, ["json", "decoder", "inner", decoder] ->
              Ok(decoder)

            _, ["json", "decoder", decoder] ->
              Ok(decoder)

            _, _ ->
              Error(Nil)
          }
        })
    }

  let decoder_guard =
    case field {
      None ->
        Error(Nil)

      Some(field) ->
        get_field_opt(field:, opts:, desc: "json guard", matching: fn(opt) {
          case field.type_, opt.strs {
            // T("Int" as type_, []), ["json", "guard", ..] |
            // T("Float" as type_, []), ["json", "guard", ..] |
            // T("Bool" as type_, []), ["json", "guard", ..] |
            T("String" as type_, []), ["json", "guard", ..] -> {
              let #(re, to_expr) =
                case type_ {
                  "String" -> {
                    let assert Ok(str_re) = "\"([^\"]+)\"$" |> regexp.from_string
                    #(str_re, fn(str) { Ok(g.String(x, str)) })
                  }

                  // "Int" -> {
                  //   let assert Ok(int_re) = "(\\d+)$" |> regexp.from_string

                  //   let to_expr = fn(str) {
                  //     str
                  //     |> int.parse
                  //     |> result.map(int.to_string)
                  //     |> result.map(g.Int(x, _))
                  //   }

                  //   #(int_re, to_expr)
                  // }

                  // "Float" -> {
                  //   let assert Ok(float_re) = "(((\\d+)[.])?\\d+)$" |> regexp.from_string

                  //   let to_expr = fn(str) {
                  //     str
                  //     |> float.parse
                  //     |> result.map(float.to_string)
                  //     |> result.map(g.Float(x, _))
                  //   }

                  //   #(float_re, to_expr)
                  // }

                  // "Bool" -> {
                  //   let assert Ok(bool_re) = "(True|False)$" |> regexp.from_string

                  //   let to_expr = fn(str) {
                  //     case str {
                  //       "True" -> Ok("True" |> term)
                  //       "False" -> Ok("False" |> term)
                  //       _ -> Error(Nil)
                  //     }
                  //   }

                  //   #(bool_re, to_expr)
                  // }

                  _ -> panic as { "`json guard` parse unimplemented for: " <> type_ }
                }

              case opt.raw |> regexp.scan(re, _) {
                [regexp.Match(_, [Some(str)])] -> Ok(str)
                _ -> Error(Nil)
              }
              |> result.try(to_expr)
              |> result.lazy_unwrap(fn() {
                panic as { "`json guard` failed to parse `" <> type_ <> "` out of: " <> opt.raw}
              })
              |> Ok
            }

            _ , ["json", "guard", ..] ->
              panic as { "`json guard` invalid type: " <> string.inspect(field) }

            _, _ ->
              Error(Nil)
          }
        })
    }

  let decoder_call = fn(type_) {
    decoder_call(field:, opts:, type_:, inner:, top_level: False)
  }

  decoder_override
  |> result.map(fn(decoder_name) {
    decoder_name |> term |> call([])
  })
  |> result.lazy_unwrap(fn() {
    case type_.name, type_.params {
      "Dict", [key, val] -> {
        let key_decoder =
          case key.name, key.params {
            "String", [] ->
              "decode" |> dot("string")

            "Int" as type_name, [] |
            "Float" as type_name, [] |
            "Bool" as type_name, [] -> {
              let func_name =
                type_name
                |> common.snake_case
                |> string.append(to: "decoder_", suffix: _)
                |> string.append(to: _, suffix: "_string")

              "deriv" |> dot(func_name) |> call([])
            }

            type_name , [] ->
              { "decoder_" <> type_name } |> term |> call([])

            _ , _ ->
              panic as {
                "`json` doesn't know how to handle parameterized `Dict` keys, got: " <> t_to_str(key)
              }
          }

        "decode" |> dot("dict")
        |> call([
          key_decoder,
          decoder_call(val),
        ])
      }

      "List", [T(name: "Option", params:[_]) as option_type] ->
        "decode" |> dot("list")
        |> call([
          decoder_call(option_type),
        ])

      "Option", [T(name: "List", params:[_]) as list_type] ->
        "decode" |> dot("optional")
        |> call([
          decoder_call(list_type),
        ])

      "Option", [inner_type] ->
        "decode" |> dot("optional") |> call([decoder_call(inner_type)])

      "List", [inner_type] ->
        "decode" |> dot("list") |> call([decoder_call(inner_type)])

      "String", [] |
      "Int", [] |
      "Float", [] |
      "Bool", [] -> {
        let decode = "decode" |> dot(type_name |> common.snake_case)

        case decoder_guard {
          Error(Nil) -> decode
          Ok(check) -> decode |> pipe("deriv" |> dot("decoder_guard") |> call([check]))
        }
      }

      type_name, params -> {
        let decoder = { "decoder_" <> type_name |> common.snake_case } |> term
        let is_variable_type = string.lowercase(type_name) == type_name

        case is_variable_type {
          True ->
            decoder

          False -> {
            let params =
              case params, dict.get(inner, 0) {
                [_param], Ok(decoder_name) -> [
                  decoder_name |> term |> call([]),
                ]

                _, _ ->
                  params |> list.map(decoder_call)
              }

            decoder |> call(params)
          }
        }
      }
    }
  })
}

type EncDec {
  Enc
  Dec
}

fn enc_dec_str(x: EncDec) -> String {
  case x {
    Enc -> "encode"
    Dec -> "decoder"
  }
}

fn get_inner(
  field f: Field,
  kind kind: EncDec,
  opts opts: DerivFieldOpts,
) -> Dict(Int, String) {
  let kind = enc_dec_str(kind)

  let opts =
    get_field_opts(field: f, opts:, matching: fn(strs) {
      case strs {
        ["json", enc_dec, "inner", func] if enc_dec == kind -> Ok(#(0, func))
        _ -> Error(Nil)
      }
    })

  let dict = opts |> dict.from_list

  case dict.size(dict) < list.length(opts) {
    True -> panic as { "`json " <> kind <> " inner` collisions for: " <> string.inspect(f)}
    False -> dict
  }
}

fn decode_field_call(
  field f: Field,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  let inner =
    get_inner(field: f, opts:, kind: Dec)

  let decoder_call = fn(type_) {
    decoder_call(type_:, field: Some(f), opts:, inner:, top_level: True)
  }

  case f.json, f.type_.name, f.type_.params {
    [], _, _ -> {
      panic as { "`derive decode` needs a JSON property, but found none for: " <> string.inspect(f) }
    }

    [prop], "Dict", [_key, val] -> {
      "decode" |> dot("field") |> call([
        string(prop),
        "decode" |> dot("dict") |> call([
          "decode" |> dot("string"),
          decoder_call(val),
        ])
      ])
    }
    // [_prop1, _prop2, ..] as props, "List", [T(params: [], ..)] -> {
    //   "decode" |> dot("subfield") |> call([
    //     list(props |> list.map(string)),
    //     decoder_call(field:, opts:, type_: field.type_),
    //   ])
    // }

    [prop], "List", [T(name: "Option", params: [_])] -> {
      "decode" |> dot("field") |> call([
        string(prop),
        decoder_call(f.type_),
      ])
    }
    [prop], "List", [_] -> {
      "decode" |> dot("optional_field") |> call([
        string(prop),
        list([]),
        decoder_call(f.type_),
      ])
    }
    [_prop1, _prop2, ..] as props, "List", [T(params: [], ..)] -> {
      "decode" |> dot("subfield") |> call([
        list(props |> list.map(string)),
        decoder_call(f.type_),
      ])
    }

    [prop], "Option", [T(name: "List", params: [_]) as t] -> {
      "decode" |> dot("optional_field") |> call([
        string(prop),
        "deriv" |> dot("none"),
        "decode" |> dot("optional") |> call([
          decoder_call(t),
        ]),
      ])
    }
    [prop], "Option", [_] -> {
      "decode" |> dot("optional_field") |> call([
        string(prop),
        "deriv" |> dot("none"),
        decoder_call(f.type_),
      ])
    }
    [_prop1, _prop2, ..] as props, "Option", [T(name: "List", params: [_])] -> {
      "deriv" |> dot("decode_optional_subfield") |> call([
        list(props |> list.map(string)),
        "deriv" |> dot("none"),
        decoder_call(f.type_),
      ])
    }
    [_prop1, _prop2, ..] as props, "Option", [_] -> {
      "decode" |> dot("then") |> call([
        "decode" |> dot("at") |> call([
          list(props |> list.map(string)),
          decoder_call(f.type_),
        ]),
      ])
    }

    [prop], _, _ -> {
      "decode" |> dot("field") |> call([
        string(prop),
        decoder_call(f.type_),
      ])
    }

    [_prop1, _prop2, ..] as props, _, _ -> {
      "decode" |> dot("subfield") |> call([
        list(props |> list.map(string)),
        decoder_call(f.type_),
      ])
    }
  }
}

fn decode_success(
  variant variant: Variant,
) -> g.Statement {
  let constr_call =
    variant
    |> variant_decoder_constructor

  "decode"
  |> dot("success")
  |> call([constr_call])
  |> g.Expression
}

fn variant_decoder_constructor(
  variant variant: Variant,
) -> g.Expression {
  let constr =
    variant.pascal_case
    |> term

  let arguments =
    variant.fields
    |> list.map(fn(field) {
      g.ShorthandField(field.gleam)
    })

  case variant.fields {
    [] -> constr
    _ -> constr |> call_(arguments:)
  }
}

// ENCODE FUNC GEN

fn type_encode_func(
  type_ type_: Type,
  opts opts: DerivFieldOpts,
  is_multi_variant is_multi_variant: Bool,
) -> g.Function {
  let encode_func_params =
    type_.params
    |> list.filter(fn(param) {
      string.lowercase(param) == param
    })
    |> list.map(fn(param) {
      g.FunctionParameter(
        label: None,
        name: { "encode_" <> param } |> g.Named,
        type_: Some(
          g.FunctionType(x,
            parameters: [
              g.VariableType(x, name: param),
            ],
            return: g.NamedType(x, module: None, name: "Json", parameters: []),
          )
        ),
      )
    })

  g.Function(x,
    name: "encode_" <> type_.snake_case,
    publicity: type_.publicity,
    parameters: [
      g.FunctionParameter(
        label: None,
        name: g.Named("value"),
        type_: Some(type_.type_),
      ),
      ..encode_func_params,
    ],
    return: Some(g.NamedType(x, name: "Json", module: None, parameters: [])),
    body: {
      g.Case(x, subjects: ["value" |> term], clauses: {
        type_.variants
        |> list.map(variant_encode_case_clause(variant: _, type_:, opts:, is_multi_variant:))
      })
      |> g.Expression
      |> list.wrap
    },
  )
}

fn to_json_object_tuples(
  encodes encodes: List(Encode),
) -> List(g.Expression) {
  encodes
  |> list.sort(fn(a, b) { string.compare(a.prop, b.prop) })
  |> list.map(fn(encode) {
    case encode {
      Encode(prop:, func: encode_func) ->
        tuple([
          string(prop),
          encode_func,
        ])

      Path(prop:, children:) ->
        tuple([
          string(prop),
          "json" |> dot("object") |> call([list(
            children |> to_json_object_tuples
          )])
        ])
    }
  })
}

fn variant_encode_case_clause(
  type_ _type_: Type,
  variant variant: Variant,
  opts opts: DerivFieldOpts,
  is_multi_variant is_multi_variant: Bool,
) -> g.Clause {
  let field_tuples =
    variant.fields
    |> list.fold([], fn(acc, field) {
      acc
      |> add_encode(
        path: field.json,
        encode: {
          let inner = get_inner(field:, opts:, kind: Enc)
          encode_call(type_: field.type_, field: Some(field), opts:, discard_value: False, inner:)
        },
      )
    })
    |> to_json_object_tuples

  let field_tuples =
    case is_multi_variant {
      False -> field_tuples
      True -> [
        tuple([
          string(deriv_variant_json_key),
          "json" |> dot("string") |> call([string(variant.pascal_case)])
        ]),
        .. field_tuples
      ]
    }

  let pattern =
    case variant.fields {
      [] ->
        g.PatternVariant(x,
          module: None,
          constructor: variant.pascal_case,
          arguments: [],
          with_spread: False,
        )

      _ ->
        g.PatternAssignment(x,
          attern: g.PatternVariant(x,
            module: None,
            constructor: variant.pascal_case,
            arguments: [],
            with_spread: True,
          ),
          name: "value",
        )
    }

  g.Clause(
    patterns: [[
      pattern,
    ]],
    guard: None,
    body: {
      "json" |> dot("object") |> call([list(
        field_tuples,
      )])
    }
  )
}

type Encode {
  Path(prop: String, children: List(Encode))
  Encode(prop: String, func: g.Expression)
}

fn add_encode(
  siblings siblings: List(Encode),
  path path: List(String),
  encode func: g.Expression,
) -> List(Encode) {
  case path {
    [prop, ..path] ->
      case path, siblings |> get_encode_for(prop:) {
        [], Error(Nil) ->
          Encode(prop:, func:)
          |> list.wrap
          |> list.append(siblings, _)

        path, Error(Nil) ->
          [
            Path(prop:, children: [] |> add_encode(path:, encode: func)),
            ..siblings
          ]

        path, Ok(#(Path(children:, ..) as p, siblings)) ->
          [
            Path(..p, children: children |> add_encode(path:, encode: func)),
            ..siblings
          ]

        [], Ok(#(Encode(..), _)) |
        _path, Ok(#(Encode(..), _)) ->
          panic as "`add_encode` property collision" // TODO context
      }

    [] ->
      panic as "`add_encode` empty `path`"
  }
}

fn get_encode_for(
  encodes es: List(Encode),
  prop prop: String,
) -> Result(#(Encode, List(Encode)), Nil) {
  es
  |> list.find(fn(e) {
    e.prop == prop
  })
  |> result.map(fn(encode) {
    es
    |> list.filter(fn(e) {
      encode != e
    })
    |> pair.new(encode, _)
  })
}

fn encode_call(
  type_ type_: T,
  discard_value discard_value: Bool,
  field field: Option(Field),
  opts opts: DerivFieldOpts,
  inner inner: Dict(Int, String),
) -> g.Expression {
  let encode_override =
    case field {
      None ->
        Error(Nil)

      Some(field) ->
        get_field_opt(field:, opts:, desc: "json encode", matching: fn(opt) {
          case opt.strs {
            ["json", "encode", encode] ->
              Ok(encode)

            _ ->
              Error(Nil)
          }
        })
    }

  let value =
    case discard_value, field {
      True , _ ->
        "_" |> term

      _, Some(field) ->
        "value" |> dot(field.gleam)

      _, None ->
        "value" |> term
    }

  encode_override
  |> result.map(fn(encode_name) {
    encode_name |> term |> call([value])
  })
  |> result.lazy_unwrap(fn() {
    encode_call_(type_:, field:, value_arg: True, inner:)
  })
}

fn encode_call_(
  type_ type_: T,
  field field: Option(Field),
  value_arg value_arg: Bool,
  inner inner: Dict(Int, String),
) -> g.Expression {
  let value =
    case value_arg, field {
      True, Some(field) -> "value" |> dot(field.gleam)
      True, None -> "value" |> term
      False, _ -> "_" |> term
    }

  let encode_call_ = fn(type_) {
    encode_call_(type_:, field:, value_arg: False, inner:)
  }

  case type_.name, type_.params {
    "Dict", [key, val] -> {
      let encode_key =
        case key.name, key.params {
          "String", [] ->
            identity_func("str")

          "Int", [] | "Float", [] | "Bool", [] -> {
            let func_name =
              key.name
              |> common.snake_case
              |> string.append(to: _, suffix: "_to_string")

            "deriv" |> dot(func_name)
          }

          _, [] ->
            { "encode_" <> { key.name |> common.snake_case } } |> term

          _, _ ->
            panic as {
              "`json` doesn't know how to handle parameterized `Dict` keys, got: " <> t_to_str(key)
            }
        }

      "json" |> dot("dict") |> call([
        value,
        encode_key,
        encode_call_(val),
      ])
    }

    "Option", [inner_type] ->
      "json" |> dot("nullable") |> call([
        value,
        encode_call_(inner_type),
      ])

    "List", [inner_type] ->
      "json" |> dot("array") |> call([
        value,
        encode_call_(inner_type ),
      ])

    "String", [] |
    "Int", [] |
    "Float", [] |
    "Bool", [] ->
      json_encode_func(type_:,) |> call([
        value,
      ])

    _, [] ->
      { "encode_" <> type_.name |> common.snake_case } |> term |> call([
        value,
      ])

    _, params ->
      { "encode_" <> type_.name |> common.snake_case } |> term |> call([
        value,
        ..{
          params
          |> list.index_map(fn(param, idx) {
            case dict.get(inner, idx), param.params {
              Ok(encode_func_name), _ -> encode_func_name |> term
              _, [] -> json_encode_func(param)
              _, _ -> encode_call_(param)
            }
          })
        }
      ])
  }
}

fn json_encode_func(
  type_ type_: T,
) -> g.Expression {
  case type_.name, type_.params {
    "String", [] |
    "Int", [] |
    "Float", [] |
    "Bool", [] ->
      "json" |> dot(type_.name |> common.snake_case)

    _, _ ->
      { "encode_" <> type_.name |> common.snake_case } |> term
  }
}
