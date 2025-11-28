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
import deriv/internal/glance.{x, string, list, term, call, call_, dot, dot_, tuple, pipe, fn_, identity_func} as _
import deriv/internal/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type DerivFieldOpt, type ModuleReader} as deriv
import deriv/internal/opts
import deriv/internal/common.{gtype}

const deriv_variant_json_key = "_var"

type Context {
  Context(
    deriv: Derivation,
    derivs: List(Derivation),
    opts: DerivFieldOpts,
    file: File,
    module_reader: ModuleReader,
    type_: Type,
  )
}

fn conv(
  ctx ctx: deriv.Context,
  type_ type_: deriv.Type,
) -> Context {
  let type_ =
    case type_ {
      deriv.TypeAlias(type_alias:) -> alias_to_type(type_alias:)
      deriv.Type(type_:) -> to_type(type_:, opts: ctx.opts)
    }

  let deriv.Context(
    deriv:,
    opts:,
    file:,
    module_reader:,
  ) = ctx

  Context(
    derivs: [],
    deriv:,
    opts:,
    file:,
    module_reader:,
    type_:,
  )
}

pub fn gen(
  type_: deriv.Type,
  ctx: deriv.Context,
) -> Gen {
  let ctx = conv(ctx:, type_:)
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
          decoder_call(type_: to_t(t.type_), field: None, ctx:, inner: None, top_level: True) |> g.Expression,
        ],
      )
      |> list.wrap
      |> list.map(g.Definition([], _))
    }

    deriv.Type(type_:) -> {
      let is_multi_variant = type_ |> common.is_multi_variant

      let type_ = to_type(type_:, opts: ctx.opts)

      [
        type_decoder_func(type_:, ctx:),
        ..{
          type_.variants
          |> list.map(variant_decoder_func(type_:, variant: _, ctx:, is_multi_variant:))
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
          encode_call(type_:, field: None, ctx:, discard_value: False, inner: None) |> g.Expression,
        ],
      )
      |> list.wrap
      |> list.map(g.Definition([], _))
    }

    deriv.Type(type_:) -> {
      let is_multi_variant = type_ |> common.is_multi_variant

      let type_ = to_type(type_:, opts: ctx.opts)

      [
        type_encode_func(type_:, ctx:, is_multi_variant:),
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
  ctx ctx: Context,
) -> g.Function {
  case type_.variants {
    [variant, ..variants] ->
      type_decoder_func_(type_:, variant:, variants:, ctx:)

    _ ->
      panic as { "`derive json decode` doesn't know what to do for types with no variants" }
  }
}

fn contains_type_var_(
  type_ type_: g.Type,
  var_name var_name: String,
) -> Bool {
  // TODO refactor out to recurse
  case type_ {
    g.HoleType(..) -> {
      False
    }

    g.VariableType(name:, ..) -> {
      name == var_name
    }

    g.NamedType(..) as nt -> {
      nt.parameters |> list.any(contains_type_var_(type_: _, var_name:))
    }

    g.FunctionType(..) as ft -> {
      [
        ft.return,
        ..ft.parameters,
      ]
      |> list.any(contains_type_var_(type_: _, var_name:))
    }

    g.TupleType(..) as tt ->  {
      tt.elements |> list.any(contains_type_var_(type_: _, var_name:))
    }
  }
}

fn is_phantom_type_(
  type_ type_: g.CustomType,
  param_at idx: Int,
) -> Bool {
  type_.parameters
  |> list.index_map(fn(param, i) {
    use <- bool.guard(i != idx, Error(Nil))
    Ok(param)
  })
  |> result.values
  |> list.first
  |> result.map(fn(var_name) {
    use <- bool.guard(var_name |> common.starts_with_uppercase, False)

    use variant <- list.any(type_.variants)
    use field <- list.any(variant.fields)

    contains_type_var_(var_name:, type_: field.item)
  })
  |> result.unwrap(False)
}

fn is_not_phantom_type(
  type_name type_name: String,
  type_ type_: Type,
) -> Bool {
  use <- bool.guard(common.starts_with_uppercase(type_name), False)
  use <- bool.guard(!{ type_.params |> list.contains(type_name) }, False)

  use variant <- list.any(type_.variants)
  use field <- list.any(variant.fields)

  use <- bool.guard(common.starts_with_uppercase(field.type_.name), False)

  contains_type_var(type_name:, type_: field.type_)
}

fn contains_type_var(
  type_name type_name: String,
  type_ type_: T,
) -> Bool {
  type_.name == type_name || list.any(type_.params, contains_type_var(type_name:, type_: _))
}

fn reject_phantom_types(
  type_name type_name: String,
  params params: List(String),
  ctx ctx: Context,
) -> List(String) {
  let ident = ctx.file.module <> "." <> type_name

  case common.fetch_custom_type(ident:, read_module: ctx.module_reader) {
    Error(_err) ->
      params

    Ok(#(_, g.Definition(definition: type_, ..))) -> {
      params
      // |> list.filter(fn(param) {
      //   ! is_phantom_type_(type_name: param, param_at: idx, ctx:)
      // })
      |> list.index_map(fn(param, idx) {
        case is_phantom_type_(type_:, param_at: idx) {
          False -> Error(Nil)
          True -> Ok(param)
        }
      })
      |> result.values
    }
  }
}

fn reject_phantom_types_(
  type_name type_name: String,
  params params: List(T),
  ctx ctx: Context,
) -> List(T) {
  let ident = ctx.file.module <> "." <> type_name

  case common.fetch_custom_type(ident:, read_module: ctx.module_reader) {
    Error(_err) ->
      params

    Ok(#(_, g.Definition(definition: type_, ..))) -> {
      params
      // |> list.filter(fn(param) {
      //   ! is_phantom_type_(type_name: param, param_at: idx, ctx:)
      // })
      |> list.index_map(fn(param, idx) {
        case is_phantom_type_(type_:, param_at: idx) {
          False -> Error(Nil)
          True -> Ok(param)
        }
      })
      |> result.values
    }
  }
}

fn decoder_func_params_for_var_types(
  type_ type_: Type,
  ctx ctx: Context,
) -> List(g.FunctionParameter) {
  type_.params
  |> list.filter(fn(param) {
    param
    |> string.first
    |> result.map(fn(str) {
      string.lowercase(str) == str
    })
    |> result.unwrap(False)
  })
  |> reject_phantom_types(type_name: type_.pascal_case, ctx:)
  // |> list.index_map(fn(param, idx) {
  //   case is_phantom_type_(type_name: type_.pascal_case, param_at: idx, ctx:) {
  //     False -> Error(Nil)
  //     True -> Ok(param)
  //   }
  // })
  // |> result.values
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
  ctx ctx: Context,
) -> g.Function {
  let call_decoder = fn(variant) {
    variant
    |> variant_decoder_name(type_:, variant: _)
    |> term
    |> call(
      type_.params
      |> reject_phantom_types(type_name: type_.pascal_case, ctx:)
      // |> reject_phantom_types(type_:)
      // |> list.index_map(fn(param, idx) {
      //   case is_phantom_type_(type_name: type_.pascal_case, param_at: idx, ctx:) {
      //     False -> Error(Nil)
      //     True -> Ok(param)
      //   }
      // })
      // |> result.values
      |> list.map(fn(param) {
        term("decoder_" <> param)
      })
    )
  }

  g.Function(x,
    name: "decoder_" <> type_.snake_case,
    publicity: type_.publicity,
    parameters: decoder_func_params_for_var_types(type_:, ctx:),
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

fn variant_json_key(
  opts opts: DerivFieldOpts,
  type_ type_: Type
) -> Option(String) {
  opts
  |> dict.get(deriv.DerivField(type_: type_.pascal_case, variant: "", field: ""))
  |> result.try(fn(opts) {
    opts
    |> list.find_map(fn(opt) {
      case opt.strs {
        ["json", "variant", "key", "None"] ->
          Ok(None)

        ["json", "variant", "key", ..] -> {
          let assert Ok(re) =
            "Some[(]\"([^\"]+)\"[)]$" |> regexp.from_string

          case opt.raw |> string.trim |> regexp.scan(re, _) {
            [regexp.Match(_, [Some(key)])] ->
              Ok(Some(key))

            _ ->
              Error(Nil)
          }
        }

        _ ->
          Error(Nil)
      }
    })
  })
  |> result.unwrap(Some(deriv_variant_json_key))
}

fn variant_decoder_func(
  type_ type_: Type,
  variant variant: Variant,
  ctx ctx: Context,
  is_multi_variant is_multi_variant: Bool,
) -> g.Function {
  let use_lines =
    variant.fields
    |> list.map(use_decode_field_line(field: _, ctx:))

  let use_lines =
    // pass from above
    case is_multi_variant {
      False -> use_lines
      True ->
        case variant_json_key(opts: ctx.opts, type_:) {
          Some(variant_json_key) ->
            [
              g.Use(x,
                patterns: [g.PatternDiscard(x, name: "deriv_var_constr") |> g.UsePattern(pattern: _, annotation: None)],
                function: {
                  "decode" |> dot("field") |> call([
                    string(variant_json_key),
                    "deriv" |> dot("is") |> call([
                      string(variant.pascal_case),
                    ]),
                  ])
                },
              ),
              ..use_lines
            ]

          _ ->
            use_lines
        }
    }

  g.Function(x,
    name: variant_decoder_name(type_:, variant:),
    publicity: type_.publicity,
    parameters: decoder_func_params_for_var_types(type_:, ctx:),
    return: Some(decoder_return_type(type_:)),
    body: {
      use_lines
      |> list.append([decode_success(variant:)])
    },
  )
}

fn use_decode_field_line(
  field field: Field,
  ctx ctx: Context,
) -> g.Statement {
  let field_gleam_name =
    g.UsePattern(
      pattern: g.PatternVariable(x, field.gleam),
      annotation: None,
    )

  let decode_field_call =
    decode_field_call(field:, ctx:)

  g.Use(x,
    patterns: [field_gleam_name],
    function: decode_field_call,
  )
}

fn build_newtype(
  type_ type_: T,
  field field: Option(Field),
  ctx ctx: Context,
) -> Result(#(deriv.Newtype, Option(opts.Inner)), Nil) {
  let #(type_, inner) =
    case type_.name, type_.params {
      "Option", [type_] -> #(type_, Some(opts.Option))
      "List", [type_] -> #(type_, Some(opts.List))
      _, _ -> #(type_, None)
    }

  build_newtype_(type_:, field:, ctx:)
  |> result.map(pair.new(_, inner))
}

fn build_newtype_(
  type_ type_: T,
  field field: Option(Field),
  ctx ctx: Context,
) -> Result(deriv.Newtype, Nil) {
  let is_newtype =
    case field {
      None ->
        Error(Nil)

      Some(field) ->
        get_field_opt(field:, opts: ctx.opts, desc: "json newtype", matching: fn(opt) {
          case opt.strs {
            ["newtype"] -> Ok(Nil)
            _ -> Error(Nil)
          }
        })
    }
    |> result.is_ok

  let ident = ctx.file.module <> "." <> type_.name

  case is_newtype {
    False -> Error(Nil)
    True ->
      common.fetch_newtype(ident, ctx.module_reader)
      |> result.map_error(fn(err) {
        panic as { "`json` failed to look up newtype " <> ident <> " " <> string.inspect(err) }
      })
  }
}

fn decoder_call(
  type_ type_: T,
  field field: Option(Field),
  ctx ctx: Context,
  inner inner: Option(String),
  top_level top_level: Bool,
) -> g.Expression {
  let opts = ctx.opts

  let type_name =
    type_.name |> common.snake_case

  let list_or_option =
    case top_level, field {
      True, Some(Field(type_:, ..)) ->
        case type_.name, type_.params {
          "List", [_] -> Some(opts.List)
          "Option", [_] -> Some(opts.Option)

          _, _ ->  None
        }

      _, _ ->
        None
    }

  let is_list_or_option =
    list_or_option |> option.is_some

  let decoder_override =
    case field {
      None ->
        Error(Nil)

      Some(field) ->
        get_field_opt(field:, opts:, desc: "json decoder", matching: fn(opt) {
          case is_list_or_option, opt.strs {
            True, ["json", "decoder", "inner", decoder] ->
              Ok(#(decoder, True))

            _, ["json", "decoder", decoder] ->
              Ok(#(decoder, False))

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

  case build_newtype(type_:, field:, ctx:) {
    Ok(#(newtype, inner)) -> {
      case newtype.wrapping {
        g.NamedType(name: wrapped_type, parameters: [], ..) -> {
          let decoder =
            case wrapped_type {
              "String" -> "decode" |> dot("string")
              "Int" -> "decode" |> dot("int")
              "Float" -> "decode" |> dot("float")
              "Bool" -> "decode" |> dot("bool")
               _ -> panic as {
                "`json` currently only supports basic type `newtype`s"
              }
            }

            let decoder =
              decoder |> pipe("decode" |> dot("map") |> call([newtype.constr |> term]))

            case inner {
              None ->
                decoder

              Some(opts.Option) ->
                decoder |> pipe("decode" |> dot("optional"))

              Some(opts.List) ->
                decoder |> pipe("decode" |> dot("list"))
            }
          }


        _ -> panic as {
          "`json` currently only supports `NamedType`s"
        }
      }
    }

    Error(Nil) -> {
      let decoder_call = fn(type_) {
        decoder_call(field:, ctx:, type_:, inner:, top_level: False)
      }

      decoder_override
      |> result.map(fn(t) {
        let #(decoder_name, inner) = t

        let decoder =
            decoder_name |> term |> call([])

        case inner, list_or_option {
          True, Some(opts.Option) ->
            "decode" |> dot("optional") |> call([decoder])

          True, Some(opts.List) ->
            "decode" |> dot("list") |> call([decoder])

          _, _ ->
            decoder
        }
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
                  case params, inner {
                    [_param], Some(decoder_name) -> [
                      decoder_name |> term |> call([]),
                    ]

                    _, _ -> {
                      params
                      |> reject_phantom_types_(type_name: type_.name, ctx:)
                      // |> list.index_map(fn(param, idx) {
                      //   case is_phantom_type_(type_name: type_.name, param_at: idx, ctx:) {
                      //     False -> Error(Nil)
                      //     True -> Ok(param)
                      //   }
                      // })
                      // |> result.values
                      |> list.map(decoder_call)
                    }
                  }

                decoder |> call(params)
              }
            }
          }
        }
      })
    }
  }
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
) -> Option(String) {
  let kind = enc_dec_str(kind)

  let opts =
    get_field_opts(field: f, opts:, matching: fn(strs) {
      case strs {
        ["json", enc_dec, "inner", func] if enc_dec == kind -> Ok(#(0, func))
        _ -> Error(Nil)
      }
    })

  let dict = opts |> dict.from_list

  case dict |> dict.to_list {
    [] -> None
    [#(_, func)] -> Some(func)
    _ -> panic as { "`json " <> kind <> " inner` collisions for: " <> string.inspect(f)}
  }
}

fn decode_field_call(
  field f: Field,
  ctx ctx: Context,
) -> g.Expression {
  let inner =
    get_inner(field: f, opts: ctx.opts, kind: Dec)

  let decoder_call = fn(type_) {
    decoder_call(type_:, field: Some(f), ctx:, inner:, top_level: True)
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
      "deriv" |> dot("decode_optional_subfield") |> call([
        list(props |> list.map(string)),
        "deriv" |> dot("none"),
        decoder_call(f.type_),
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
  ctx ctx: Context,
  is_multi_variant is_multi_variant: Bool,
) -> g.Function {
  let encode_func_params =
    type_.params
    |> list.filter(fn(param) {
      string.lowercase(param) == param
    })
    |> reject_phantom_types(type_name: type_.pascal_case, ctx:)
    // |> list.index_map(fn(param, idx) {
    //   case is_phantom_type_(type_name: type_.pascal_case, param_at: idx, ctx:) {
    //     False -> Error(Nil)
    //     True -> Ok(param)
    //   }
    // })
    // |> result.values
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
        |> list.map(variant_encode_case_clause(variant: _, type_:, ctx:, is_multi_variant:))
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
  type_ type_: Type,
  variant variant: Variant,
  ctx ctx: Context,
  is_multi_variant is_multi_variant: Bool,
) -> g.Clause {
  let field_tuples =
    variant.fields
    |> list.fold([], fn(acc, field) {
      acc
      |> add_encode(
        path: field.json,
        encode: {
          let inner = get_inner(field:, opts: ctx.opts, kind: Enc)
          encode_call(type_: field.type_, field: Some(field), ctx:, discard_value: False, inner:)
        },
      )
    })
    |> to_json_object_tuples

  let field_tuples =
    case is_multi_variant {
      False -> field_tuples
      True ->
        case variant_json_key(opts: ctx.opts, type_:) {
          Some(variant_json_key) ->
            [
              tuple([
                string(variant_json_key),
                "json" |> dot("string") |> call([string(variant.pascal_case)])
              ]),
              .. field_tuples
            ]

          _ ->
            field_tuples
        }
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
  ctx ctx: Context,
  inner inner: Option(String),
) -> g.Expression {
  let value =
    case discard_value, field {
      True , _ ->
        "_" |> term

      _, Some(field) ->
        "value" |> dot(field.gleam)

      _, None ->
        "value" |> term
    }

  case build_newtype(type_:, field:, ctx:) {
    Ok(#(newtype, inner)) -> {
      case newtype.wrapping {
        g.NamedType(name: wrapped_type, parameters: [], ..) -> {
          let encode_func =
            case wrapped_type {
              "String" -> "json" |> dot("string")
              "Int" -> "json" |> dot("int")
              "Float" -> "json" |> dot("float")
              "Bool" -> "json" |> dot("bool")
               _ -> panic as {
                "`json` currently only supports basic type `newtype`s"
              }
            }

            case inner {
              None ->
                encode_func |> call([
                  value |> dot_(newtype.field_access)
                ])

              Some(opts.Option) -> {
                let map_newtype =
                  "option" |> dot("map") |> call([
                    fn_(["x"], [
                      "x" |> dot(newtype.field_access) |> g.Expression,
                    ]),
                  ])

                "json" |> dot("nullable") |> call([
                  value |> pipe(map_newtype),
                  encode_func,
                ])
              }

              Some(opts.List) -> {
                let map_newtype =
                  "deriv" |> dot("list_map") |> call([
                    fn_(["x"], [
                      "x" |> dot(newtype.field_access) |> g.Expression,
                    ]),
                  ])

                "json" |> dot("array") |> call([
                  value |> pipe(map_newtype),
                  encode_func,
                ])
              }
            }
          }


        _ -> panic as {
          "`json` currently only supports `NamedType`s"
        }
      }
    }

    Error(Nil) -> {
      let encode_override =
        case field {
          None ->
            Error(Nil)

          Some(field) ->
              get_field_opt(field:, opts: ctx.opts, desc: "json encode", matching: fn(opt) {
                case opt.strs {
                  ["json", "encode", encode] ->
                    Ok(encode)

                  _ ->
                    Error(Nil)
                }
              })
        }

      let encode_override =
        case encode_override, inner {
          Ok(_), Some(_) -> panic as {
            "inner & outer encode specified for :" <>
            string.inspect(field) <> " " <> string.inspect(type_)
          }

          Error(Nil), Some(inner) ->
            Ok(inner)

          _, _ ->
            encode_override
        }

      encode_override
      |> result.map(fn(encode_name) {
        case inner, type_ {
          Some(func), T(name: "Option", params: [_]) -> {
            "json" |> dot("nullable") |> call([
              value,
              func |> term,
            ])
          }

          Some(func), T(name: "List", params: [_]) -> {
            "json" |> dot("array") |> call([
              value,
              func |> term,
            ])
          }

          Some(func), T(params: [_], ..) ->
            { "encode_" <> type_.name |> common.snake_case } |> term |> call([
              value,
              func |> term,
            ])

          Some(_func), _ ->
            panic as "unimplemented"

          None, _ ->
            encode_name |> term |> call([value])
        }
      })
      |> result.lazy_unwrap(fn() {
        encode_call_(type_:, field:, value_arg: True, inner:, ctx:)
      })
    }
  }
}

fn encode_call_(
  type_ type_: T,
  field field: Option(Field),
  value_arg value_arg: Bool,
  inner inner: Option(String),
  ctx ctx: Context,
) -> g.Expression {
  let value =
    case value_arg, field {
      True, Some(field) -> "value" |> dot(field.gleam)
      True, None -> "value" |> term
      False, _ -> "_" |> term
    }

  let encode_call_ = fn(type_) {
    encode_call_(type_:, field:, value_arg: False, inner:, ctx:)
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
          // use <- bool.guard({ !is_not_phantom_type(type_name: type_.name, type_: ctx.type_) }, [])

          params
          |> reject_phantom_types_(type_name: type_.name, ctx:)
          // |> list.index_map(fn(param, idx) {
          //   case is_phantom_type_(type_name: type_.name, param_at: idx, ctx:) {
          //     False -> Error(Nil)
          //     True -> Ok(param)
          //   }
          // })
          // |> result.values
          |> list.map(fn(param) {
            case inner, param.params {
              Some(encode_func_name), _ -> encode_func_name |> term
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
