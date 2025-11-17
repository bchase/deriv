import gleam/bool
import gleam/pair
import gleam/dict
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess} as _
import glance as g
import deriv/internal/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivField} as deriv
import deriv/internal/common.{gtype, gtype_}

pub type GenFunc = fn(CustomType, Derivation, DerivFieldOpts, File) -> Gen

type Context {
  Context(
    file: File,
    opts: DerivFieldOpts,
    module_reader: ModuleReader,
  )
}

const x = g.Span(-1, -1)

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  let ctx = Context(file:, opts:, module_reader:)

  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/from` "

    deriv.Type(type_:) -> {
      let ident =
        case deriv.opts {
          [ident] -> {
            ident
          }
          _ -> panic as "`from` requires specifying a single type in the form `m1/m2.T"
        }

      let imports = []

      let funcs =
        from(
          type_,
          ident,
          module_reader,
          ctx,
        )
        |> from_func
        |> list.wrap

      let src =
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file:, deriv:, imports:, funcs:, types: [], src:, meta: dict.new())
    }
  }
}

type FromFunc {
  FromFunc(
    func_name: String,
    param_type: ParamType,
    return_type: String,
    return_contr: String,
    fields: List(Field),
  )
}

fn get_param_types_and_variants(
  ident: String,
  module_reader: ModuleReader,
) -> #(Definition(CustomType), Variant, String) {
  let #(module_name, type_) =
    case common.fetch_custom_type(ident, module_reader) {
      Error(err) -> {
        common.debug(ident)
        common.debug(err)
        panic as "`from` issue with the above `ident`"
      }

      Ok(x) ->
        x
    }

  case type_.definition.variants {
    [variant] ->
      #(type_, variant, module_name)

    _, -> {
      common.debug(type_)
      panic as "`from` derivation currently only supports invariant types"
    }
  }
}

fn from(
  return_type: CustomType,
  ident: String,
  module_reader: ModuleReader,
  ctx: Context,
) -> FromFunc {
  case return_type.variants {
    [return_variant] -> {
      let #(param_type_def, param_variant, param_type_module) =
        get_param_types_and_variants(ident, module_reader)

      let param_type = param_type_def.definition

      from_variant_(
        param_type_module,
        param_type,
        param_variant,
        return_type,
        return_variant,
        ident,
        ctx,
      )
    }

    _, -> {
      common.debug(return_type)
      panic as "`from` derivation currently only supports single-variant types"
    }
  }
}

type ParamType {
  InScope(name: String)
  Qualified(name: String, module: String)
}

fn build_param_type(
  param_type_module: String,
  param_type: CustomType,
  ctx: Context,
) {
  let assert Ok(module) = g.module(ctx.file.src)

  let is_curr_module = param_type_module == ctx.file.module
  use <- bool.guard(is_curr_module, InScope(name: param_type.name))

  case common.find_import(module_name: param_type_module, module:) {
    Error(_) -> panic as {
      // TODO auto import
      ctx.file.module <> "\n" <>
      "missing import for: " <> param_type_module <> "." <> param_type.name
    }

    Ok(g.Definition(_, import_)) -> {
      let is_in_scope =
        import_.unqualified_types
        |> list.any(fn(t) { t.name == param_type.name })

      use <- bool.guard(is_in_scope, InScope(name: param_type.name))

      let assert Ok(module) =
        import_.module
        |> string.split("/")
        |> list.last

      let module =
        case import_.alias {
          Some(g.Discarded(..)) -> panic as {
            ctx.file.module <> "\n" <>
            "neither exposing type nor providing module for: " <> param_type_module <> "." <> param_type.name
          }
          None -> module
          Some(g.Named(module)) -> module
        }

      Qualified(module:, name: param_type.name)
    }
  }
}


fn from_variant_(
  param_type_module: String,
  param_type: CustomType,
  param_variant: Variant,
  return_type: CustomType,
  return_variant: Variant,
  ident: String,
  ctx: Context,
) -> FromFunc {
  let ident =
    ident
    |> parse_ident
    |> result.lazy_unwrap(fn() {
      panic as { "`from` couldn't make sense of this type identifier: " <> ident }
    })

  let fields =
    return_variant
    |> fields(type_: return_type, variant: _)
    |> list.map(fn(t) {
      let #(field, type_) = t
      from_func_field(field:, type_:, ident:, ctx:)
    })

  let from = common.snake_case(param_type.name)
  let to = common.snake_case(return_type.name)

  let func_name = "from_" <> from <> "_to_" <> to
  let param_type = build_param_type(param_type_module, param_type, ctx)
  let return_type = return_type.name
  let return_contr = return_variant.name

  FromFunc(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  )
}

type Field {
  Field(
    param_field: String,
    return_field: String,
    conv: Option(#(Conv, Option(Inner))),
  )
  Missing(
    field: String,
    type_: g.Type,
  )
}

pub type ConvArgs {
  EntireValue
  ValueDotField
}

fn parse_ident_with_field(
  ident ident: String,
) -> #(String, String, Option(String)) {
  case string.split(ident, ".") {
    [""] -> #("", "", None)
    [a, b] -> {
      case a, b, starts_with_uppercase(b) {
        module, type_, True -> #(module, module <> "." <> type_, None)
        type_, field, False -> #("", type_, Some(field))
      }
    }
    [module, type_, field] -> #(module, module <> "." <> type_, Some(field))
    [type_] -> #("", type_, None)
    // _ -> panic // TODO panic w/ error
    _ -> #("", "", None)
  }
}

type Ident {
  IdentFieldForType(
    module: Option(String),
    type_: String,
    field: String,
  )
  IdentType(
    module: Option(String),
    type_: String,
  )
}

fn starts_with_uppercase(str: String) -> Bool {
  str
  |> string.first
  |> result.map(fn(ch) { ch == string.uppercase(ch) })
  |> result.unwrap(False)
}

fn parse_ident(
  ident ident: String,
) -> Result(Ident, Nil) {
  case ident |> string.split(".") {
    [""] -> Error(Nil)
    [type_] ->
      case starts_with_uppercase(type_) {
        True -> Ok(IdentType(module: None, type_:))
        False -> Error(Nil)
      }
    [module, type_, field] ->
      case starts_with_uppercase(type_) {
        True -> Ok(IdentFieldForType(module: Some(module), type_:, field:))
        False -> Error(Nil)
      }
    [a, b] ->
      case starts_with_uppercase(a), starts_with_uppercase(b), a, b {
        True, False, type_, field -> Ok(IdentFieldForType(module: None, type_:, field:))
        False, True, module, type_ -> Ok(IdentType(module: Some(module), type_:))
        _, _, _, _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

type Conv {
  Conv(
    module: Option(String),
    func: String,
    args: ConvArgs,
    inner: Option(Inner),
  )
}

type Inner {
  Option
  List
}

type Override {
  SpecifyField(ident: Ident, field: String)
  ConvAllWith(conv: Conv, inner: Option(Inner))
  ConvTypeWith(ident: Ident, conv: Conv, inner: Option(Inner))
}


fn generalize_field_ident(
  ident ident: Ident,
) -> #(String, String) {
  case ident {
    IdentType(module:, type_:) -> #(module |> option.unwrap(""), type_)
    IdentFieldForType(module:, type_:, ..) -> #(module |> option.unwrap(""), type_)
  }
}

fn match_specific(
  field f: DerivField,
  ident ident: Ident,
  overrides overrides: List(Override),
) -> Result(Override, Nil) {
  let module_type = #(ident.module |> option.unwrap(""), ident.type_)

  overrides
  |> list.filter(fn(override) {
    case override {
      SpecifyField(ident:, ..) |
      ConvTypeWith(ident:, ..) -> {
        generalize_field_ident(ident) == module_type
      }

      _ -> {
        False
      }
    }
  })
  |> fn(os) {
    case os {
      [_, _, ..] -> panic as {
        [
          "`from` found multiple options specifically for the same field, namely:\n",
          f |> string.inspect,
          .. os |> list.map(string.inspect),
        ]
        |> string.join("")
      }
      [o] -> Ok(o)
      [] -> Error(Nil)
    }
  }
}

fn match_general(
  field f: DerivField,
  ident ident: Ident,
  overrides overrides: List(Override),
) -> Result(Override, Nil) {
  let overrides_by_unqualified_type =
    overrides
    |> list.filter(fn(override)  {
      case override {
        ConvTypeWith(ident: IdentFieldForType(type_: "", module: None, ..), ..) |
        ConvTypeWith(ident: IdentFieldForType(type_: "*", module: None, ..), ..) |
        SpecifyField(ident: IdentFieldForType(type_: "", module: None, ..), ..) |
        SpecifyField(ident: IdentFieldForType(type_: "*", module: None, ..), ..) -> {
          True
        }

        ConvTypeWith(ident: IdentFieldForType(type_:, module: None, ..), ..) |
        SpecifyField(ident: IdentFieldForType(type_:, module: None, ..), ..) |
        ConvTypeWith(ident: IdentType(type_:, module: None), ..) -> {
          type_ == ident.type_
        }

        _ -> {
          False
        }
      }
    })
    |> fn(os) {
      case os {
        [o, _, ..] -> {
          [
            "`from` found multiple options generally matching the following field:\n",
            f |> string.inspect,
            .. os |> list.map(string.inspect),
          ]
          |> string.join("")
          |> io.println_error

          Ok(o)
        }
        [o] -> Ok(o)
        [] -> Error(Nil)
      }
    }

  let overrides_for_all = fn() {
    overrides
    |> list.find(fn(override) {
      case override {
        ConvAllWith(..) -> True
        _ -> False
      }
    })
    // |> fn(os) {
    //   case os {
    //     [o, _, ..] -> {
    //       [
    //         "`from` found multiple options generally matching the following field:\n",
    //         f |> string.inspect,
    //         .. os |> list.map(string.inspect),
    //       ]
    //       |> string.join("")
    //       |> io.println_error

    //       Ok(o)
    //     }
    //     [o] -> Ok(o)
    //     [] -> Error(Nil)
    //   }
    // }
  }

  overrides_by_unqualified_type
  |> result.lazy_or(overrides_for_all)
}

fn build_field_override(
  opt opt: DerivFieldOpt,
  field field: DerivField,
  type_ type_: g.Type,
) -> Result(Override, Nil) {
  // let module_name =
  //   case common.fetch_custom_type(ident, module_reader) {
  //     Ok(#(m, _td)) -> m
  //     Error(_err) -> ""
  //   }
  // let #(module_name, type_) =
  //   case common.fetch_custom_type(ident, module_reader) {
  //     Error(err) -> {
  //       common.debug(err)
  //       panic
  //     }
  //     Ok(#(m, td)) -> #(m, td.definition)
  //   }

  let not_inner = ""

  case opt.strs {
    ["from", ..rest] -> {
      case rest, not_inner {
        [ident], _ -> {
          result.try(parse_ident(ident:), fn(ident) {
            case ident {
              IdentType(..) ->
                Error(Nil)

              IdentFieldForType(field:, ..) ->
                Ok(SpecifyField(ident:, field:))
            }
          })
        }

        ["*", "using", conv], inner_str |
        ["*", "using", "inner" as inner_str, conv], _ |
        ["using", conv], inner_str |
        ["using", conv, "inner" as inner_str], _ -> {
          let inner = inner_str |> to_inner(relative_to: type_, on: field)

          Ok(ConvAllWith(conv: { conv |> parse_conv_or_panic }(ValueDotField), inner:))
        }

        [ident, "using", "inner" as inner_str, conv], _ |
        [ident, "using", conv], inner_str -> {
          let inner = inner_str |> to_inner(relative_to: type_, on: field)

          result.try(parse_ident(ident:), fn(ident) {
            let build_conv = conv |> parse_conv_or_panic
            // TODO clean up `*` handling
            let args =
              case ident.type_ == "*", ident.type_ |> string.contains("*") {
                True, _ -> ValueDotField
                False, True -> EntireValue
                False, False -> ValueDotField
               }
            let conv = build_conv(args)
            let type_ = ident.type_ |> string.replace("*", "")
            let ident =
              case ident {
                IdentFieldForType(..) -> IdentFieldForType(..ident, type_:)
                IdentType(..) -> IdentType(..ident, type_:)
              }
            Ok(ConvTypeWith(ident:, conv:, inner:))
          })
        }

        _, _ -> {
          panic as { "`from` invalid field option: " <> opt.strs |> string.join(" ") }
        }
      }
    }

    _ -> {
      Error(Nil)
    }
  }
}

fn to_inner(
  inner_str inner_str: String,
  relative_to type_: g.Type,
  on field: DerivField,
) -> Option(Inner) {
  case inner_str == "inner", type_ {
    False, _ -> None
    True, g.NamedType(name: "Option", parameters: [_], ..) -> Some(Option)
    True, g.NamedType(name: "List", parameters: [_], ..) -> Some(List)
    True, _ -> panic as { "`from`: the `inner` option only makes sense in the context of a `List` or `Option` but got: \n" <>
      string.inspect(field) <> "\n" <>
      string.inspect(type_) <> "\n"
    }
  }
}

fn parse_conv_or_panic(
  str str: String,
) -> fn(ConvArgs) -> Conv {
  case str |> string.split(".") {
    [""] -> panic as { "`from` must specify a convert function for `using`" }
    [func] -> fn(args) { Conv(module: None, func:, args:, inner: None) }
    [module, func] -> fn(args) { Conv(module: Some(module), func:, args:, inner: None) }
    _ -> panic as {
      "`from` field convert function specified by `using` is invalid. Valid syntax is `func_name` or `module.func_name`, but got: " <> str
    }
  }
}


fn from_func_field(
  field field: DerivField,
  type_ type_: g.Type,
  ident ident: Ident,
  ctx ctx: Context,
) -> Field {
  let overrides =
    ctx.opts
    |> dict.get(field)
    |> result.unwrap([] )
    |> list.map(build_field_override(opt: _, field:, type_:))
    |> result.values

  let override =
    match_specific(field:, ident:, overrides:)
    |> result.lazy_or(fn() {
      match_general(field:, ident:, overrides:)
    })

  let #(param_field, conv, inner) =
    case override {
      Error(Nil) ->
        #(field.field, None, None)

      Ok(SpecifyField(field:, ..)) ->
        #(field, None, None)

      Ok(ConvAllWith(conv:, inner:)) ->
        #(field.field, Some(conv), inner)

      Ok(ConvTypeWith(ident:, conv:, inner:)) ->
        case ident {
          IdentFieldForType(field:, ..) ->
            #(field, Some(conv), inner)
          IdentType(..) ->
            #(field.field, Some(conv), inner)
        }
    }

  let conv = conv |> option.map(fn(conv) { #(conv, inner) })

  let ident =
    ident.module
    |> option.unwrap(ctx.file.module)
    |> string.append(to: _, suffix: "." <> ident.type_)

  case common.fetch_custom_type(ident, ctx.module_reader) {
    Ok(#(_module, g.Definition(_, g.CustomType(variants: [variant], ..)))) -> {
      let has_field = variant.fields |> list.any(fn(f) {
        let assert g.LabelledVariantField(label: name, ..) = f
        name == param_field
      })

      case has_field {
        True ->
          Field(
            param_field:,
            return_field: field.field,
            conv:,
          )

        False ->
          Missing(
            field: field.field,
            type_:,
          )
      }
    }

    Ok(#(_module, g.Definition(_, g.CustomType(variants: _, ..))) as t) ->
      panic as { "`from` doesn't implement multi-variant `CustomType`s -- " <> string.inspect(t) }

    Error(err) ->
      panic as { "Error fetching `CustomType`: " <> string.inspect(err) }
  }
}

fn fields(
  type_ type_: CustomType,
  variant variant: Variant,
) -> List(#(DerivField, g.Type)) {
  variant.fields
  |> list.map(fn(field) {
    case field {
      LabelledVariantField(item: NamedType(..) as t, label: field) ->
        #(DerivField(type_: type_.name, variant: variant.name, field:), t)

      _ -> {
        common.debug(variant)
        common.debug(field)
        panic as "Only the following field type is supported: `LabelledVariantField(item: NamedType(name:, ..), label:)`"
      }
    }
  })
}

fn from_func(
  uf: FromFunc
) -> Definition(Function) {
  let FromFunc(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  ) = uf

  // let #(fields, missing) =
  let #(fields, missing_params) =
    list.map(fields, fn(field) {
      case field {
        Missing(field:, type_:) ->
          // Error(g.ShorthandField(label: field))
          g.ShorthandField(label: field)
          |> pair.new(Ok(#(field, type_)))

        Field(..) as field -> case field.conv {
          None ->
            LabelledField(
              label: field.return_field,
              item: FieldAccess(x,
                Variable(x, "value"),
                field.param_field
              ))
              |> pair.new(Error(Nil))

          Some(#(conv, inner)) -> {
            let value = Variable(x, "value")

            let value =
              case conv.args {
                EntireValue -> value
                ValueDotField -> FieldAccess(x, value, field.param_field)
              }

            let conv_func =
              case conv {
                Conv(module: Some(module), func:, ..) ->
                  FieldAccess(x,
                    Variable(x, module),
                    func,
                  )

                Conv(module: None, func:, ..) ->
                  Variable(x, func)
              }

            let conv_func =
              case inner {
                None ->
                  conv_func

                Some(inner) -> {
                  let mod =
                    case inner {
                      Option -> "option"
                      List -> "list"
                    }

                  FieldAccess(x, Variable(x, mod), "map")
                  |> Call(x, _, [ conv_func |> g.UnlabelledField ])
                }
              }

            LabelledField(
              label: field.return_field,
              item: g.BinaryOperator(x,
                name: g.Pipe,
                left: value,
                right: conv_func,
              ),
            )
            |> pair.new(Error(Nil))
          }
        }
        // |> Ok
      }
    })
    |> list.unzip
    // |> result.partition

  let missing_params =
    missing_params
    |> result.values
    |> list.map(fn(t) {
      let #(field, type_) = t

      FunctionParameter(
        label: Some(field),
        name: g.Named(field),
        type_: Some(type_),
      )
    })

  let param_type =
    case param_type {
      InScope(name:) -> gtype(name, [])
      Qualified(module:, name:) -> gtype_(module: Some(module), name:, params: [])
    }

  Definition([], Function(x, func_name, Public,
    [FunctionParameter(None, Named("value"), Some(param_type)), ..missing_params],
    Some(NamedType(x, return_type, None, [])),
    [Expression(Call(x, Variable(x, return_contr), fields))])
  )
}
