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
import deriv/internal/common.{type ImportedType, InScope, Qualified, gtype, gtype_, build_imported_type}
import deriv/internal/opts

pub type Override = opts.FromIntoOverride

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
    param_type: ImportedType,
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
    |> opts.parse_ident
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
  let param_type = build_imported_type(param_type_module, param_type, ctx.file)
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
    conv: Option(#(opts.Conv, Option(opts.Inner))),
  )
  Missing(
    field: String,
    type_: g.Type,
  )
}

fn from_func_field(
  field field: DerivField,
  type_ type_: g.Type,
  ident ident: opts.Ident,
  ctx ctx: Context,
) -> Field {
  let overrides =
    ctx.opts
    |> dict.get(field)
    |> result.unwrap([] )
    |> list.map(opts.build_from_into_field_override(opts.From, opt: _, field:, type_:))
    |> result.values

  let override =
    opts.match_specific(field:, ident:, overrides:)
    |> result.lazy_or(fn() {
      opts.match_general(field:, ident:, overrides:)
    })

  let #(param_field, conv, inner) =
    case override {
      Error(Nil) ->
        #(field.field, None, None)

      Ok(opts.SpecifyField(field:, ..)) ->
        #(field, None, None)

      Ok(opts.ConvAllWith(conv:, inner:)) ->
        #(field.field, Some(conv), inner)

      Ok(opts.ConvTypeWith(ident:, conv:, inner:)) ->
        case ident {
          opts.IdentFieldForType(field:, ..) ->
            #(field, Some(conv), inner)
          opts.IdentType(..) ->
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
                opts.EntireValue -> value
                opts.ValueDotField -> FieldAccess(x, value, field.param_field)
              }

            let conv_func =
              case conv {
                opts.Conv(module: Some(module), func:, ..) ->
                  FieldAccess(x,
                    Variable(x, module),
                    func,
                  )

                opts.Conv(module: None, func:, ..) ->
                  Variable(x, func)
              }

            let conv_func =
              case inner {
                None ->
                  conv_func

                Some(inner) -> {
                  let mod =
                    case inner {
                      opts.Option -> "option"
                      opts.List -> "list"
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
      InScope(name:, ..) -> gtype(name, [])
      Qualified(module:, name:) -> gtype_(module: Some(module), name:, params: [])
    }

  Definition([], Function(x, func_name, Public,
    [FunctionParameter(None, Named("value"), Some(param_type)), ..missing_params],
    Some(NamedType(x, return_type, None, [])),
    [Expression(Call(x, Variable(x, return_contr), fields))])
  )
}
