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
import deriv/internal/common.{type ImportedType, InScope, Qualified, gtype, gtype_, build_imported_type}
import deriv/internal/opts

type From
type Into

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
          _ -> panic as "`from`/`into` requires specifying a single type in the form `m1/m2.T"
        }

      let imports = []

      let funcs =
        from(
          module: file.module,
          type_:,
          ident:,
          ctx:,
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

pub fn gen_into(
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
          idents -> panic as {
            "`from`/`into` requires specifying a single type in the form `m1/m2.T`, got: " <> string.inspect(idents)
          }
        }

      let imports = []

      let funcs =
        into(
          module: file.module,
          type_:,
          ident:,
          ctx:,
        )
        |> into_func
        |> list.wrap

      let src =
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file:, deriv:, imports:, funcs:, types: [], src:, meta: dict.new())
    }
  }
}

type Func(kind) {
  Func(
    func_name: String,
    param_type: ImportedType,
    return_type: String,
    return_contr: String,
    fields: List(Field(kind)),
  )
}

fn get_type_variant(
  ident ident: String,
  ctx ctx: Context,
) -> TypeVariant {
  let #(module, g.Definition(definition: type_, ..)) =
    case common.fetch_custom_type(ident, ctx.module_reader) {
      Error(err) -> {
        common.debug(ident)
        common.debug(err)
        panic as "`from`/`into` issue with the above `ident`"
      }

      Ok(x) ->
        x
    }

  case type_.variants {
    [variant] ->
      TypeVariant(module:, type_:, variant:)

    _, -> {
      common.debug(type_)
      panic as "`from`/`into` derivation currently only supports invariant types"
    }
  }
}

fn from(
  module module: String,
  type_ type_: CustomType,
  ident ident: String,
  ctx ctx: Context,
) -> Func(From) {
  func(kind: opts.From, module:, type_:, ident:, ctx:)
}

fn into(
  module module: String,
  type_ type_: CustomType,
  ident ident: String,
  ctx ctx: Context,
) -> Func(Into) {
  func(kind: opts.Into, module:, type_:, ident:, ctx:)
}

fn func(
  kind kind: opts.FromInto,
  module module: String,
  type_ type_: CustomType,
  ident ident: String,
  ctx ctx: Context,
) -> Func(kind) {
  let ident = {
    use <- bool.guard(common.starts_with_uppercase(ident), ctx.file.module <> "." <> ident)
    ident
  }

  case type_.variants {
    [variant] -> {
      let #(param, return) =
        case kind {
          opts.From -> {
            let local_type_variant = TypeVariant(module:, type_:, variant:)
            let remote_type_variant = get_type_variant(ident:, ctx:)

            #(remote_type_variant, local_type_variant)
          }

          opts.Into -> {
            let local_type_variant = get_type_variant(ident:, ctx:)
            let remote_type_variant = TypeVariant(module:, type_:, variant:)

            #(remote_type_variant, local_type_variant)
          }
        }

      build_variant(kind:, param:, return:, ident:, ctx:)
    }

    _, -> {
      common.debug(type_)
      panic as "`from`/`into` derivation currently only supports single-variant types"
    }
  }
}

type TypeVariant {
  TypeVariant(
    module: String,
    type_: g.CustomType,
    variant: g.Variant,
  )
}

// GLANCE

fn from_func(
  uf: Func(From)
) -> Definition(Function) {
  let Func(
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

fn into_func(
  uf: Func(Into)
) -> Definition(Function) {
  let Func(
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

// GENERAL

fn build_variant(
  kind kind: opts.FromInto,
  param param: TypeVariant,
  return return: TypeVariant,
  ident ident: String,
  ctx ctx: Context,
) -> Func(kind) {
  let ident =
    ident
    |> opts.parse_ident
    |> result.lazy_unwrap(fn() {
      panic as { "`from`/`into` couldn't make sense of this type identifier: " <> ident }
    })

  let fields =
    return.variant
    |> fields(type_: return.type_, variant: _)
    |> list.map(fn(t) {
      let #(field, type_) = t
      func_field(kind:, field:, type_:, ident:, ctx:)
    })

  // TODO rename things
  let from = common.snake_case(param.type_.name)
  let to = common.snake_case(return.type_.name)
  let func_name =
    case kind {
      opts.From -> "from_" <> from <> "_to_" <> to
      opts.Into -> "into_" <> to <> "_from_" <> from
    }

  let param_type = build_imported_type(param.module, param.type_, ctx.file)
  let return_type = return.type_.name
  let return_contr = return.variant.name

  Func(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  )
}

type Field(kind) {
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

fn func_field(
  kind kind: opts.FromInto,
  field field: DerivField,
  type_ type_: g.Type,
  ident ident: opts.Ident,
  ctx ctx: Context,
) -> Field(kind) {
  let overrides =
    ctx.opts
    |> dict.get(field)
    |> result.unwrap([] )
    |> list.map(opts.build_from_into_field_override(kind:, opt: _, field:, type_:))
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
      panic as { "`from`/`into` doesn't implement multi-variant `CustomType`s -- " <> string.inspect(t) }

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
