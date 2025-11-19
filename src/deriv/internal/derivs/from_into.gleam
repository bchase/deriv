import gleam/bool
import gleam/pair
import gleam/dict
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type CustomType, type Definition, type Function, LabelledVariantField, Definition, Function, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess} as _
import glance as g
import deriv/internal/types.{type Gen, Gen, type DerivField, DerivField, type Context} as deriv
import deriv/internal/common.{type ImportedType, InScope, Qualified, gtype, gtype_, build_imported_type}
import deriv/internal/opts

pub type Override = opts.FromIntoOverride

const x = g.Span(-1, -1)

fn gen(
  kind: opts.FromInto,
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `from`/`into`"

    deriv.Type(type_:) -> {
      let ident =
        case ctx.deriv.opts {
          [ident] -> ident
          _ -> panic as "`from`/`into` requires specifying a single type in the form `m1/m2.T"
        }

      let funcs =
        kind
        |> func(module: ctx.file.module, type_:, ident:, ctx:)
        |> build_glance_func
        |> list.wrap

      let src =
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file: ctx.file, deriv: ctx.deriv, imports: [], funcs:, types: [], src:, meta: dict.new())
    }
  }
}

pub fn gen_from(
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  gen(opts.From, t, ctx)
}

pub fn gen_into(
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  gen(opts.Into, t, ctx)
}

type Func(kind) {
  Func(
    kind: opts.FromInto,
    publicity: g.Publicity,
    func_name: String,
    func_param_name: String,
    param_type: ImportedType,
    return_type: ImportedType,
    return_constr: String,
    imported_type: Option(ImportedType),
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
      TypeVariant(module:, type_:, variant:, qualified: None)

    _, -> {
      common.debug(type_)
      panic as "`from`/`into` derivation currently only supports invariant types"
    }
  }
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

  let remote_type_variant = get_type_variant(ident:, ctx:)
  let imported_type =
    case ident |> opts.parse_ident {
      Ok(opts.IdentFieldForType(module: Some(module_name), ..)) |
      Ok(opts.IdentType(module: Some(module_name), ..)) ->
        common.build_imported_type(
          module_name:,
          type_: remote_type_variant.type_,
          file: ctx.file,
        )
        |> Some

      Ok(opts.IdentFieldForType(module: None, ..)) |
      Ok(opts.IdentType(module: None, ..)) |
      Error(_) ->
        None
    }

  let publicity = type_.publicity

  case type_.variants {
    [variant] -> {
      let local_type_variant = TypeVariant(module:, type_:, variant:, qualified: None)

      let #(param, return) =
        case kind {
          opts.From -> #(remote_type_variant, local_type_variant)
          opts.Into -> #(local_type_variant, remote_type_variant)
        }

      variant_func(kind:, publicity:, param:, return:, imported_type:, ident:, ctx:)
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
    qualified: Option(String),
  )
}

// GLANCE

fn build_glance_func(
  uf: Func(kind)
) -> Definition(Function) {
  let Func(
    kind:,
    publicity:,
    func_name:,
    func_param_name:,
    param_type:,
    return_type:,
    return_constr:,
    imported_type:,
    fields:,
  ) = uf
  let #(fields, missing_params) =
    list.map(fields, fn(field) {
      case field {
        Missing(field:, type_:) ->
          g.ShorthandField(label: field)
          |> pair.new(Ok(#(field, type_)))

        Field(..) as field -> case field.conv {
          None -> {
            let #(label, param_field) = case kind {
              opts.Into -> #(field.param_field, field.return_field)
              opts.From -> #(field.return_field, field.param_field)
            }
            // let #(label, param_field) =
            //   #(field.return_field, field.param_field)

            LabelledField(
              label:,
              item: FieldAccess(x,
                Variable(x, func_param_name),
                param_field,
              ))
              |> pair.new(Error(Nil))
          }

          Some(#(conv, inner)) -> {
            let value = Variable(x, func_param_name)

            let #(label, param_field) = case kind {
              opts.Into -> #(field.param_field, field.return_field)
              opts.From -> #(field.return_field, field.param_field)
            }
            // let #(label, param_field) =
            //   #(field.return_field, field.param_field)

            let value =
              case conv.args {
                opts.EntireValue -> value
                opts.ValueDotField -> FieldAccess(x, value, param_field)
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
              label:,
              item: g.BinaryOperator(x,
                name: g.Pipe,
                left: value,
                right: conv_func,
              ),
            )
            |> pair.new(Error(Nil))
          }
        }
      }
    })
    |> list.unzip

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

  let return_type =
    case kind, return_type {
      opts.From, _ |
      opts.Into, InScope(..) ->
        gtype(return_type.name, [])

      opts.Into, Qualified(module:, name:) ->
        gtype_(
          module: Some(module),
          name:,
          params:[]
        )
    }

  let #(return_type, return_constr) =
    case kind, imported_type {
      opts.From, _ |
      opts.Into, None |
      opts.Into, Some(InScope(curr_module: True, ..)) ->
        #(return_type, Variable(x, return_constr))
        // TODO `term` etc here

      opts.Into, Some(InScope(module:, curr_module: False, ..)) |
      opts.Into, Some(Qualified(module:, ..)) -> {
        let qualified_return_constr =
          FieldAccess(x, Variable(x, module), return_constr)

        #(return_type, qualified_return_constr)
      }
    }

  Definition([], Function(x, func_name, publicity,
    [FunctionParameter(Some(func_param_name), Named(func_param_name), Some(param_type)), ..missing_params],
    Some(return_type),
    [Expression(Call(x, return_constr, fields))])
  )
}

// GENERAL

fn variant_func(
  kind kind: opts.FromInto,
  publicity publicity: g.Publicity,
  param param: TypeVariant,
  return return: TypeVariant,
  imported_type imported_type: Option(ImportedType),
  ident ident: String,
  ctx ctx: Context,
) -> Func(kind) {
  // TODO refactor1 -- rename `param` & `return`

  let ident =
    ident
    |> opts.parse_ident
    |> result.lazy_unwrap(fn() {
      panic as { "`from`/`into` couldn't make sense of this type identifier: " <> ident }
    })

  let type_variant =
    case kind {
      opts.From -> return
      opts.Into -> param
    }

  let fields =
    type_variant
    |> fields
    |> list.map(fn(t) {
      let #(field, type_) = t
      func_field(kind:, param:, return:, field:, type_:, ident:, ctx:)
    })

  let fields =
    case kind {
      opts.From ->
        fields

      // TODO refactor1
      opts.Into -> {
        let param_fields =
          param.variant.fields
          |> list.filter_map(fn(field) {
            use #(field, type_) <- result.try(case field {
              g.LabelledVariantField(label: field, item: type_) -> Ok(#(field, type_))
              g.UnlabelledVariantField(..) -> Error(Nil)
            })

            let f = deriv.DerivField(param.type_.name, param.variant.name, field)

            let overrides =
              ctx.opts
              |> dict.get(f)
              |> result.unwrap([] )
              |> list.map(opts.build_from_into_field_override(kind:, opt: _, field: f, type_:))
              |> result.values

            let override =
              opts.match_specific(field: f, ident:, overrides:)
              |> result.lazy_or(fn() {
                opts.match_general(field: f, ident:, overrides:)
              })

            case override {
              // Ok(opts.ConvAllWith(..)) ->
              //   Error(Nil)

              Error(Nil) |
              Ok(opts.ConvAllWith(..)) |
              Ok(opts.ConvTypeWith(ident: opts.IdentType(..), ..), ..) ->
                Ok(field)

              Ok(opts.SpecifyField(field:, ..)) |
              Ok(opts.ConvTypeWith(ident: opts.IdentFieldForType(field:, ..), ..)) ->
                Ok(field)
            }
          })

        let return_fields = return.variant.fields |> field_names

        let fields =
          fields
          |> list.filter_map(fn(field) {
            case field {
              Field(..) ->
                Ok(field)

              Missing(field: f, ..) -> {
                use <- bool.guard(return_fields |> list.contains(f), Ok(field))
                Error(Nil)
              }
            }
          })

        let missing =
          return.variant.fields
          |> list.filter_map(fn(field) {
            case field {
              g.LabelledVariantField(label: field, item: type_) -> {
                use <- bool.guard(field |> list.contains(param_fields, _), Error(Nil))
                Ok(Missing(field:, type_:))
              }

              g.UnlabelledVariantField(..) ->
                Error(Nil)
            }
          })

        [ fields, missing ]
        |> list.flatten
      }
    }

  // TODO refactor1 -- rename
  let from = common.snake_case(param.type_.name)
  let to = common.snake_case(return.type_.name)
  let func_name =
    case kind {
      opts.From -> "from_" <> from <> "_to_" <> to
      opts.Into -> "into_" <> to <> "_from_" <> from
    }

  let param_type = build_imported_type(param.module, param.type_, ctx.file)
  let return_type = build_imported_type(return.module, return.type_, ctx.file)
  let return_constr = return.variant.name

  let func_param_name = build_uniq_func_param_name(param:, return:)

  Func(
    func_name:,
    func_param_name:,
    publicity:,
    param_type:,
    return_type:,
    return_constr:,
    kind:,
    imported_type:,
    fields:,
  )
}

fn build_uniq_func_param_name(
  param param: TypeVariant,
  return return: TypeVariant,
) -> String {
  return.variant.fields
  |> field_names
  |> build_uniq_func_param_name_(check: param.type_.name |> common.snake_case)
  // |> build_uniq_func_param_name_(check: "value")
}

fn build_uniq_func_param_name_(
  field_names field_names: List(String),
  check check: String,
) -> String {
  case list.contains(field_names, check) {
    False -> check
    True -> build_uniq_func_param_name_(field_names:, check: check <> "_")
  }
}

fn field_names(
  fields fields: List(g.VariantField),
) -> List(String) {
  fields
  |> list.filter_map(fn(field) {
    case field {
      g.LabelledVariantField(label: field, ..) -> Ok(field)
      g.UnlabelledVariantField(..) -> Error(Nil)
    }
  })
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
  param param: TypeVariant,
  return return: TypeVariant,
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

  let #(target_field, conv, inner) =
    case override {
      Error(Nil) ->
        #(None, None, None)

      Ok(opts.SpecifyField(field:, ..)) ->
        #(Some(field), None, None)

      Ok(opts.ConvAllWith(conv:, inner:)) ->
        #(None, Some(conv), inner)

      Ok(opts.ConvTypeWith(ident:, conv:, inner:)) ->
        case ident {
          opts.IdentFieldForType(field:, ..) ->
            #(Some(field), Some(conv), inner)
          opts.IdentType(..) ->
            #(None, Some(conv), inner)
        }
    }

  let conv = conv |> option.map(fn(conv) { #(conv, inner) })

  // TODO refactor1 -- all `case kind` below
  let #(param_field, return_field) =
    case kind, target_field {
      opts.From, None |
      opts.Into, None ->
        #(field.field, field.field)

      opts.From, Some(target) -> #(field.field, target)
      opts.Into, Some(target) -> #(target, field.field)
    }

  let variant =
    case kind {
      opts.From -> param.variant
      opts.Into -> return.variant
    }

  let has_field = variant.fields |> list.any(fn(f) {
    let assert g.LabelledVariantField(label: name, ..) = f
    case kind, target_field {
      // opts.From, _ -> name == return_field
      // opts.Into, _ -> name == param_field

      opts.From, None -> name == return_field
      opts.From, Some(target) -> target == return_field

      opts.Into, None -> name == param_field
      opts.Into, Some(target) -> target == param_field
    }
  })

  let #(param_field, return_field) =
    case kind {
      opts.Into -> #(param_field, return_field)
      opts.From -> #(return_field, param_field)
    }

  // param_field --> `LABEL: value.field`
  // return_field --> `value.FIELD`

  case has_field {
    True ->
      Field(
        param_field:,
        return_field:,
        conv:,
      )

    False ->
      Missing(
        field: return_field,
        type_:,
      )
  }
}

fn fields(
  variant variant: TypeVariant,
) -> List(#(DerivField, g.Type)) {
  let #(type_, variant) = #(variant.type_, variant. variant)

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
