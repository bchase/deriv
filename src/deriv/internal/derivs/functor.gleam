import deriv/internal/common/casing
import simplifile
import gleam/dict
import gleam/option.{Some, None}
import gleam/list
import gleam/result.{try}
import gleam/string
import glance.{type Expression, type CustomType, type Definition, type Function, type Variant, type VariantField, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, List, UnlabelledField, String, Int, Float, CustomType} as g
import deriv/internal/types.{type Context, type Gen, Gen} as deriv
import deriv/internal/common
import deriv/internal/glance.{x, term, dot, call} as _


pub fn gen(
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/functor` "

    deriv.Type(type_:) -> {
      let imports = gen_imports(type_)

      let funcs =
        map_func(type_, ctx)
        |> list.wrap

      let src = ""
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file: ctx.file, deriv: ctx.deriv, imports:, funcs:, consts: [], types: [], src:, meta: dict.new())
    }
  }
}

fn gen_imports(
  type_: CustomType,
) -> List(Import) {
  // case common.are_any_fields_options(type_) {
  //   True -> [common.none_constr_import()]
  //   False -> []
  // }
  // |> list.append([
  //   common.util_import(),
  // ])
  []
}

// Module([], [Definition([], CustomType(Span(0, 80), "Foo", Public, False, ["t"], [Variant("Foo", [LabelledVariantField(NamedType(Span(55, 61), "String", None, []), "bar"), LabelledVariantField(VariableType(Span(72, 73), "t"), "baz")], [])]))], [], [], [Definition([], Function(Span(94, 193), "map_foo", Private, [FunctionParameter(Some("foo"), Named("foo"), Some(NamedType(Span(117, 123), "Foo", None, [VariableType(Span(121, 122), "a")]))), FunctionParameter(Some("apply"), Named("f"), Some(FunctionType(Span(136, 146), [VariableType(Span(139, 140), "a")], VariableType(Span(145, 146), "b"))))], Some(NamedType(Span(153, 159), "Foo", None, [VariableType(Span(157, 158), "b")])), [Expression(RecordUpdate(Span(164, 191), None, "Foo", Variable(Span(170, 173), "foo"), [RecordUpdateField("baz", Some(Call(Span(180, 190), Variable(Span(180, 181), "f"), [UnlabelledField(FieldAccess(Span(182, 189), Variable(Span(182, 185), "foo"), "baz"))])))]))]))])

fn functor_prop(
  type_ type_: CustomType,
) -> Result(Prop, Nil) {
  case type_ {
    CustomType(variants: [], ..) ->
      panic as {
        "`derive functor` needs at least one variant, but found none:\n" <>
          string.inspect(type_)
      }

    CustomType(parameters: [], ..) ->
      panic as {
        "`derive functor` needs at least one type parameter, but found none:\n" <>
          string.inspect(type_)
      }

    CustomType(location:, name:, publicity:, opaque_:, parameters:, variants: [_, _, ..]) ->
      todo as "multi-variant functor derive"

    CustomType(parameters: params, variants: [variant], ..) -> {
      variant
      |> simple_prop(params:)
    }
  }
}

type Prop {
  Prop(
    variant_constr: String,
    name: String,
    type_: g.Type,
  )
}

fn simple_prop(
  variant variant: g.Variant,
  params params: List(String),
) -> Result(Prop, Nil) {
  use param <- try(list.first(params))

  variant.fields
  |> list.filter_map(fn(field) {
    case field {
      g.LabelledVariantField(item:, label:) -> {
        case item {
          g.VariableType(name:, ..) ->
            case name == param {
              False -> Error(Nil)
              True -> Ok(#(label, item))
            }

          _ -> Error(Nil)
        }
      }

      g.UnlabelledVariantField(..) ->
        Error(Nil)
    }
  })
  |> list.map(fn(t) {
    Prop(variant_constr: variant.name, name: t.0, type_: t.1)
  })
  |> list.first // TODO err/warn multiple?
}

fn map_func(
  type_ type_: CustomType,
  ctx _ctx: Context,
) -> Definition(Function) {
  let type_pascal_case = type_.name
  let type_snake_case = type_pascal_case |> casing.snake_case
  let func_name = "map_" <> type_snake_case

  let prop =
    case functor_prop(type_:) {
      Error(Nil) -> panic as {
        "`derive functor` couldn't find a property for: " <> string.inspect(type_)
      }

      Ok(prop) -> prop
    }

  Definition([],
    Function(x, func_name, type_.publicity,
      [
        g.FunctionParameter(Some(type_snake_case), g.Named(type_snake_case), Some(NamedType(x, type_pascal_case, None, [g.VariableType(x, "a")]))),
        g.FunctionParameter(Some("apply"), g.Named("f"), Some(g.FunctionType(x, [g.VariableType(x, "a")], g.VariableType(x, "b"))))
      ],

      Some(NamedType(x, type_pascal_case, None, [g.VariableType(x, "b")])),

      [
        Expression(g.RecordUpdate(x, None, prop.variant_constr, Variable(x, type_snake_case), [g.RecordUpdateField(prop.name, Some(Call(x, Variable(x, "f"), [UnlabelledField(FieldAccess(x, Variable(x, type_snake_case), prop.name))])))]))
      ]
    )
  )
}
