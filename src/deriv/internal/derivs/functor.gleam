import simplifile
import gleam/int
import deriv/internal/common/casing
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
  _type_: CustomType,
) -> List(Import) {
  []
}

// fn functor_props(
//   type_ type_: CustomType,
//   override override: Result(Prop, Nil),
// ) -> List(Prop) {
//   case type_ {
//     CustomType(variants: [], ..) ->
//       panic as {
//         "`derive functor` needs at least one variant, but found none:\n" <>
//           string.inspect(type_)
//       }

//     CustomType(parameters: params, variants:, ..) -> {
//       variants
//       |> build_props(type_:, params:, override:)
//       |> todo
//     }
//   }
// }

fn build_params_list(
  params params: List(String),
  prop_type prop_type: String,
  var var: String,
  generic generic: String,
) -> List(String) {
  params
  |> build_params_list_(prop_type:, var:, generic:, idx: 1, acc: [])
  |> list.reverse
}


fn build_params_list_(
  params params: List(String),
  prop_type prop_type: String,
  var var: String,
  generic generic: String,
  idx idx: Int,
  acc acc: List(String,)
) -> List(String) {
  case params {
    [] ->
      acc

    [param, ..params] -> {
      let #(str, idx) =
        case param == prop_type {
          True -> #(var, idx)
          False -> #(generic <> int.to_string(idx), idx + 1)
        }

      let acc = [str, ..acc]

      build_params_list_(params:, prop_type:, var:, generic:, idx:, acc:)
    }
  }
}

type Prop {
  Prop(
    variant_constr: String,
    name: String,
    type_name: String,
  )
}

fn functor_props(
  type_ type_: CustomType,
  override override: Result(Prop, Nil),
) -> List(Prop) {
  let param =
    case override |> result.map(fn(o) { o.type_name}) |> result.or(list.first(type_.parameters)) {
      Ok(param) -> param
      Error(Nil) -> panic as {
        "`derive functor` needs at least one type parameter, but found none:\n" <>
          string.inspect(type_)
      }
    }

  use variant <- list.flat_map(type_.variants)
  use field <- list.filter_map(variant.fields)

  case field {
    g.LabelledVariantField(item: g.VariableType(name:, ..), label:) if name == param ->
      Ok(Prop(variant_constr: variant.name, name: label, type_name: name))

    _ ->
      Error(Nil)
  }
}

fn map_func(
  type_ type_: CustomType,
  ctx ctx: Context,
) -> Definition(Function) {
  let before_var = "a"
  let after_var = "b"
  let generic = "t"

  let override = case ctx.deriv.opts {
    [] -> Error(Nil)
    [_, _, ..] as opts -> {
      panic as { "`derive functor` only accepts on option (to specify a property name), but got:\n" <>
        string.inspect(opts) <> "\n" <>
        string.inspect(type_)
      }
    }
    [opt] ->
      {
        use variant <- list.map(type_.variants)
        use field <- list.find_map(variant.fields)
        case field {
          g.LabelledVariantField(item: g.VariableType(name:, ..), label:) if label == opt ->
            Ok(Prop(variant_constr: variant.name, name: label, type_name: name))

          _ ->
            Error(Nil)
        }
      }
      |> list.first
      |> result.flatten
  }

  let type_pascal_case = type_.name
  let type_snake_case = type_pascal_case |> casing.snake_case
  let func_name = "map_" <> type_snake_case
  let func_name = case override {
    Error(Nil) -> func_name
    Ok(prop) -> func_name <> "_" <> prop.name
  }

  let expr_for = fn(prop: Prop) -> g.Expression {
    g.RecordUpdate(x, None, prop.variant_constr, Variable(x, type_snake_case), [g.RecordUpdateField(prop.name, Some(Call(x, Variable(x, "f"), [UnlabelledField(FieldAccess(x, Variable(x, type_snake_case), prop.name))])))])
  }

  let #(prop_type, func_body) =
    case functor_props(type_:, override:) {
      [] ->
        panic as { "`derive functor` couldn't find a property for: " <> string.inspect(type_) }

      [prop] ->
        #(prop.type_name, g.Expression(expr_for(prop)))

      [prop, ..] as props ->
        #(prop.type_name, g.Expression(g.Case(x, subjects: [Variable(x, type_snake_case)], clauses: {
          use prop <- list.map(props)
          g.Clause(patterns: [[g.PatternVariant(x, None, constructor: prop.variant_constr, arguments: [], with_spread: True)]], guard: None, body: expr_for(prop))
        })))
    }

  let params_with = fn(var) {
    type_.parameters
    |> build_params_list(prop_type:, var:, generic:)
    |> list.map(fn(t) { g.VariableType(x, t) })
  }

  Definition([],
    Function(x, func_name, type_.publicity,
      [
        g.FunctionParameter(Some(type_snake_case), g.Named(type_snake_case), Some(NamedType(x, type_pascal_case, None, params_with(before_var)))),
        g.FunctionParameter(Some("apply"), g.Named("f"), Some(g.FunctionType(x, [g.VariableType(x, before_var)], g.VariableType(x, after_var))))
      ],

      Some(NamedType(x, type_pascal_case, None, params_with(after_var))),

      [
        func_body,
      ]
    )
  )
}

// pub fn main() {
//   let assert Ok(src) = simplifile.read("/home/bosco/dev/gleam/deriv/test/examples/functor/after.gleam")
//   let assert Ok(module) = g.module(src)

//   use f <- list.each(module.functions)
//   echo f

//   Nil
// }
