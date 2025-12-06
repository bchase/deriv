import gleam/dict
import gleam/option.{Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type Expression, type CustomType, type Definition, type Function, type Variant, type VariantField, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, List, UnlabelledField, String, Int, Float}
import deriv/internal/types.{type Context, type Gen, Gen} as deriv
import deriv/internal/common


pub fn gen(
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/zero` "

    deriv.Type(type_:) -> {
      let imports = gen_imports(type_)

      let funcs =
        zero_func(type_)
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
  case common.are_any_fields_options(type_) {
    True -> [common.none_constr_import()]
    False -> []
  }
  |> list.append([
    common.util_import(),
  ])
}

fn zero_func(
  type_: CustomType,
) -> Definition(Function) {
  type_.variants
  |> list.fold_until(None, fn(acc, variant) {
    case zero_func_(variant, type_) {
      Ok(func) -> list.Stop(Some(func))
      Error(_) -> list.Continue(acc)
    }
  })
  |> option.lazy_unwrap(fn() {
    panic as { "`CustomType` has no variants!\n\n" <> string.inspect(type_)}
  })
}

fn zero_func_(
  variant: Variant,
  type_: CustomType
) -> Result(Definition(Function), Nil) {
  use field_zero_vals: List(glance.Field(Expression)) <- result.try(result.all(
    variant.fields
    |> list.map(zero_call)
    |> list.map(result.map(_, UnlabelledField))
  ))

  let constr_name = variant.name
  let func_name = "zero_" <> common.snake_case(type_.name)
  let func_return_type_name = type_.name

  let func_return_type = Some(NamedType(common.dummy_location(), func_return_type_name, None, []))

  let body =
    case variant.fields {
      [] -> {
        Variable(
          location: common.dummy_location(),
          name: constr_name,
        )
        |> Expression
      }

      _ -> {
        Call(
          location: common.dummy_location(),
          function: Variable(common.dummy_location(), constr_name),
          arguments: field_zero_vals,
        )
        |> Expression
      }
    }

  let func =
    Function(common.dummy_location(), func_name, Public, [], func_return_type, [body])

  Ok(Definition([], func))
}

fn zero_call(
  field: VariantField
) -> Result(Expression, Nil) {
  case field.item {
    NamedType(name: "String", ..) -> Ok(zero_string())
    NamedType(name: "Bool", ..) -> Ok(zero_bool())
    NamedType(name: "Int", ..) -> Ok(zero_int())
    NamedType(name: "Float", ..) -> Ok(zero_float())
    NamedType(name: "Option", ..) -> Ok(zero_option())
    NamedType(name: "List", ..) -> Ok(zero_list())
    NamedType(name: "Time", ..) -> Ok(zero_time())
    NamedType(name: "Uuid", ..) -> Ok(zero_uuid())
    NamedType(name: type_name, ..) -> Ok(zero_custom(type_name:))
    // _ -> Error(Nil)
    _ -> Error(Nil)
  }
}

fn zero_custom(
  type_name type_name: String,
) -> Expression {
  let type_name = type_name |> common.snake_case
  Call(common.dummy_location(), Variable(common.dummy_location(), "zero_" <> type_name), [])
}

fn zero_uuid() -> Expression {
  Call(common.dummy_location(), FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "util"), "zero_uuid"), [])
}

fn zero_time() -> Expression {
  Call(common.dummy_location(), FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "util"), "zero_time"), [])
}

fn zero_string() -> Expression {
  String(common.dummy_location(), "")
}

fn zero_option() -> Expression {
  Variable(common.dummy_location(), "None")
}

fn zero_int() -> Expression {
  Int(common.dummy_location(), "0")
}

fn zero_float() -> Expression {
  Float(common.dummy_location(), "0.0")
}

fn zero_bool() -> Expression {
  Variable(common.dummy_location(), "False")
}

fn zero_list() -> Expression {
  List(common.dummy_location(), [], None)
}
