import gleam/dict
import gleam/option.{Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type Expression, type CustomType, type Definition, type Function, type Variant, type Span, type VariantField, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, Span, List, UnlabelledField, String, Int, Float}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader}
import deriv/util

pub fn gen(
  type_: CustomType,
  deriv: Derivation,
  _field_opts: DerivFieldOpts,
  file: File,
  _module_reader: ModuleReader,
) -> Gen {
  let imports = gen_imports(type_)

  let funcs =
    zero_func(type_)
    |> list.wrap

  let src = ""
    funcs
    |> list.map(util.func_str)
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
}

fn gen_imports(
  type_: CustomType,
) -> List(Import) {
  case util.are_any_fields_options(type_) {
    True -> [util.none_constr_import()]
    False -> []
  }
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
  let func_name = "zero_" <> util.snake_case(type_.name)
  let func_return_type_name = type_.name

  let func_return_type = Some(NamedType(func_return_type_name, None, []))

  let body =
    Call(
      function: Variable(constr_name),
      arguments: field_zero_vals,
    )
    |> Expression

  let func =
    Function(func_name, Public, [], func_return_type, [body], dummy_location())

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
    _ -> Error(Nil)
  }
}

fn zero_uuid() -> Expression {
  Call(FieldAccess(Variable("util"), "zero_uuid"), [])
}

fn zero_time() -> Expression {
  Call(FieldAccess(Variable("util"), "zero_time"), [])
}

fn zero_string() -> Expression {
  String("")
}

fn zero_option() -> Expression {
  Variable("None")
}

fn zero_int() -> Expression {
  Int("0")
}

fn zero_float() -> Expression {
  Float("0.0")
}

fn zero_bool() -> Expression {
  Variable("False")
}

fn zero_list() -> Expression {
  List([], None)
}

fn dummy_location() -> Span {
  Span(0, 0)
}
