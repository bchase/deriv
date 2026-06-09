import gleam/pair
import gleam/int
import gleam/dict
import gleam/option.{Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type Expression, type CustomType, type Definition, type Function, type Variant, type VariantField, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, List, UnlabelledField, String, Int, Float}
import deriv/internal/types.{type Context, type Gen, Gen} as deriv
import deriv/internal/common
import deriv/internal/glance.{term, dot, call} as _


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
        zero_func(type_, ctx)
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
  ctx: Context,
) -> Definition(Function) {
  type_.variants
  |> list.fold_until(None, fn(acc, variant) {
    case zero_func_(variant, type_, ctx) {
      Ok(func) -> list.Stop(Some(func))
      Error(_) -> list.Continue(acc)
    }
  })
  |> option.lazy_unwrap(fn() {
    panic as { "`CustomType` has no variants!\n\n" <> string.inspect(type_)}
  })
}

fn build_params_and_zero_vals(
  variant: Variant,
  type_: CustomType,
  ctx: Context,
) -> #(List(glance.FunctionParameter), List(glance.Field(Expression))) {
  let generic = "param"

  variant.fields
  |> list.map(zero_call(_, variant, type_, ctx))
  |> list.index_map(fn(result, idx) {
    case result {
      Ok(expr) ->
        #(None, expr)

      Error(glance.LabelledVariantField(label:, item: glance.VariableType(..) as type_)) ->
        #(Some(glance.FunctionParameter(label: Some(label), name: glance.Named(label), type_: Some(type_))), term(label))

      Error(glance.UnlabelledVariantField(item: glance.VariableType(..) as type_)) -> {
        let name = generic <> int.to_string(idx+1)
        #(Some(glance.FunctionParameter(label: Some(name), name: glance.Named(name), type_: Some(type_))), term(name))
      }

      Error(_) ->
        panic as {
          "`derive zero`"
        }
    }
  })
  |> list.map(pair.map_second(_, UnlabelledField))
  |> list.unzip
  |> pair.map_first(option.values)
}

fn zero_func_(
  variant: Variant,
  type_: CustomType,
  ctx: Context,
) -> Result(Definition(Function), VariantField) {
  let #(fn_params, field_zero_vals) = build_params_and_zero_vals(variant, type_, ctx)
  let return_type_params = fn_params |> list.map(fn(t) { t.type_ }) |> option.values

  let constr_name = variant.name
  let func_name = "zero_" <> common.snake_case(type_.name)
  let func_return_type_name = type_.name

  let func_return_type = Some(NamedType(common.dummy_location(), func_return_type_name, None, return_type_params))

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
    Function(common.dummy_location(), func_name, Public, fn_params, func_return_type, [body])

  Ok(Definition([], func))
}

fn zero_call(
  field: VariantField,
  variant: Variant,
  type_: CustomType,
  ctx: Context,
) -> Result(Expression, VariantField) {
  case field {
    glance.LabelledVariantField(label:, ..) -> {
      case ctx.opts |> dict.get(deriv.DerivField(type_: type_.name, variant: variant.name, field: label)) {
        Ok([]) | Error(Nil) ->
          default_zero_call(field)

        Ok(opts) ->
          zero_call_override(opts, field)
      }
    }

    glance.UnlabelledVariantField(..) ->
      default_zero_call(field)
  }
  |> result.replace_error(field)
}

fn zero_call_override(
  opts: List(deriv.DerivFieldOpt),
  field: VariantField,
) {
  opts
  |> list.filter_map(fn(opt) {
    case opt.strs {
      ["zero", ident] -> Ok(ident)
      _ -> Error(Nil)
    }
  })
  |> fn(overrides) {
    case overrides {
      [] ->
        default_zero_call(field)

      [override] ->
        case override |> string.split(".") {
          [override] -> Ok(override |> term |> call([]))
          [module, func] -> Ok(module |> dot(func))
          _ -> panic as { "can't make sense of `zero` override: " <> override }
        }

      [_opt1, _opt2, ..] ->
        panic as "invalid or multiple `zero` overrides specified"
    }
  }
}

fn default_zero_call(
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
