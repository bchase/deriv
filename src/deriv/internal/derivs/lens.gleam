import glance.{type CustomType, type Definition, type Function, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, UnlabelledField, FunctionParameter, Named, Case, Variant, Clause, PatternVariable, PatternString, PatternDiscard, PatternVariant, String} as g
import gleam/dict
import gleam/list
import gleam/string
import deriv/internal/common
import deriv/internal/types.{type Context, type Gen, Gen, type DerivFieldOpts} as deriv

pub fn gen(
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  let type_ = common.custom_type_or_panic(type_: t, deriv_name: "enum")

  let imports = imports(type_)

  // let fail = fail_variant(type_:, ctx:)

  let funcs =
    [
      // parse_func(type_:, fail:),
      // to_string_func(type_:),
    ]
    // |> list.append(optional_funcs(type_:, fail:, opts: ctx.opts))

  let src = ""
    funcs
    |> list.map(common.func_str)
    |> string.join("\n\n")

  Gen(file: ctx.file, deriv: ctx.deriv, imports:, funcs:, consts: [], types: [], src:, meta: dict.new())
}

fn imports(
  _type_: glance.CustomType,
) -> List(Import) {
  [
    common.import_("gleam/string"),
  ]
}
