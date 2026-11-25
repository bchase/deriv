import bchase/casing
import deriv/gen/types.{type GenDef} as x
import deriv/internal/glance.{dot, term, call, named_type}
import glance as g
import gleam/option.{Some, None}

pub fn lens() -> GenDef {
  x.CustomTypeVariantLabelledFieldDeriveExprGen(gens: [{
    use field <- x.field()

    use outer_name <- x.named_gen_param_or("inner", Ok, default: "o")
    use inner_name <- x.named_gen_param_or("outer", Ok, default: "i")

    use prefix <- build_prefix()

    let type_ = x.to_glance_type(field.custom_type, None)

    let lens_publicity = field.custom_type.publicity
    let get_set_publicity = g.Private

    //

    let get_lens_func = g.Definition([], g.Function(
      location: x.z,
      name: prefix <> field.field_name <> "_get",
      publicity: get_set_publicity,
      parameters: [
        g.FunctionParameter(None, g.Named(outer_name), Some(type_)),
      ],
      return: Some(field.field_type),
      body: [g.Expression({
        outer_name |> dot(field.field_name)
      })],
    ))

    let set_lens_func = g.Definition([], g.Function(
      location: x.z,
      name: prefix <> field.field_name <> "_set",
      publicity: get_set_publicity,
      parameters: [
        g.FunctionParameter(None, g.Named(outer_name), Some(type_)),
        g.FunctionParameter(None, g.Named(inner_name), Some(field.field_type)),
      ],
      return: Some(type_),
      body: [
        g.Expression(g.RecordUpdate(
          location: x.z,
          module: None,
          constructor: field.variant.name,
          record: term(outer_name),
          fields: [
            g.RecordUpdateField(field.field_name, Some(term(inner_name))),
          ],
        ))
      ],
    ))

    let lens_const = g.Definition([], g.Constant(
      location: x.z,
      name: field.field_name,
      publicity: lens_publicity,
      annotation: Some(named_type("Lens", [type_, field.field_type])),
      value: term("Lens") |> call([
        get_lens_func.definition.name |> term,
        set_lens_func.definition.name |> term,
      ]),
    ))

    //

    use <- x.ensure_import(lens_import)
    use <- x.overwrite_const(lens_const)
    use <- x.overwrite_func(get_lens_func)
    use <- x.overwrite_func(set_lens_func)

    x.custom_type_variant_labelled_field_success()
  }])
}

const lens_import = g.Definition([], g.Import(
  location: x.z,
  module: "bchase/lens",
  alias: None,
  unqualified_types: [g.UnqualifiedImport(name: "Lens", alias: None)],
  unqualified_values: [g.UnqualifiedImport(name: "Lens", alias: None)],
))

fn build_prefix(
  cont cont: fn(String) -> x.Gen(a, x.CustomTypeVariantLabelledField),
) -> x.Gen(a, x.CustomTypeVariantLabelledField) {
  use field <- x.field()

  use prefixed <- x.has_gen_param("prefixed")
  use prefix <- x.named_gen_param_or("prefix", Ok, default: "")

  case prefix, prefixed {
    "", False -> ""
    "", True -> field.variant.name |> casing.snake
    _, _ -> prefix
  }
  |> fn(prefix) {
    case prefix {
      "" -> ""
      _ -> prefix <> "_"
    }
  }
  |> cont
}
