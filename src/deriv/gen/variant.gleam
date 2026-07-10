import bchase/casing
import bchase/function.{x}
import glance as g
import gleam/option.{None}
import gleam/list
import gleam/pair
import gleam/result
import deriv/internal/glance.{z, term, call, call_, pipe, dot} as _

pub opaque type VariantExpr(out) {
  VariantExpr(
    run: fn(
      g.Variant,
      String,
      fn(g.Type) -> Result(g.CustomType, Nil),
      List(String), // fields acc, used to detect need for `with_spread`
    ) -> Result(#(g.Expression, List(String)), Nil),
  )
}

// pub fn parse(variant variant: g.Variant, decoder decoder: VariantExpr(out)) {
//   todo
// }

fn variant_shorthand_field_type(
  name name: String,
  cont cont: fn(g.Type) -> VariantExpr(out),
) -> VariantExpr(out) {
  VariantExpr(fn(variant, args, get_type, fields) {
    use #(field, f) <- result.try(
      variant.fields
      |> list.find_map(fn(field) {
        case field {
          g.LabelledVariantField(label:, item:) if label == name ->
            Ok(#(name, item))

          _ -> Error(Nil)
        }
      }),
    )

    cont(f).run(variant, args, get_type, [field, ..fields])
  })
}

pub fn variant_shorthand_field(
  name name: String,
  cont cont: fn(g.Field(g.Expression)) -> VariantExpr(out),
) -> VariantExpr(out) {
  variant_shorthand_field_map(name, cont, x(short, pair.first))
}

fn variant_shorthand_type(
  name name: String,
  cont cont: fn(g.Type) -> VariantExpr(out),
) -> VariantExpr(out) {
  variant_shorthand_field_map(name, cont, pair.second)
}

fn variant_shorthand_field_map(
  name name: String,
  cont cont: fn(t) -> VariantExpr(out),
  apply f: fn(#(String, g.Type)) -> t,
) -> VariantExpr(out) {
  VariantExpr(fn(variant, args, get_type, fields) {
    use type_ <- result.try(
      variant.fields
      |> list.find_map(fn(field) {
        case field {
          g.LabelledVariantField(label:, item:) if label == name -> Ok(item)

          _ -> Error(Nil)
        }
      }),
    )

    cont(f(#(name, type_))).run(variant, args, get_type, [name, ..fields])
  })
}

pub fn failure() -> VariantExpr(out) {
  VariantExpr(fn(_, _, _, _) { Error(Nil) })
}

pub fn success(expr expr: g.Expression) -> VariantExpr(out) {
  VariantExpr(fn(_, _, _, fields) { Ok(#(expr, fields)) })
}

pub fn run(
  ve: VariantExpr(out),
  variant variant: g.Variant,
  args args: String,
  get_type get_type: fn(g.Type) -> Result(g.CustomType, Nil),
) -> Result(g.Clause, Nil) {
  ve.run(variant, args, get_type, [])
  |> result.map(fn(t) {
    let #(expr, fields) = t

    let with_spread = list.length(variant.fields) > list.length(fields)
    let arguments = fields |> list.map(g.ShorthandField)

    let pattern =
      g.PatternVariant(z, None, variant.name, arguments:, with_spread:)

    g.Clause(patterns: [[pattern]], guard: None, body: expr)
  })
}

pub fn variant_name(
  cont cont: fn(String) -> VariantExpr(out),
) -> VariantExpr(out) {
  VariantExpr(fn(variant, args, get_type, fields) {
    cont(variant.name).run(variant, args, get_type, fields)
  })
}

pub fn short(field: String) -> g.Field(t) {
  g.ShorthandField(field)
}
