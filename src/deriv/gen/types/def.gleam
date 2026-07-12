import bchase/function.{x}
import glance as g
import gleam/option.{None, type Option}
import gleam/list
import gleam/pair
import gleam/result
import deriv/internal/glance.{z} as _
import deriv/gen/types.{type TypeDef}

pub type ExprGen {
  VariantClauseCaseExprGen(expr: VariantExpr)
}

//

pub opaque type VariantExpr {
  VariantExpr(
    run: fn(
      g.Variant,
      String,
      fn(Option(String), String) -> Result(TypeDef, Nil),
      List(String), // NOTE: fields acc, used to detect need for `with_spread`
    ) -> Result(#(g.Expression, List(String)), Nil),
  )
}
