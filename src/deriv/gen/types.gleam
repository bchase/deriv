import gleam/set.{type Set}
import bchase/function.{x}
import bchase/list.{at as list_at} as _
import deriv/internal/glance.{z} as _
import glance as g
import gleam/list
import gleam/dict.{type Dict}
import gleam/option.{None, type Option}
import gleam/pair
import gleam/result

pub type ExprGen {
  VariantClauseCaseExprGen(clauses: List(VariantExpr))
}

pub type TypeDef {
  TypeDef(
    path: GleamPath,
    def: g.Definition(g.CustomType),
  )
}

// pub type ExprGenRef {
//   ExprGenRef(
//     module: String,
//     func: String,
//   )
// }

// pub fn expr_gen_ref(
//   path path: GleamPath,
//   func func: String,
// ) -> ExprGenRef {
//   ExprGenRef(
//     module: path.full |> string.join("/"),
//     func:,
//   )
// }

pub type GleamPath {
  GleamPath(
    full: List(String),
    package: String,
    module: String,
  )
}

pub type GleamFile {
  GleamFile(
    path: GleamPath,
    filepath: String,
    src: String,
    ast: AST,
  )
}

pub type AST {
  AST(
    imports: Imports,
    custom_types: Dict(String, g.Definition(g.CustomType)),
    type_aliases: Dict(String, g.Definition(g.TypeAlias)),
    constants: Dict(String, g.Definition(g.Constant)),
    functions: Dict(String, g.Definition(g.Function)),
  )
}

pub type Imports {
  Imports(
    named: Dict(String, g.Definition(g.Import)),
    discarded: List(g.Definition(g.Import)),
  )
}

pub fn all_imports(
  imports imports: Imports,
) -> List(g.Definition(g.Import)) {
  [
    imports.discarded,
    imports.named |> dict.values,
  ]
  |> list.flatten
}

//

pub opaque type VariantExpr {
  VariantExpr(
    run: fn(
      g.Variant,
      String,
      fn(Option(String), String) -> Result(TypeDef, Nil),
      Set(String), // NOTE: fields acc, used to detect need for `with_spread`
    ) -> Result(#(g.Expression, Set(String)), Nil),
  )
}

// pub fn parse(variant variant: g.Variant, decoder decoder: VariantExpr) {
//   todo
// }

// fn variant_shorthand_field_type(
//   name name: String,
//   cont cont: fn(g.Type) -> VariantExpr,
// ) -> VariantExpr {
//   VariantExpr(fn(variant, args, get_type, fields) {
//     use #(field, f) <- result.try(
//       variant.fields
//       |> list.find_map(fn(field) {
//         case field {
//           g.LabelledVariantField(label:, item:) if label == name ->
//             Ok(#(name, item))

//           _ -> Error(Nil)
//         }
//       }),
//     )

//     cont(f).run(variant, args, get_type, [field, ..fields])
//   })
// }

pub fn try(
  result result: Result(t, e),
  cont cont: fn(t) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(variant, args, get_type, fields) {
    case result {
      Error(_err) -> Error(Nil)
      Ok(x) -> cont(x).run(variant, args, get_type, fields)
    }
  })
}

pub fn variant_shorthand_field(
  name name: String,
  cont cont: fn(g.Field(g.Expression)) -> VariantExpr,
) -> VariantExpr  {
  variant_shorthand_field_map(name, x(short, pair.first), cont)
}

pub fn variant_shorthand_type(
  name name: String,
  cont cont: fn(g.Type) -> VariantExpr,
) -> VariantExpr {
  variant_shorthand_field_map(name, pair.second, cont)
}

pub fn type_params(
  type_ type_: g.Type,
  cont cont: fn(List(#(g.Type, Result(TypeDef, Nil)))) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(variant, args, get_type, fields) {
    case type_ {
      g.NamedType(parameters:, ..) ->
        parameters
        |> list.map(fn(type_) {
          case type_ {
            g.NamedType(module:, name:, ..) ->
              get_type(module, name)

            _ ->
              Error(Nil)
          }
          |> pair.new(type_, _)
        })
        |> fn(x) {
          cont(x).run(variant, args, get_type, fields)
        }

      _ ->
        Error(Nil)
    }
  })
}

pub fn type_param_at(
  type_ type_: g.Type,
  idx idx: Int,
  cont cont: fn(g.Type) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(variant, args, get_type, fields) {
    case type_ {
      g.NamedType(parameters:, ..) ->
        parameters
        |> list_at(idx)
        |> result.map(fn(type_) {
          cont(type_).run(variant, args, get_type, fields)
        })
        |> result.flatten

      _ ->
        Error(Nil)
    }
  })
}

// fn variant_foo(
//   // name name: String,
//   type_ type_: g.Type,
//   apply f: fn(#(String, g.Type)) -> t,
//   cont cont: fn(t) -> VariantExpr,
// ) -> VariantExpr {
//   VariantExpr(fn(variant, args, get_type, orig_fields) {
//     use type_ <- result.try(get_named_param(variant:, name:))

//     // variant_shorthand_field_map(name, function.identity)
//     todo
//   })
// }

fn variant_shorthand_field_map(
  name name: String,
  apply f: fn(#(String, g.Type)) -> t,
  cont cont: fn(t) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(variant, args, get_type, fields) {
    use type_ <- result.try(get_named_param(variant:, name:))

    cont(f(#(name, type_))).run(variant, args, get_type, fields |> set.insert(name))
  })
}

fn get_named_param(
  variant variant: g.Variant,
  name name: String,
) -> Result(g.Type, Nil) {
  variant.fields
  |> list.find_map(fn(field) {
    case field {
      g.LabelledVariantField(label:, item:) if label == name -> Ok(item)

      _ -> Error(Nil)
    }
  })
}

pub fn variant_failure() -> VariantExpr {
  VariantExpr(fn(_, _, _, _) { Error(Nil) })
}

pub fn variant_success(expr expr: g.Expression) -> VariantExpr {
  VariantExpr(fn(_, _, _, fields) { Ok(#(expr, fields)) })
}

pub fn run_variant_expr(
  ve: VariantExpr,
  variant variant: g.Variant,
  args args: String,
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
) -> Result(g.Clause, Nil) {
  ve.run(variant, args, get_type, set.new())
  |> result.map(fn(t) {
    let #(expr, fields) = t

    let with_spread = list.length(variant.fields) > set.size(fields)
    let arguments = fields |> set.map(g.ShorthandField) |> set.to_list

    let pattern =
      g.PatternVariant(z, None, variant.name, arguments:, with_spread:)

    g.Clause(patterns: [[pattern]], guard: None, body: expr)
  })
}

pub fn variant_name(
  cont cont: fn(String) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(variant, args, get_type, fields) {
    cont(variant.name).run(variant, args, get_type, fields)
  })
}

pub fn short(field: String) -> g.Field(t) {
  g.ShorthandField(field)
}
