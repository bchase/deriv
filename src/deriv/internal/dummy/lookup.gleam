import bchase/casing
import bchase/id
import deriv/gen/types.{type ExprGen}
import deriv/internal/dummy/lookup_other.{type OtherImport}
import deriv/internal/dummy/lookup_other_other as oo
import deriv/internal/glance as ast
import glance
import gleam/int
import gleam/option

pub fn to_str() -> ExprGen {
  types.VariantClauseCaseExprGen([
    // // todo
    // //   - register (ensure) import
    // //     * avoid name collision / duplicate import diff name
    // //   - check type
    // {
    //   use _str <- types.variant_shorthand_field("str")
    //   types.variant_success({
    //     "str" |> ast.term
    //   })
    // },
    // {
    //   use _int <- types.variant_shorthand_field("int")
    //   types.variant_success({
    //     "int" |> ast.dot("to_string") |> ast.call([ ast.term("int") ])
    //   })
    // },
  ])
}

pub fn test0() -> ExprGen {
  types.VariantClauseCaseExprGen(clauses: [
    {
      use variant, _type_def <- types.variant()
      use foo, _type, _type_def <- types.pun_variant_named_param("foo")

      let func = { variant.name |> casing.snake <> "_func" } |> ast.term

      types.variant_clause_success(func |> ast.call_([ foo, ast.short("bar") ]))
    },
  ])
}

type Thing {
  Var1(str: String)
  Var2(int: Int)
}

pub fn bar() -> String {

}

pub type Local {
  Local
}

pub type Foo {
  Foo(foo: String)
}

pub type LocalAlias = Local

// pub fn gen() {
//   let foo = todo

//   // some other comment

//   { //$ gen target
//     case foo {
//       _ -> foo
//     }
//   }
// }