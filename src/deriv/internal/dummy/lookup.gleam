import gleam/int
import deriv/internal/dummy/lookup_other.{type OtherImport}
import deriv/internal/dummy/lookup_other_other as oo
import glance
import bchase/id
import gleam/option
import deriv/gen/types.{type ExprGen}
import deriv/internal/glance as ast

pub fn to_str() -> ExprGen {
  types.VariantClauseCaseExprGen([
    // todo
    //   - register (ensure) import
    //     * avoid name collision / duplicate import diff name
    //   - check type
    {
      use _str <- types.variant_shorthand_field("str")
      types.variant_success({
        "str" |> ast.term
      })
    },
    {
      use _int <- types.variant_shorthand_field("int")
      types.variant_success({
        "int" |> ast.dot("to_string") |> ast.call([ ast.term("int") ])
      })
    },
  ])
}

fn test0(
  thing thing: Thing,
) -> String {
  { //$ gen deriv/internal/dummy/lookup.to_str thing
    case thing {
      Var1(str:) -> str
      Var2(int:) -> int.to_string(int)
    }
  }
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
