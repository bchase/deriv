import deriv/internal/dummy/lookup_other.{type OtherImport}
import deriv/internal/dummy/lookup_other_other as oo
import glance
import bchase/id
import gleam/option
import deriv/gen/types

// pub fn foo() -> types.ExprGen {
//   types.VariantClauseCaseExprGen({
//     use str <- types.variant_shorthand_field("str")
//     use str <- types.variant_shorthand_field("str")
//     types.variant_success(todo)
//   })
// }

pub fn bar() -> String {

}

pub type Local {
  Local
}

pub type Foo {
  Foo(foo: String)
}

pub type LocalAlias = Local

pub fn gen() {
  let foo = todo

  // some other comment

  { //$ gen target
    case foo {
      _ -> foo
    }
  }
}
