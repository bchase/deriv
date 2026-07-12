import glance
import deriv/gen/types.{type ExprGen}
import deriv/internal/glance.{z} as _

pub fn gen() -> ExprGen {
  types.VariantClauseCaseExprGen(expr: types.variant_success(glance.String(z, "")))
}

pub type Foo {
  Foo(foo: String)
}

pub fn f1(foo: Foo) {
  let bar = Nil

  { //$ gen bchase/foo/bar.test0 foo
    case foo {
      Foo(foo:) -> foo_func(foo:, bar:)
    }
  }
}

// pub fn f2(foo: Foo) {
//   let bar = Nil

//   { //$ gen bchase/foo/bar/test0 foo
//     case foo {
//       Foo(foo:) -> foo_func(foo:, bar:)
//     }
//   }
// }

fn foo_func(foo _, bar _) { Nil }
