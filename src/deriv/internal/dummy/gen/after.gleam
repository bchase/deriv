import bchase/casing
import deriv/gen/types.{type ExprGen}
import deriv/internal/glance.{term, short, call_} as _

pub fn gen() -> ExprGen {
  types.VariantClauseCaseExprGen([
    {
      use variant <- types.variant_name()
      use foo <- types.variant_shorthand_field("foo")

      let func = { variant |> casing.snake <> "_func" } |> term

      types.variant_success(func |> call_([ foo, short("bar") ]))
    },
  ])
}

pub type Foo {
  Foo(foo: String)
}

pub fn f1(foo: Foo) {
  let bar = Nil

  { //$ gen deriv/internal/dummy/lookup.test0 foo
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