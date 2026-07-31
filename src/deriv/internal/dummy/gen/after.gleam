import bchase/casing
import deriv/gen/types.{type ExprGen}
import deriv/internal/glance.{call_, short, term} as _

pub fn gen() -> ExprGen {
  types.VariantClauseCaseExprGen([
    {
      use variant, _type_def <- types.variant()
      use foo, _type, _type_def <- types.pun_variant_named_param("foo")

      let func = { variant.name |> casing.snake <> "_func" } |> term

      types.variant_clause_success(func |> call_([ foo, short("bar") ]))
    },
  ])
}

pub type Foo {
  Foo(foo: String)
}

pub fn f1(foo: Foo) {
  let bar = Nil

  { //$ deriv/internal/dummy/lookup.test0 subject:foo
    case foo {
      Foo(foo:) -> foo_func(foo:, bar:)
    }
  }
}

fn foo_func(foo _, bar _) { Nil }
