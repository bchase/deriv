import deriv/internal/dummy/lookup_other.{type Other}

pub type Foo {
  Foo(foo: String)
}

pub fn f1(foo: Other) {
  let bar = Nil

  { //$ gen bchase/foo/bar/test0 foo
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
