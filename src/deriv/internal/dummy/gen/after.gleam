pub type Foo {
  Foo(foo: String)
}

pub fn exec(foo: Foo) {
  let bar = Nil

  { //$ gen bchase/foo/bar/test0 foo
    case foo {
      Foo(foo:) -> foo_func(foo:, bar:)
    }
  }
}

fn foo_func(foo _, bar _) { Nil }
