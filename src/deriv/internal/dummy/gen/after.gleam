pub type Foo {
  Foo(foo: String)
}

pub fn exec(foo: Foo) {
  let bar = Nil

  { case foo { //$ gen bchase/foo/bar/test0 foo
      Foo(foo:) -> foo_func(foo:, bar:)
    }
  }
}

fn foo_func(foo _, bar _) { Nil }
