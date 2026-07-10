type Foo {
  Foo(foo: String)
}

pub fn slice_replace(bool: Bool) {
  let foo = "bar"

  // some other logic and comments to differ the src & AST

  { //$ gen bchase/foo/bar/test0 variant:deriv/internal/dummy/lookup.Foo bool
    case bool {
      True -> todo
      False -> todo
    }
  }
}
