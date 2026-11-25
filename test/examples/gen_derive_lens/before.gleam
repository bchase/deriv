pub type Foo {
  Foo(
    bar: String,
    //$ derive.lens
    baz: Int,
  )
}

// DERIVED
