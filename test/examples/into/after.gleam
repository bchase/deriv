import examples/into/foo as f

pub type Bar {
  //$ derive into examples/into/foo.Foo as f
  Bar(
    title: String,
    //$ into field examples/into/foo.Foo name
    count: Int,
  )
}

pub fn into_foo_from_bar(value: Bar) -> f.Foo {
  f.Foo(name: value.title, count: value.count)
}
