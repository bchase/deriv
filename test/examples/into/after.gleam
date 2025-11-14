import examples/into/foo as f

pub type Bar {
  //$ derive into examples/into/foo.Foo as f
  //$ derive into Missing
  Bar(
    title: String,
    //$ into field examples/into/foo.Foo name
    count: Int,
  )
}

pub type Missing {
  Missing(
    title: String,
    count: Int,
    bonus: Bool,
  )
}

pub fn into_foo_from_bar(value: Bar) -> f.Foo {
  f.Foo(name: value.title, count: value.count)
}

pub fn into_missing_from_bar(value: Bar, bonus bonus: Bool) -> Missing {
  Missing(title: value.title, count: value.count, bonus:)
}
