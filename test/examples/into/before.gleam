import examples/into/foo as f

pub type Bar {
  //$ derive into examples/into/foo.Foo as f
  Bar(
    title: String,
    //$ into field examples/into/foo.Foo name
    count: Int,
  )
}
