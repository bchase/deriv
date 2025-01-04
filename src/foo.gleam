pub type Foo {
  Foo(
    id: Int, //$ json(decode(decoder(foo/bar.baz)))
    name: String,
    active: Bool,
  )
} //$ derive json(decode,encode)

pub type Bar {
  Bar(
    baz: Bool,
  )
} //$ derive json(decode)
