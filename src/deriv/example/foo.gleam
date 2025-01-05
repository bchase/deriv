pub type Foo {
  Foo(
    id: Int, //$ json(decode(decoder(foo/bar.baz)))
    name: String, //$ json(decode(decoder(asdf)))
    active: Bool,
  )
} //$ derive json(decode,encode)

pub type Bar {
  Bar(
    baz: Bool,
  )
} //$ derive json(decode)
