import youid/uuid.{type Uuid}

pub type Foo {
  Foo(
    uuid: Uuid,
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
