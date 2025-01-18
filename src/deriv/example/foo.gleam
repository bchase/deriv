import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type Foo {
  Foo(
    uuid: Uuid,
    id: Int, //$ json(decode(decoder(foo/bar.baz)))
    name: String, //$ json(decode(decoder(asdf)))
    active: Bool,
    ratio: Float,
    maybe: Option(String),
    words: List(String),
  )
} //$ derive json(decode,encode)

pub type Bar {
  Bar(baz: Bool)
} //$ derive json(decode)
