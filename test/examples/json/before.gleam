import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type Foo {
  //$ derive json decode encode
  Foo(
    uuid: Uuid,
    id: Int, //$ json named int_id
    name: String,
    active: Bool,
    ratio: Float,
    words: List(String),
    maybe_list: Option(List(String)),
  )
}

pub type Bar {
  //$ derive json decode
  Bar(
    baz: Bool,
  )
}
