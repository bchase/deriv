import deriv/util as deriv
import gleam/json.{type Json}
import gleam/dynamic/decode.{type Decoder}
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

pub fn encode_uuid(uuid: Uuid) -> Json {
  deriv.encode_uuid(uuid)
}

pub fn decoder_uuid() -> Decoder(Uuid) {
  deriv.decoder_uuid()
}
