import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option}

pub fn suppress_warnings() -> Decoder(String) { decode.string }

pub type MessageEvent {
  //$ derive json decode
  MessageEvent(
    type_: String,
    //$ json named type
    //$ json guard "message"
    text: String,
  )
}

pub type Foo {
  //$ derive json decode encode
  Foo(
    id: FooId,
    //$ newtype
    option_id: Option(FooId),
    //$ newtype
    list_id: List(FooId),
    //$ newtype
    option_id_named: Option(FooId),
    //$ json named foo.id
    //$ newtype
  )
}

pub type FooId {
  FooId(
    id: Int,
  )
}

pub type Row(resource) {
  //$ derive json decode
  Row(
    id: Id(resource),
    //$ newtype
  )
}

pub type Id(resource) {
  Id(id: String)
}

pub type R(resource) {
  //$ derive json decode encode
  R(
    id: Id(resource),
    //$ newtype
    //$ json decoder decoder_id_custom
    //$ json encode encode_id_custom
    option_id: Option(Id(resource)),
    //$ newtype
  )
}

pub fn decoder_id_custom() -> Decoder(Id(resource)) {
  decode.string |> decode.map(Id)
}
pub fn encode_id_custom(
  value value: Id(resource),
) -> Json {
  json.string(value.id)
}
