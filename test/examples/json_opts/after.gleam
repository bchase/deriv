import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

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
  )
}

pub type FooId {
  FooId(
    id: Int,
  )
}

pub fn decoder_message_event() -> Decoder(MessageEvent) {
  decode.one_of(decoder_message_event_message_event(), [])
}

pub fn decoder_message_event_message_event() -> Decoder(MessageEvent) {
  use type_ <- decode.field(
    "type",
    decode.string |> deriv.decoder_guard("message"),
  )
  use text <- decode.field("text", decode.string)
  decode.success(MessageEvent(type_:, text:))
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use id <- decode.field("id", decode.int |> decode.map(FooId))
  decode.success(Foo(id:))
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value -> json.object([#("id", json.int(value.id.id))])
  }
}
