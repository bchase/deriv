import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}

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
