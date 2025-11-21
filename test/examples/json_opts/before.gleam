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
