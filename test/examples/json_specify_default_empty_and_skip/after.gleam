import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Empty {
  //$ derive json decode encode
  Empty(
    list: List(String),
    //$ json decode default empty
    //$ json encode skip
  )
}

pub fn decoder_empty() -> Decoder(Empty) {
  decode.one_of(decoder_empty_empty(), [])
}

pub fn decoder_empty_empty() -> Decoder(Empty) {
  use list <- decode.field(
    "list",
    decode.one_of(decode.list(decode.string), [decode.success([])]),
  )
  decode.success(Empty(list:))
}

pub fn encode_empty(value: Empty) -> Json {
  case value {
    Empty(..) as value -> json.object([])
  }
}
