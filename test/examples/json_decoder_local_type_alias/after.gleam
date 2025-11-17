import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

// simple type alias
pub type Fields =
  //$ derive json decode encode
  Dict(String, String)

pub type Listy(t) =
  //$ derive json decode encode
  List(t)

pub fn decoder_fields() -> Decoder(Fields) {
  decode.dict(decode.string, decode.string)
}

pub fn encode_fields(value: Dict(String, String)) -> Json {
  json.dict(value, fn(str) { str }, json.string)
}

pub fn decoder_listy(decoder_t: Decoder(t)) -> Decoder(Listy(t)) {
  decode.list(decoder_t)
}

pub fn encode_listy(value: List(t), encode_t: fn(t) -> Json) -> Json {
  json.array(value, encode_t)
}
