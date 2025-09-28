import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/string

// simple type alias
pub type Fields =
  //$ derive json decode encode
  Dict(String, String)

// recursive type aliases
pub type Foo =
  //$ derive json decode encode
  Bar
pub type Bar =
  //$ derive json decode encode
  String

pub type Listy(t) =
  //$ derive json decode encode
  List(t)

pub fn decoder_fields() -> Decoder(Fields) {
  decode.dict(decode.string, decode.string)
}

pub fn encode_fields(value: Fields) -> Json {
  json.dict(value, string.inspect, json.string)
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.string
}

pub fn encode_foo(value: Foo) -> Json {
  encode_bar(value)
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.string
}

pub fn encode_bar(value: Bar) -> Json {
  json.string(value)
}

pub fn decoder_listy(decoder_t: Decoder(t)) -> Decoder(Listy(t)) {
  decode.list(decoder_t)
}

pub fn encode_listy(value: Listy(t), encode_t: fn(t) -> Json) -> Json {
  json.array(value, encode_t)
}
