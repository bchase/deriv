import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Bar {
  Bar
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.success(Bar)
}

pub fn encode_bar(_value: Bar) -> Json {
  json.null()
}

pub fn decoder_custom_string() -> Decoder(String) {
  decode.string
}

pub fn encode_custom_string(value: String) -> Json {
  json.string(value)
}
