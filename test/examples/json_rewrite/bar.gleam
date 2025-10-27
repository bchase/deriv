import gleam/dynamic/decode.{type Decoder}

pub type Bar {
  Bar
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.success(Bar)
}

pub fn decoder_custom_string() -> Decoder(String) {
  decode.string
}
