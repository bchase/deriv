import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option, None}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
    //$ json decoder some_decoder_name
    //$ json encode some_encode_func_name
  )
}

pub fn some_decoder_name() -> Decoder(Option(String)) {
  decode.success(None)
}

pub fn some_encode_func_name(x: Option(String)) -> Json {
  json.nullable(x, json.string)
}
