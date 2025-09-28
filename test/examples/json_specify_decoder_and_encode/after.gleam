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

fn some_decoder_name() -> Decoder(Option(String)) {
  todo
}

fn some_encode_func_name(x: String) -> Json {
  todo
}

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.one_of(decoder_maybe_maybe(), [])
}

pub fn decoder_maybe_maybe() -> Decoder(Maybe) {
  use name <- decode.optional_field("name", None, some_decoder_name())
  decode.success(Maybe(name:))
}

pub fn encode_maybe(value: Maybe) -> Json {
  case value {
    Maybe(..) as value ->
      json.object([#("name", json.nullable(value.name, some_encode_func_name))])
  }
}
