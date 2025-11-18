import deriv/util as deriv
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

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.one_of(decoder_maybe_maybe(), [])
}

pub fn decoder_maybe_maybe() -> Decoder(Maybe) {
  use name <- decode.optional_field("name", deriv.none, some_decoder_name())
  decode.success(Maybe(name:))
}

pub fn encode_maybe(value: Maybe) -> Json {
  case value {
    Maybe(..) as value ->
      json.object([#("name", some_encode_func_name(value.name))])
  }
}
