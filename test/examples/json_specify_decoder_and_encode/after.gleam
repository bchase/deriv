import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option}

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

fn some_encode_func_name(x: Option(String)) -> Json {
  todo
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
