import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
  )
}

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.one_of(decoder_maybe_maybe(), [])
}

pub fn decoder_maybe_maybe() -> Decoder(Maybe) {
  use name <- decode.optional_field(
    "name",
    deriv.none,
    decode.optional(decode.string),
  )
  decode.success(Maybe(name:))
}

pub fn encode_maybe(value: Maybe) -> Json {
  case value {
    Maybe(..) as value ->
      json.object([#("name", json.nullable(value.name, json.string))])
  }
}
