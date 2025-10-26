import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Fields(t) =
  Dict(String, Field(t))

pub type Field(t) {
  Field(
    id: String,
    touched: Bool,
    value: t,
  )
}

pub type Form {
  //$ derive json encode decode
  Form(
    text_fields: Fields(String),
    list_fields: Fields(List(String)),
    override: Fields(String),
    //$ json decoder some_specific_decoder
    //$ json encode some_specific_encode
  )
}

fn decoder_fields(inner: Decoder(t)) -> Decoder(Fields(t)) {
  todo
}

fn some_specific_decoder() -> Decoder(Fields(String)) {
  todo
}

fn encode_fields(x: Fields(t), inner: fn(t) -> Json) -> Json {
  todo
}

fn some_specific_encode(x: String) -> Json {
  todo
}
