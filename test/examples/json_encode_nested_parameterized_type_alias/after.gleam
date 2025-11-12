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
    //$ json decoder decoder_override
    //$ json encode encode_override
  )
}

fn decoder_fields(inner: Decoder(t)) -> Decoder(Fields(t)) {
  todo
}

fn encode_fields(x: Fields(t), inner: fn(t) -> Json) -> Json {
  todo
}

fn decoder_override() -> Decoder(Fields(String)) {
  todo
}

fn encode_override(x: Fields(String)) -> Json {
  todo
}

pub fn encode_form(value: Form) -> Json {
  case value {
    Form(..) as value ->
      json.object([
        #(
          "list_fields",
          encode_fields(value.list_fields, json.array(_, json.string)),
        ),
        #("override", encode_override(value.override)),
        #("text_fields", encode_fields(value.text_fields, json.string)),
      ])
  }
}

pub fn decoder_form() -> Decoder(Form) {
  decode.one_of(decoder_form_form(), [])
}

pub fn decoder_form_form() -> Decoder(Form) {
  use text_fields <- decode.field("text_fields", decoder_fields(decode.string))
  use list_fields <- decode.field(
    "list_fields",
    decoder_fields(decode.list(decode.string)),
  )
  use override <- decode.field("override", decoder_override())
  decode.success(Form(text_fields:, list_fields:, override:))
}
