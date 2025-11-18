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
    override_inner: Fields(String),
    //$ json decoder inner decoder_override_inner
    //$ json encode inner encode_override_inner
  )
}

// IGNORE
pub fn decoder_fields(_) -> Decoder(Fields(t)) { decode.success(dict.new()) }
pub fn encode_fields(_, _) { json.null() }
pub fn decoder_override() { decode.success(dict.new()) }
pub fn encode_override(_) -> Json { json.null() }
pub fn decoder_override_inner() { decode.string }
pub const encode_override_inner = json.string

pub fn encode_form(value: Form) -> Json {
  case value {
    Form(..) as value ->
      json.object([
        #(
          "list_fields",
          encode_fields(value.list_fields, json.array(_, json.string)),
        ),
        #("override", encode_override(value.override)),
        #(
          "override_inner",
          encode_fields(value.override_inner, encode_override_inner),
        ),
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
  use override_inner <- decode.field(
    "override_inner",
    decoder_fields(decoder_override_inner()),
  )
  decode.success(Form(text_fields:, list_fields:, override:, override_inner:))
}
