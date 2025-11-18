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
