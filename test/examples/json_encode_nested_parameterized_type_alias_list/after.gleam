import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Validation(t) = fn(t) -> Result(t, List(String))

pub type Field1(t) {
  //$ derive json encode decode
  Field1(
    id: String,
    validations: List(Validation(t)),
    //$ json decoder decoder_fake_validation
    //$ json encode encode_fake_validation
    touched: Bool,
    value: t,
    errs: List(String),
  )
}

pub fn encode_fake_validation(
  _value: Validation(t),
) -> Json {
  json.null()
}

pub fn decoder_fake_validation() -> Decoder(Validation(t)) {
  decode.success(fn(_) { Error([]) })
}

pub fn encode_field1(value: Field1(t), encode_t: fn(t) -> Json) -> Json {
  case value {
    Field1(..) as value ->
      json.object([
        #("errs", json.array(value.errs, json.string)),
        #("id", json.string(value.id)),
        #("touched", json.bool(value.touched)),
        #("validations", json.array(value.validations, encode_fake_validation)),
        #("value", encode_t(value.value)),
      ])
  }
}

pub fn decoder_field1(decoder_t: Decoder(t)) -> Decoder(Field1(t)) {
  decode.one_of(decoder_field1_field1(decoder_t), [])
}

pub fn decoder_field1_field1(decoder_t: Decoder(t)) -> Decoder(Field1(t)) {
  use id <- decode.field("id", decode.string)
  use validations <- decode.optional_field(
    "validations",
    [],
    decode.list(decoder_fake_validation()),
  )
  use touched <- decode.field("touched", decode.bool)
  use value <- decode.field("value", decoder_t)
  use errs <- decode.field("errs", decode.list(decode.string))
  decode.success(Field1(id:, validations:, touched:, value:, errs:))
}
