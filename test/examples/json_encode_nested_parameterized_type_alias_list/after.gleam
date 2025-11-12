import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Validation(t) = fn(t) -> Result(t, List(String))

pub type Field1(t) {
  //$ derive json encode decode
  Field1(
    id: String,
    validations: List(Validation(t)),
    //$ json decoder decoder_list_fake_validation
    //$ json encode encode_list_fake_validation
    validations_inner: List(Validation(t)),
    //$ json decoder inner decoder_fake_validation
    //$ json encode inner encode_fake_validation
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

pub fn encode_list_fake_validation(
  _value: List(Validation(t)),
) -> Json {
  json.array([], encode_fake_validation)
}

pub fn decoder_fake_validation() -> Decoder(Validation(t)) {
  decode.success(fn(_) { Error([]) })
}

pub fn decoder_list_fake_validation() -> Decoder(List(Validation(t))) {
  decode.list(decode.success(fn(_) { Error([]) }))
}

pub fn encode_field1(value: Field1(t), encode_t: fn(t) -> Json) -> Json {
  case value {
    Field1(..) as value ->
      json.object([
        #("errs", json.array(value.errs, json.string)),
        #("id", json.string(value.id)),
        #("touched", json.bool(value.touched)),
        #("validations", encode_list_fake_validation(value.validations)),
        #(
          "validations_inner",
          // json.array(value.validations_inner, encode_fake_validation(encode_t)),
          todo as "think e.g. `encode_fake_validation` needs to take a `fn(t) -> Json`",
        ),
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
    decoder_list_fake_validation(),
  )
  use validations_inner <- decode.optional_field(
    "validations_inner",
    [],
    decode.list(decoder_fake_validation()),
  )
  use touched <- decode.field("touched", decode.bool)
  use value <- decode.field("value", decoder_t)
  use errs <- decode.optional_field("errs", [], decode.list(decode.string))
  decode.success(Field1(
    id:,
    validations:,
    validations_inner:,
    touched:,
    value:,
    errs:,
  ))
}
