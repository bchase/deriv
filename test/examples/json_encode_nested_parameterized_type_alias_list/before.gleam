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
