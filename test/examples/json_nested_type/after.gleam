import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type A {
  //$ derive json decode encode
  A(
    b: B,
  )
}

pub type B {
  //$ derive json decode encode
  B(
    x: String,
  )
}

pub fn decoder_a() -> Decoder(A) {
  decode.one_of(decoder_a_a(), [])
}

pub fn decoder_a_a() -> Decoder(A) {
  use b <- decode.field("b", decoder_b())
  decode.success(A(b:))
}

pub fn encode_a(value: A) -> Json {
  case value {
    A(..) as value -> json.object([#("b", encode_b(value.b))])
  }
}

pub fn decoder_b() -> Decoder(B) {
  decode.one_of(decoder_b_b(), [])
}

pub fn decoder_b_b() -> Decoder(B) {
  use x <- decode.field("x", decode.string)
  decode.success(B(x:))
}

pub fn encode_b(value: B) -> Json {
  case value {
    B(..) as value -> json.object([#("x", json.string(value.x))])
  }
}
