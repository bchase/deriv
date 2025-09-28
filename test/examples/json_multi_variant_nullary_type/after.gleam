import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type T {
  //$ derive json decode encode
  X
  Y
}

pub fn decoder_t() -> Decoder(T) {
  decode.one_of(decoder_t_x(), [decoder_t_y()])
}

pub fn decoder_t_x() -> Decoder(T) {
  use _deriv_var_constr <- decode.field("_var", util.is("X"))
  decode.success(X)
}

pub fn decoder_t_y() -> Decoder(T) {
  use _deriv_var_constr <- decode.field("_var", util.is("Y"))
  decode.success(Y)
}

pub fn encode_t(value: T) -> Json {
  case value {
    X(..) as value -> json.object([#("_var", json.string("X"))])
    Y(..) as value -> json.object([#("_var", json.string("Y"))])
  }
}
