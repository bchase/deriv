import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type T {
  //$ derive json decode encode
  X(foo: String)
  Y(bar: Int)
}

pub fn decoder_t() -> Decoder(T) {
  decode.one_of(decoder_t_x(), [decoder_t_y()])
}

pub fn decoder_t_x() -> Decoder(T) {
  use _deriv_var_constr <- decode.field("_var", util.is("X"))
  use foo <- decode.field("foo", decode.string)
  decode.success(X(foo:))
}

pub fn decoder_t_y() -> Decoder(T) {
  use _deriv_var_constr <- decode.field("_var", util.is("Y"))
  use bar <- decode.field("bar", decode.int)
  decode.success(Y(bar:))
}

pub fn encode_t(value: T) -> Json {
  case value {
    X(..) as value ->
      json.object([
        #("_var", json.string("X")),
        #("foo", json.string(value.foo)),
      ])
    Y(..) as value ->
      json.object([#("_var", json.string("Y")), #("bar", json.int(value.bar))])
  }
}

