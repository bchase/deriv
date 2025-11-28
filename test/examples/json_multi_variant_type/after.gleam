import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type T {
  //$ derive json decode encode
  //$ json foo
  X(foo: String)
  Y(bar: Int)
}

pub type TT {
  //$ derive json decode encode
  //$ json variant key Some("_foo")
  A(foo: String)
  B(bar: Int)
}

pub type TTT {
  //$ derive json decode encode
  //$ json variant key None
  M(foo: String)
  N(bar: Int)
}

pub fn decoder_t() -> Decoder(T) {
  decode.one_of(decoder_t_x(), [decoder_t_y()])
}

pub fn decoder_t_x() -> Decoder(T) {
  use _deriv_var_constr <- decode.field("_var", deriv.is("X"))
  use foo <- decode.field("foo", decode.string)
  decode.success(X(foo:))
}

pub fn decoder_t_y() -> Decoder(T) {
  use _deriv_var_constr <- decode.field("_var", deriv.is("Y"))
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

pub fn decoder_t_t() -> Decoder(TT) {
  decode.one_of(decoder_t_t_a(), [decoder_t_t_b()])
}

pub fn decoder_t_t_a() -> Decoder(TT) {
  use _deriv_var_constr <- decode.field("_foo", deriv.is("A"))
  use foo <- decode.field("foo", decode.string)
  decode.success(A(foo:))
}

pub fn decoder_t_t_b() -> Decoder(TT) {
  use _deriv_var_constr <- decode.field("_foo", deriv.is("B"))
  use bar <- decode.field("bar", decode.int)
  decode.success(B(bar:))
}

pub fn encode_t_t(value: TT) -> Json {
  case value {
    A(..) as value ->
      json.object([
        #("_foo", json.string("A")),
        #("foo", json.string(value.foo)),
      ])
    B(..) as value ->
      json.object([#("_foo", json.string("B")), #("bar", json.int(value.bar))])
  }
}

pub fn decoder_t_t_t() -> Decoder(TTT) {
  decode.one_of(decoder_t_t_t_m(), [decoder_t_t_t_n()])
}

pub fn decoder_t_t_t_m() -> Decoder(TTT) {
  use foo <- decode.field("foo", decode.string)
  decode.success(M(foo:))
}

pub fn decoder_t_t_t_n() -> Decoder(TTT) {
  use bar <- decode.field("bar", decode.int)
  decode.success(N(bar:))
}

pub fn encode_t_t_t(value: TTT) -> Json {
  case value {
    M(..) as value -> json.object([#("foo", json.string(value.foo))])
    N(..) as value -> json.object([#("bar", json.int(value.bar))])
  }
}
