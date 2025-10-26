import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option}

pub type Foo {
  //$ derive json decode
  Foo(
    int: Int,
    string: String,
    bool: Bool,
    float: Float,
    option_string: Option(String),
  )
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use int <- decode.field("int", decode.int)
  use string <- decode.field("string", decode.string)
  use bool <- decode.field("bool", decode.bool)
  use float <- decode.field("float", decode.float)
  use option_string <- decode.optional_field(
    "option_string",
    deriv.none,
    decode.optional(decode.string),
  )
  decode.success(Foo(int:, string:, bool:, float:, option_string:))
}
