import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option}

pub type Field(key, val) {
  //$ derive json decode
  Field(
    key: key,
    val: val,
  )
}

pub type Foo {
  //$ derive json decode
  Foo(
    scalar: Field(String, String),
    list: List(Field(String, String)),
    option: Option(Field(String, String)),
    option_list: Option(List(Field(String, String))),
  )
}

pub fn decoder_field(
  decoder_key: Decoder(key),
  decoder_val: Decoder(val),
) -> Decoder(Field(key, val)) {
  decode.one_of(decoder_field_field(decoder_key, decoder_val), [])
}

pub fn decoder_field_field(
  decoder_key: Decoder(key),
  decoder_val: Decoder(val),
) -> Decoder(Field(key, val)) {
  use key <- decode.field("key", decoder_key)
  use val <- decode.field("val", decoder_val)
  decode.success(Field(key:, val:))
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use scalar <- decode.field(
    "scalar",
    decoder_field(decode.string, decode.string),
  )
  use list <- decode.field(
    "list",
    decode.list(decoder_field(decode.string, decode.string)),
  )
  use option <- decode.optional_field(
    "option",
    deriv.none,
    decode.optional(decoder_field(decode.string, decode.string)),
  )
  use option_list <- decode.optional_field(
    "option_list",
    deriv.none,
    decode.optional(decode.list(decoder_field(decode.string, decode.string))),
  )
  decode.success(Foo(scalar:, list:, option:, option_list:))
}
