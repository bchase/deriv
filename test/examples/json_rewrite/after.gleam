import deriv/util as deriv
import examples/json_rewrite/bar.{type Bar, decoder_bar, decoder_custom_string}
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
    list_int: List(Int),
    //
    named: String,
    //$ json named property
    nested: Float,
    //$ json named some.nested.prop
    decoder: String,
    //$ json decoder decoder_custom_string
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
  use list_int <- decode.optional_field("list_int", [], decode.list(decode.int))
  use named <- decode.field("property", decode.string)
  use nested <- decode.subfield(["some", "nested", "prop"], decode.float)
  use decoder <- decode.field("decoder", decoder_custom_string())
  decode.success(Foo(
    int:,
    string:,
    bool:,
    float:,
    option_string:,
    list_int:,
    named:,
    nested:,
    decoder:,
  ))
}
