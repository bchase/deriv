import deriv/util as deriv
import examples/json_rewrite/bar.{type Bar, decoder_bar, decoder_custom_string, encode_bar}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option}

pub type Foo {
  //$ derive json decode encode
  Foo(
    int: Int,
    string: String,
    bool: Bool,
    float: Float,
    option_string: Option(String),
    list_int: List(Int),
    bar: Bar,
    //
    named: String,
    //$ json named property
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
  use bar <- decode.field("bar", decoder_bar())
  use named <- decode.field("property", decode.string)
  use decoder <- decode.field("decoder", decoder_custom_string())
  decode.success(Foo(
    int:,
    string:,
    bool:,
    float:,
    option_string:,
    list_int:,
    bar:,
    named:,
    decoder:,
  ))
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #("int", json.int(value.int)),
        #("string", json.string(value.string)),
        #("bool", json.bool(value.bool)),
        #("float", json.float(value.float)),
        #("option_string", json.nullable(value.option_string, json.string)),
        #("list_int", json.array(value.list_int, json.int)),
        #("bar", encode_bar(value.bar)),
        #("property", json.string(value.named)),
        #("decoder", json.string(value.decoder)),
      ])
  }
}
