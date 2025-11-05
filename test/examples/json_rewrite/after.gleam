import deriv/util as deriv
import examples/json_rewrite/bar.{type Bar, Bar, decoder_bar, decoder_custom_string, encode_bar, encode_custom_string}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option, None}

pub type Foo {
  //$ derive zero
  //$ derive json decode encode
  Foo(
    int: Int,
    string: String,
    bool: Bool,
    float: Float,
    option_string: Option(String),
    list_int: List(Int),
    option_list_float: Option(List(Float)),
    list_option_bool: List(Option(Bool)),
    bar: Bar,
    //
    named: String,
    //$ json named property
    nested: Float,
    //$ json named some.nested.prop
    decoder: String,
    //$ json decoder decoder_custom_string
    encode: String,
    //$ json encode encode_custom_string
    nested_option: Option(String),
    //$ json named nested.prop
    nested_option_list: Option(List(String)),
    //$ json named option.list
    nested_list: List(String),
    //$ json named list.strings
  )
}

fn zero_bar() -> Bar {
  Bar
}

pub fn zero_foo() -> Foo {
  Foo(
    0,
    "",
    False,
    0.0,
    None,
    [],
    None,
    [],
    zero_bar(),
    "",
    0.0,
    "",
    "",
    None,
    None,
    [],
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
  use option_list_float <- decode.optional_field(
    "option_list_float",
    deriv.none,
    decode.optional(decode.list(decode.float)),
  )
  use list_option_bool <- decode.field(
    "list_option_bool",
    decode.list(decode.optional(decode.bool)),
  )
  use bar <- decode.field("bar", decoder_bar())
  use named <- decode.field("property", decode.string)
  use nested <- decode.subfield(["some", "nested", "prop"], decode.float)
  use decoder <- decode.field("decoder", decoder_custom_string())
  use encode <- decode.field("encode", decode.string)
  use nested_option <- decode.then(decode.at(
    ["nested", "prop"],
    decode.optional(decode.string),
  ))
  use nested_option_list <- deriv.decode_optional_subfield(
    ["option", "list"],
    deriv.none,
    decode.optional(decode.list(decode.string)),
  )
  use nested_list <- decode.subfield(
    ["list", "strings"],
    decode.list(decode.string),
  )
  decode.success(Foo(
    int:,
    string:,
    bool:,
    float:,
    option_string:,
    list_int:,
    option_list_float:,
    list_option_bool:,
    bar:,
    named:,
    nested:,
    decoder:,
    encode:,
    nested_option:,
    nested_option_list:,
    nested_list:,
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
        #(
          "option_list_float",
          json.nullable(value.option_list_float, json.array(_, json.float)),
        ),
        #(
          "list_option_bool",
          json.array(value.list_option_bool, json.nullable(_, json.bool)),
        ),
        #("bar", encode_bar(value.bar)),
        #("property", json.string(value.named)),
        #(
          "some",
          json.object([
            #("nested", json.object([#("prop", json.float(value.nested))])),
          ]),
        ),
        #("decoder", json.string(value.decoder)),
        #("encode", encode_custom_string(value.encode)),
        #(
          "nested",
          json.object([
            #("prop", json.nullable(value.nested_option, json.string)),
          ]),
        ),
        #(
          "option",
          json.object([
            #(
              "list",
              json.nullable(
                value.nested_option_list,
                json.array(_, json.string),
              ),
            ),
          ]),
        ),
        #(
          "list",
          json.object([#("strings", json.array(value.nested_list, json.string))]),
        ),
      ])
  }
}
