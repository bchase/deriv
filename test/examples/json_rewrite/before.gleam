import examples/json_rewrite/bar.{type Bar, Bar, decoder_bar, decoder_custom_string, encode_bar, encode_custom_string}
import gleam/option.{type Option}

// IGNORE, HERE TO SUPPRESS WARNINGS
pub const bar = Bar
pub const decoder_bar = decoder_bar
pub const decoder_custom_string = decoder_custom_string
pub const encode_bar = encode_bar
pub const encode_custom_string = encode_custom_string

pub fn zero_bar() -> Bar {
  Bar
}

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
    //$ json named nested.option
    nested_option_list: Option(List(String)),
    //$ json named option.list
    nested_list: List(String),
    //$ json named nested.list
  )
}
