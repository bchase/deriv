import examples/json_rewrite/bar.{type Bar, Bar, decoder_bar, decoder_custom_string, encode_bar, encode_custom_string}
import gleam/option.{type Option}

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
  )
}

fn zero_bar() -> Bar {
  Bar
}
