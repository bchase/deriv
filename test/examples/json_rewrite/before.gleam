import examples/json_rewrite/bar.{type Bar, decoder_bar, decoder_custom_string, encode_bar}
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
