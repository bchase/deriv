import examples/json_rewrite/bar.{type Bar, decoder_bar, decoder_custom_string}
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
