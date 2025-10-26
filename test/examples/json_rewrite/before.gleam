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
    override: String,
    //$ json named property
    nested: Float,
    //$ json named some.nested.prop
  )
}
