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
