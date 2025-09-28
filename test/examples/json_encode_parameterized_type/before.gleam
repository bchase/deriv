import gleam/option.{type Option}

pub type Field(key, val) {
  //$ derive json encode
  Field(
    key: key,
    val: val,
  )
}

pub type Foo {
  //$ derive json encode
  Foo(
    scalar: Field(String, String),
    list: List(Field(String, String)),
    option: Option(Field(String, String)),
    option_list: Option(List(Field(String, String))),
  )
}
