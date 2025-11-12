import gleam/json.{type Json}
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

pub fn encode_field(
  value: Field(key, val),
  encode_key: fn(key) -> Json,
  encode_val: fn(val) -> Json,
) -> Json {
  case value {
    Field(..) as value ->
      json.object([
        #("key", encode_key(value.key)),
        #("val", encode_val(value.val)),
      ])
  }
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #(
          "list",
          json.array(value.list, encode_field(_, json.string, json.string)),
        ),
        #(
          "option",
          json.nullable(value.option, encode_field(_, json.string, json.string)),
        ),
        #(
          "option_list",
          json.nullable(
            value.option_list,
            json.array(_, encode_field(_, json.string, json.string)),
          ),
        ),
        #("scalar", encode_field(value.scalar, json.string, json.string)),
      ])
  }
}
