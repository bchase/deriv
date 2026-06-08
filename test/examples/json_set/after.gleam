import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/set.{type Set}

pub type Foo {
  //$ derive json encode decode
  Foo(
    bar: Set(String),
    baz: Set(Int),
    //$ json decoder decoder_set_int
    //$ json encode encode_set_int
    boo: Set(Float),
    //$ json encode inner encode_float
    //$ json decoder inner decoder_float
  )
}

pub fn encode_float(
  float float: Float,
) -> Json {
  json.float(float)
}

pub fn decoder_float() -> Decoder(Float) {
  decode.float
}

pub type Item {
  Item
}

pub fn encode_item(
  item _item: Item,
) -> Json {
  json.null()
}

pub fn decoder_item() -> Decoder(Item) {
  decode.success(Item)
}

pub fn encode_set_int(
  set set: Set(Int),
) -> Json {
  set
  |> set.to_list
  |> json.array(json.int)
}

pub fn decoder_set_int() -> Decoder(Set(Int)) {
  decode.list(decode.int)
  |> decode.map(set.from_list)
}

// DERIVED

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #("bar", deriv.encode_set(value.bar, json.string)),
        #("baz", encode_set_int(value.baz)),
        #("boo", deriv.encode_set(value.boo, encode_float)),
      ])
  }
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use bar <- decode.field("bar", deriv.decoder_set(decode.string))
  use baz <- decode.field("baz", decoder_set_int())
  use boo <- decode.field("boo", deriv.decoder_set(decoder_float()))
  decode.success(Foo(bar:, baz:, boo:))
}
