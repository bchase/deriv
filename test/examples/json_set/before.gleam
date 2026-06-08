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
