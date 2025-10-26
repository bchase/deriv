import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option, None}
import youid/uuid.{type Uuid}

pub type Foo {
  //$ derive json decode encode
  Foo(
    uuid: Uuid,
    id: Int, //$ json named int_id
    name: String,
    active: Bool,
    ratio: Float,
    words: List(String),
    maybe_list: Option(List(String)),
  )
}

pub type Bar {
  //$ derive json decode
  Bar(
    baz: Bool,
  )
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use uuid <- decode.field("uuid", util.decoder_uuid())
  use id <- decode.field("int_id", decode.int)
  use name <- decode.field("name", decode.string)
  use active <- decode.field("active", decode.bool)
  use ratio <- decode.field("ratio", decode.float)
  use words <- decode.field("words", decode.list(decode.string))
  use maybe_list <- decode.optional_field(
    "maybe_list",
    None,
    decode.optional(decode.list(decode.string)),
  )
  decode.success(Foo(uuid:, id:, name:, active:, ratio:, words:, maybe_list:))
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #("uuid", util.encode_uuid(value.uuid)),
        #("int_id", json.int(value.id)),
        #("name", json.string(value.name)),
        #("active", json.bool(value.active)),
        #("ratio", json.float(value.ratio)),
        #("words", json.array(value.words, json.string)),
        #(
          "maybe_list",
          json.nullable(value.maybe_list, json.array(_, json.string)),
        ),
      ])
  }
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.one_of(decoder_bar_bar(), [])
}

pub fn decoder_bar_bar() -> Decoder(Bar) {
  use baz <- decode.field("baz", decode.bool)
  decode.success(Bar(baz:))
}
