import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option}

pub fn suppress_warnings() -> Decoder(String) { decode.string }

pub type MessageEvent {
  //$ derive json decode
  MessageEvent(
    type_: String,
    //$ json named type
    //$ json guard "message"
    text: String,
  )
}

pub type Foo {
  //$ derive json decode encode
  Foo(
    id: FooId,
    //$ newtype
    option_id: Option(FooId),
    //$ newtype
    list_id: List(FooId),
    //$ newtype
    option_id_named: Option(FooId),
    //$ json named foo.id
    //$ newtype
  )
}

pub type FooId {
  FooId(
    id: Int,
  )
}

pub type Row(resource) {
  //$ derive json decode
  Row(
    id: Id(resource),
    //$ newtype
  )
}

pub type Id(resource) {
  Id(id: String)
}

pub type R(resource) {
  //$ derive json decode encode
  R(
    id: Id(resource),
    //$ newtype
    //$ json decoder decoder_id_custom
    //$ json encode encode_id_custom
    option_id: Option(Id(resource)),
    //$ newtype
  )
}

pub fn decoder_id_custom() -> Decoder(Id(resource)) {
  decode.string |> decode.map(Id)
}
pub fn encode_id_custom(
  value value: Id(resource),
) -> Json {
  json.string(value.id)
}

pub fn decoder_message_event() -> Decoder(MessageEvent) {
  decode.one_of(decoder_message_event_message_event(), [])
}

pub fn decoder_message_event_message_event() -> Decoder(MessageEvent) {
  use type_ <- decode.field(
    "type",
    decode.string |> deriv.decoder_guard("message"),
  )
  use text <- decode.field("text", decode.string)
  decode.success(MessageEvent(type_:, text:))
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use id <- decode.field("id", decode.int |> decode.map(FooId))
  use option_id <- decode.optional_field(
    "option_id",
    deriv.none,
    decode.int |> decode.map(FooId) |> decode.optional,
  )
  use list_id <- decode.field(
    "list_id",
    decode.int |> decode.map(FooId) |> decode.list,
  )
  use option_id_named <- deriv.decode_optional_subfield(
    ["foo", "id"],
    deriv.none,
    decode.int |> decode.map(FooId) |> decode.optional,
  )
  decode.success(Foo(id:, option_id:, list_id:, option_id_named:))
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #(
          "foo",
          json.object([
            #(
              "id",
              json.nullable(
                value.option_id_named |> option.map(fn(x) { x.id }),
                json.int,
              ),
            ),
          ]),
        ),
        #("id", json.int(value.id.id)),
        #(
          "list_id",
          json.array(value.list_id |> deriv.list_map(fn(x) { x.id }), json.int),
        ),
        #(
          "option_id",
          json.nullable(value.option_id |> option.map(fn(x) { x.id }), json.int),
        ),
      ])
  }
}

pub fn decoder_row() -> Decoder(Row(resource)) {
  decode.one_of(decoder_row_row(), [])
}

pub fn decoder_row_row() -> Decoder(Row(resource)) {
  use id <- decode.field("id", decode.string |> decode.map(Id))
  decode.success(Row(id:))
}

pub fn decoder_r() -> Decoder(R(resource)) {
  decode.one_of(decoder_r_r(), [])
}

pub fn decoder_r_r() -> Decoder(R(resource)) {
  use id <- decode.field("id", decoder_id_custom())
  use option_id <- decode.optional_field(
    "option_id",
    deriv.none,
    decode.string |> decode.map(Id) |> decode.optional,
  )
  decode.success(R(id:, option_id:))
}

pub fn encode_r(value: R(resource)) -> Json {
  case value {
    R(..) as value ->
      json.object([
        #("id", encode_id_custom(value.id)),
        #(
          "option_id",
          json.nullable(
            value.option_id |> option.map(fn(x) { x.id }),
            json.string,
          ),
        ),
      ])
  }
}
