import decode.{type Decoder}
import deriv/util
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type Foo {
  Foo(
    uuid: Uuid,
    id: Int, //$ json(decode(decoder(foo/bar.baz)))
    name: String, //$ json(decode(decoder(asdf)))
    active: Bool,
    ratio: Float,
    maybe: Option(String),
    words: List(String),
  )
} //$ derive json(decode,encode)

pub type Bar {
  Bar(baz: Bool)
} //$ derive json(decode)


pub fn decoder_foo() -> Decoder(Foo) {
  decode.into({
    use uuid <- decode.parameter
    use id <- decode.parameter
    use name <- decode.parameter
    use active <- decode.parameter
    use ratio <- decode.parameter
    use maybe <- decode.parameter
    use words <- decode.parameter

    Foo(uuid:, id:, name:, active:, ratio:, maybe:, words:)
  })
  |> decode.field("uuid", util.decoder_uuid())
  |> decode.field("id", decode.int)
  |> decode.field("name", decode.string)
  |> decode.field("active", decode.bool)
  |> decode.field("ratio", decode.float)
  |> decode.field("maybe", decode.optional(decode.string))
  |> decode.field("words", decode.list(decode.string))
}

pub fn encode_foo(value: Foo) -> Json {
  json.object([
    #("uuid", util.encode_uuid(value.uuid)),
    #("id", json.int(value.id)),
    #("name", json.string(value.name)),
    #("active", json.bool(value.active)),
    #("ratio", json.float(value.ratio)),
    #("maybe", json.nullable(value.maybe, json.string)),
    #("words", json.preprocessed_array(list.map(value.words, json.string))),
  ])
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.into({
    use baz <- decode.parameter

    Bar(baz:)
  })
  |> decode.field("baz", decode.bool)
}