import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Unnested {
  //$ derive json decode encode
  Unnested(
    unnested: Int,
    //$ json named foo.bar.baz
  )
}

pub fn decoder_unnested() -> Decoder(Unnested) {
  decode.one_of(decoder_unnested_unnested(), [])
}

pub fn decoder_unnested_unnested() -> Decoder(Unnested) {
  use unnested <- decode.subfield(["foo", "bar", "baz"], decode.int)
  decode.success(Unnested(unnested:))
}

pub fn encode_unnested(value: Unnested) -> Json {
  case value {
    Unnested(..) as value ->
      json.object([
        #(
          "foo",
          json.object([
            #("bar", json.object([#("baz", json.int(value.unnested))])),
          ]),
        ),
      ])
  }
}
