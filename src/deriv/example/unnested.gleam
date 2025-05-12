import decode.{type Decoder}
import gleam/json.{type Json}

pub type Unnested {
  Unnested(
    unnested: Int, //$ json(named(foo.bar.baz))
  )
} //$ derive json(decode,encode)

pub fn decoder_unnested() -> Decoder(Unnested) {
  decode.one_of([decoder_unnested_unnested()])
}

pub fn decoder_unnested_unnested() -> Decoder(Unnested) {
  decode.into({
    use unnested <- decode.parameter
    Unnested(unnested:)
  })
  |> decode.subfield(["foo", "bar", "baz"], decode.int)
}

pub fn encode_unnested(value: Unnested) -> Json {
  case value {
    Unnested(..) as value ->
      json.object([
        #("foo", json.object([
          #("bar", json.object([
            #("baz", json.int(value.unnested))
          ])),
        ]))
      ])
  }
}
