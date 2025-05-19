import decode.{type Decoder}
import deriv/example/baz as m7
import gleam/json.{type Json}

pub fn encode_baz(value: m7.Baz) -> Json {
  json.object([
    #("baz", json.string(value.baz)),
  ])
}

pub fn decoder_baz() -> Decoder(m7.Baz) {
  decode.into({
    use baz <- decode.parameter

    m7.Baz(baz:)
  })
  |> decode.field("baz", decode.string)
}

pub fn encode_other(value: m7.Other) -> Json {
  json.object([
    #("other", json.int(value.other)),
  ])
}

pub fn decoder_other() -> Decoder(m7.Other) {
  decode.into({
    use other <- decode.parameter

    m7.Other(other:)
  })
  |> decode.field("other", decode.int)
}