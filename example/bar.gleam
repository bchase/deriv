import decode.{type Decoder}
import gleam/json.{type Json}

pub type Baz {
  Baz(
    id: Int,
    name: String,
    active: Bool,
  )
} //$ derive json(decode,encode)


pub fn decoder_baz() -> Decoder(Baz) {
  decode.into({
    use id <- decode.parameter
    use name <- decode.parameter
    use active <- decode.parameter

    Baz(id:, name:, active:)
  })
  |> decode.field("id", decode.int)
  |> decode.field("name", decode.string)
  |> decode.field("active", decode.bool)
}

pub fn encode_baz(value: Baz) -> Json {
  json.object([
    #("id", json.int(value.id)),
    #("name", json.string(value.name)),
    #("active", json.bool(value.active)),
  ])
}