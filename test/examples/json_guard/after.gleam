import deriv/util as deriv
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Field {
  //$ derive json decode encode
  //$ json variant key None
  Text(
    //$ json guard type "text"
    //$ json encode static type "text"
    text: String,
  )

  Number(
    //$ json guard type "number"
    //$ json encode static type "number"
    number: Float,
  )
}

pub fn decoder_field() -> Decoder(Field) {
  decode.one_of(decoder_field_text(), [decoder_field_number()])
}

pub fn decoder_field_text() -> Decoder(Field) {
  use _ <- decode.subfield(["type"], deriv.is("text"))
  use text <- decode.field("text", decode.string)
  decode.success(Text(text:))
}

pub fn decoder_field_number() -> Decoder(Field) {
  use _ <- decode.subfield(["type"], deriv.is("number"))
  use number <- decode.field("number", decode.float)
  decode.success(Number(number:))
}

pub fn encode_field(value: Field) -> Json {
  case value {
    Text(..) as value ->
      json.object([
        #("type", json.string("text")),
        #("text", json.string(value.text)),
      ])
    Number(..) as value ->
      json.object([
        #("type", json.string("number")),
        #("number", json.float(value.number)),
      ])
  }
}
