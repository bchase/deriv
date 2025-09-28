import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{type Option, None}

pub type Top {
  //$ derive json decode
  Top(
    list: List(String),
    //$ json decoder top list_override
    option: Option(Int),
    //$ json decoder top option_override
  )
}

fn list_override() -> Decoder(List(String)) {
  todo
}

fn option_override() -> Decoder(Option(Int)) {
  todo
}

pub fn decoder_top() -> Decoder(Top) {
  decode.one_of(decoder_top_top(), [])
}

pub fn decoder_top_top() -> Decoder(Top) {
  use list <- decode.field("list", list_override())
  use option <- decode.optional_field("option", None, option_override())
  decode.success(Top(list:, option:))
}
