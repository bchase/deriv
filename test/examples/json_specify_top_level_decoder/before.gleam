import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option}

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


