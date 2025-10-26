import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
    //$ json decoder some_decoder_name
    //$ json encode some_encode_func_name
  )
}

fn some_decoder_name() -> Decoder(Option(String)) {
  todo
}

fn some_encode_func_name(x: String) -> Json {
  todo
}
