import deriv/util as deriv
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type StrKeyDict =
  //$ derive json decode encode
  Dict(String, String)

pub type IntKeyDict =
  //$ derive json decode encode
  Dict(Int, String)

pub type FloatKeyDict =
  //$ derive json decode encode
  Dict(Float, String)

pub type BoolKeyDict =
  //$ derive json decode encode
  Dict(Bool, String)

pub fn decoder_str_key_dict() -> Decoder(StrKeyDict) {
  decode.dict(decode.string, decode.string)
}

pub fn encode_str_key_dict(value: Dict(String, String)) -> Json {
  json.dict(value, fn(str) { str }, json.string)
}

pub fn decoder_int_key_dict() -> Decoder(IntKeyDict) {
  decode.dict(deriv.decoder_int_string(), decode.string)
}

pub fn encode_int_key_dict(value: Dict(Int, String)) -> Json {
  json.dict(value, deriv.int_to_string, json.string)
}

pub fn decoder_float_key_dict() -> Decoder(FloatKeyDict) {
  decode.dict(deriv.decoder_float_string(), decode.string)
}

pub fn encode_float_key_dict(value: Dict(Float, String)) -> Json {
  json.dict(value, deriv.float_to_string, json.string)
}

pub fn decoder_bool_key_dict() -> Decoder(BoolKeyDict) {
  decode.dict(deriv.decoder_bool_string(), decode.string)
}

pub fn encode_bool_key_dict(value: Dict(Bool, String)) -> Json {
  json.dict(value, deriv.bool_to_string, json.string)
}
