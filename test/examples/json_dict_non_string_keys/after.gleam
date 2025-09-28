import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import deriv/util

pub type IntKeyDict =
  //$ derive json decode
  Dict(Int, String)

pub type FloatKeyDict =
  //$ derive json decode
  Dict(Float, String)

pub type BoolKeyDict =
  //$ derive json decode
  Dict(Bool, String)

pub fn decoder_int_key_dict() -> Decoder(IntKeyDict) {
  decode.dict(util.decoder_int_string(), decode.string)
}

pub fn decoder_float_key_dict() -> Decoder(FloatKeyDict) {
  decode.dict(util.decoder_float_string(), decode.string)
}

pub fn decoder_bool_key_dict() -> Decoder(BoolKeyDict) {
  decode.dict(util.decoder_bool_string(), decode.string)
}
