import gleam/dict.{type Dict}
import gleam/string
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type DictFieldType {
  //$ derive json decode encode
  DictFieldType(
    dict: Dict(String, Int),
  )
}

pub fn decoder_dict_field_type() -> Decoder(DictFieldType) {
  decode.one_of(decoder_dict_field_type_dict_field_type(), [])
}

pub fn decoder_dict_field_type_dict_field_type() -> Decoder(DictFieldType) {
  use dict <- decode.field("dict", decode.dict(decode.string, decode.int))
  decode.success(DictFieldType(dict:))
}

pub fn encode_dict_field_type(value: DictFieldType) -> Json {
  case value {
    DictFieldType(..) as value ->
      json.object([#("dict", json.dict(value.dict, string.inspect, json.int))])
  }
}
