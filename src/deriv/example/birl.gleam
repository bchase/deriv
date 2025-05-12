import gleam/dict.{type Dict}
import birl.{type Time}
import decode.{type Decoder}
import deriv/util
import gleam/json.{type Json}

pub type DateTimeExamples {
  DateTimeExamples(
    t0: Time,
    t1: Time, //$ json(birl(parse))
    t2: Time, //$ json(birl(naive))
    t3: Time, //$ json(birl(http))
    t4: Time, //$ json(birl(unix))
    t5: Time, //$ json(birl(unix_milli))
    t6: Time, //$ json(birl(unix_micro))
  )
} //$ derive json(decode,encode)

pub fn decoder_date_time_examples() -> Decoder(DateTimeExamples) {
  decode.into({
    use t0 <- decode.parameter
    use t1 <- decode.parameter
    use t2 <- decode.parameter
    use t3 <- decode.parameter
    use t4 <- decode.parameter
    use t5 <- decode.parameter
    use t6 <- decode.parameter
    DateTimeExamples(t0:, t1:, t2:, t3:, t4:, t5:, t6:)
  })
  |> decode.field("t0", util.decoder_birl_parse())
  |> decode.field("t1", util.decoder_birl_parse())
  |> decode.field("t2", util.decoder_birl_from_naive())
  |> decode.field("t3", util.decoder_birl_from_http())
  |> decode.field("t4", util.decoder_birl_from_unix())
  |> decode.field("t5", util.decoder_birl_from_unix_milli())
  |> decode.field("t6", util.decoder_birl_from_unix_micro())
}

pub fn encode_date_time_examples(value: DateTimeExamples) -> Json {
  json.object([
      #("t0", util.encode_birl_to_iso8601(value.t0)),
      #("t1", util.encode_birl_to_iso8601(value.t1)),
      #("t2", util.encode_birl_to_naive(value.t2)),
      #("t3", util.encode_birl_to_http(value.t3)),
      #("t4", util.encode_birl_to_unix(value.t4)),
      #("t5", util.encode_birl_to_unix_milli(value.t5)),
      #("t6", util.encode_birl_to_unix_micro(value.t6)),
  ])
}

pub type DictFieldType {
  DictFieldType(
    dict: Dict(String, Int),
  )
} //$ derive json(decode,encode)

pub fn decoder_dict_field_type() -> Decoder(DictFieldType) {
  decode.one_of([decoder_dict_field_type_dict_field_type()])
}

pub fn decoder_dict_field_type_dict_field_type() -> Decoder(
  DictFieldType,
) {
  decode.into({
    use dict <- decode.parameter
    DictFieldType(dict:)
  })
  |> decode.field("dict", decode.dict(decode.string, decode.int))
}

pub fn encode_dict_field_type(value: DictFieldType) -> Json {
  case value {
    DictFieldType(..) as value ->
      json.object([
        #("dict", json.dict(value.dict, fn(str) { str }, json.int)),
      ])
  }
}
