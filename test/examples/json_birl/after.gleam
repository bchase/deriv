import birl.{type Time}
import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type DateTimeExamples {
  //$ derive json decode encode
  DateTimeExamples(
    t0: Time,
    t1: Time, //$ json birl iso8601
    t2: Time, //$ json birl naive
    t3: Time, //$ json birl http
    t4: Time, //$ json birl unix
    t5: Time, //$ json birl unix_milli
    t6: Time, //$ json birl unix_micro
  )
}

pub fn decoder_date_time_examples() -> Decoder(DateTimeExamples) {
  decode.one_of(decoder_date_time_examples_date_time_examples(), [])
}

pub fn decoder_date_time_examples_date_time_examples() -> Decoder(
  DateTimeExamples,
) {
  use t0 <- decode.field("t0", util.decoder_birl_parse())
  use t1 <- decode.field("t1", util.decoder_birl_parse())
  use t2 <- decode.field("t2", util.decoder_birl_from_naive())
  use t3 <- decode.field("t3", util.decoder_birl_from_http())
  use t4 <- decode.field("t4", util.decoder_birl_from_unix())
  use t5 <- decode.field("t5", util.decoder_birl_from_unix_milli())
  use t6 <- decode.field("t6", util.decoder_birl_from_unix_micro())
  decode.success(DateTimeExamples(t0:, t1:, t2:, t3:, t4:, t5:, t6:))
}

pub fn encode_date_time_examples(value: DateTimeExamples) -> Json {
  case value {
    DateTimeExamples(..) as value ->
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
}
