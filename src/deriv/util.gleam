import gleam/int
import gleam/float
import gleam/string
import gleam/json.{type Json}
import gleam/dynamic/decode.{type Decoder}
import youid/uuid.{type Uuid}
import birl.{type Time}

fn decoder_from_string(
  parse: fn(String) -> Result(t, err),
  zero: t,
  err_msg: fn(String) -> String,
) -> Decoder(t) {
  use str <- decode.subfield([], decode.string)
  case parse(str) {
    Ok(x) -> decode.success(x)
    Error(_) -> decode.failure(zero, err_msg(str))
  }
}

pub fn decoder_int_string() -> Decoder(Int) {
  decoder_from_string(int.parse, 0, fn(str) {
    "`decoder_int_string` failed to parse `Int` from: " <> str
  })
}

pub fn decoder_float_string() -> Decoder(Float) {
  decoder_from_string(float.parse, 0.0, fn(str) {
    "`decoder_float_string` failed to parse `Float` from: " <> str
  })
}

pub fn decoder_bool_string() -> Decoder(Bool) {
  decoder_from_string(parse_bool, False, fn(str) {
    "`decoder_bool_string` failed to parse `Bool` from: " <> str
  })
}

pub fn decoder_uuid_string() -> Decoder(Uuid) {
  decoder_from_string(uuid.from_string, uuid.v7_from_millisec(0), fn(str) {
    "`decoder_uuid_string` failed to parse `Uuid` from: " <> str
  })
}

fn parse_bool(
  str: String,
) -> Result(Bool, Nil) {
  case str {
    "True" -> Ok(True)
    "False" -> Ok(False)
    _ -> Error(Nil)
  }
}

pub fn decoder_uuid() -> Decoder(Uuid) {
  use str <- decode.then(decode.string)
  case uuid.from_string(str) {
    Ok(uuid) -> decode.success(uuid)
    Error(Nil) -> {
      decode.failure(zero_uuid(), "Failed to parse UUID")
    }
  }
}

pub fn encode_uuid(uuid: Uuid) -> Json {
  uuid
  |> uuid.to_string
  |> string.lowercase
  |> json.string
}

pub fn zero_uuid() -> Uuid {
  uuid.v7_from_millisec(0)
}

pub fn zero_time() -> Time {
  birl.from_unix(0)
}

pub fn is(
  value: String,
) -> Decoder(Nil) {
  decode.string
  |> decode.then(fn(str) {
    case str == value {
      True -> decode.success(Nil)
      False -> decode.failure(Nil, "failed to match for value: " <> value)
    }
  })
}

pub fn decoder_birl_parse() -> Decoder(Time) {
  decoder_birl_string_to_result(
    func_name: "parse",
    func: birl.parse,
  )
}

pub fn decoder_birl_from_naive() -> Decoder(Time) {
  decoder_birl_string_to_result(
    func_name: "from_naive",
    func: birl.from_naive,
  )
}

pub fn decoder_birl_from_http() -> Decoder(Time) {
  decoder_birl_string_to_result(
    func_name: "from_http",
    func: birl.from_http,
  )
}

pub fn decoder_birl_from_unix() -> Decoder(Time) {
  decoder_birl_int_to_time(birl.from_unix)
}

pub fn decoder_birl_from_unix_milli() -> Decoder(Time) {
  decoder_birl_int_to_time(birl.from_unix_milli)
}

pub fn decoder_birl_from_unix_micro() -> Decoder(Time) {
  decoder_birl_int_to_time(birl.from_unix_micro)
}

pub fn encode_birl_to_iso8601(time: Time) -> Json {
  time
  |> birl.to_iso8601
  |> json.string
}

pub fn encode_birl_to_naive(time: Time) -> Json {
  time
  |> birl.to_naive
  |> json.string
}

pub fn encode_birl_to_http(time: Time) -> Json {
  time
  |> birl.to_naive
  |> json.string
}

pub fn encode_birl_to_unix(time: Time) -> Json {
  time
  |> birl.to_unix
  |> json.int
}

pub fn encode_birl_to_unix_milli(time: Time) -> Json {
  time
  |> birl.to_unix_milli
  |> json.int
}

pub fn encode_birl_to_unix_micro(time: Time) -> Json {
  time
  |> birl.to_unix_micro
  |> json.int
}

fn decoder_birl_string_to_result(
  func func: fn(String) -> Result(Time, Nil),
  func_name func_name : String,
) -> Decoder(Time) {
  decode.string
  |> decode.then(fn(str) {
    case func(str) {
      Ok(time) -> decode.success(time)
      Error(_) -> decode.failure(birl.from_unix(0), "Failed to `" <> func_name <> "`: " <> str)
    }
  })
}

fn decoder_birl_int_to_time(
  func: fn(Int) -> Time,
) -> Decoder(Time) {
  decode.int
  |> decode.then(fn(int) {
    int
    |> func
    |> decode.success
  })
}
