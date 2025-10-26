import gleam/pair
import gleam/list
import formal/form
import gleam/result
import gleam/int
import gleam/float
import gleam/string
import gleam/json.{type Json}
import gleam/dynamic/decode.{type Decoder}
import youid/uuid.{type Uuid}
import birl
import deriv/internal/common
import gleam/option

// stdlib re-exports

pub const none = option.None
pub const list_map = list.map

// dep re-exports

pub type Time = birl.Time

//

pub type DerivedFormLookups(field, form) {
  DerivedFormLookups(
    name_to_field: fn(String) -> Result(field, Nil),
    field_to_name: fn(field) -> String,
    field_to_type: fn(field) -> GleamType,
    field_to_dom_id: fn(field) -> String,
    field_to_default_label: fn(field) -> String,
  )
}

pub type GleamType {
  String
  Int
  Float
  Bool
  Option(GleamType)
  List(GleamType)
  //
  Uri
  Date
  TimeOfDay
}

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

pub fn snake_case(str: String) -> String {
  common.snake_case(str)
}

pub fn formal_scalar_parser(
  parse parse: fn(String) -> Result(t, Nil),
  type_display type_display: String,
  zero zero: t,
) -> form.Parser(t) {
  fn(strs) {
    case strs {
      [] -> {
        Error(#(zero, "Missing " <> type_display))
      }

      [str] -> {
        parse(str)
        |> result.replace_error(#(zero, "Invalid " <> type_display))
      }

      _multiple -> {
        Error(#(zero, "Invalid " <> type_display <> " (multiple)"))
      }
    }
  }
  |> form.parse
}

// re-exports

pub fn inspect(x: a) -> String {
  string.inspect(x)
}

// form birl

pub fn birl_time_iso8601_parser() -> form.Parser(birl.Time) {
  birl_time_parser(parse: fn(str) {
    str
    |> birl.parse
    |> result.replace_error("Invalid ISO8601 date/time")
  })
}

fn birl_time_parser(
  parse parse: fn(String) -> Result(birl.Time, String),
) -> form.Parser(birl.Time) {
  form.parse(fn(strs) {
    strs
    |> fn(strs) {
      case strs {
        [str] -> {
          Ok(str)
        }

        [_, ..] -> {
          Error(#(zero_birl_time(), "Expected a single ISO8601 but got multiple"))
        }

        [] -> {
          Error(#(zero_birl_time(), "Didn't find an ISO8601 date/time"))
        }
      }
    }
    |> result.try(fn(str) {
      str
      |> parse
      |> result.map_error(pair.new(zero_birl_time(), _))
      // |> birl.parse
      // |> result.replace_error(#(birl.from_unix(0), "Invalid ISO8601 date/time"))
    })
  })
}

fn zero_birl_time() -> birl.Time {
  birl.from_unix(0)
}

// `derive form` static lookup funcs

pub fn field_to_default_label(
  field field: field,
  field_to_name field_to_name: fn(field) -> String,
) -> String {
  field
  |> field_to_name
  |> common.snake_case_to_label
}

pub fn field_to_dom_id(
  field field: field,
) -> String {
  string.inspect(field)
}
