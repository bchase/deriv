import birl
import deriv/util
import formal/form
import gleam/int
import gleam/option.{type Option}
import gleam/result
import gleam/time/calendar
import gleam/uri

pub type Form {
  //$ derive form
  Form(
    str: String,
    str_option: Option(String),
    str_list: List(String),
    //
    int: Int,
    int_option: Option(Int),
    int_list: List(Int),
    //
    float: Float,
    float_option: Option(Float),
    float_list: List(Float),
    //
    bool: Bool,
    bool_option: Option(Bool),
    bool_list: List(Bool),
    //
    parse_int: Int,
    //$ form parser custom_parse_int
    //$ form check divisible_by_two
    parse_int_option: Option(Int),
    //$ form parser custom_parse_int_option
    parse_int_option_inner: Option(Int),
    //$ form parser inner custom_parse_int
    parse_int_list: List(Int),
    //$ form parser custom_parse_int_list
    parse_int_list_inner: List(Int),
    //$ form parser inner custom_parse_int
    //
    email: String,
    //$ form parse_email
    phone_number: String,
    //$ form parse_phone_number
    colour: String,
    //$ form parse_colour
    //
    uri: uri.Uri,
    //$ form parse_url
    //
    date: calendar.Date,
    //$ form parse_date
    time: calendar.TimeOfDay,
    //$ form parse_time
    date_time: #(calendar.Date, calendar.TimeOfDay),
    //$ form parse_date_time
    birl_time: birl.Time,
    //
    email_confirm: String,
    //$ form parse_email
    //$ form check_confirms email
    bool_accepted: Bool,
    //$ form check_accepted
    float_check_less: Float,
    //$ form check_float_less_than 1.5
    float_check_more: Float,
    //$ form check_float_more_than 3.0
    int_check_less: Int,
    //$ form check_int_less_than 1
    int_check_more: Int,
    //$ form check_int_more_than 3
    str_check_not_empty: String,
    //$ form check_not_empty
    str_check_length_less: String,
    //$ form check_string_length_less_than 1
    str_check_length_more: String,
    //$ form check_string_length_more_than 3
    dir: Dir,
    //$ form parser parser_dir
  )
}

pub type Dir {
  //$ derive zero
  //$ derive enum
  Up
  //$ enum display up
  Down
  //$ enum display down
}

fn custom_parse_int() -> form.Parser(Int) {
  todo
}
fn custom_parse_int_option() -> form.Parser(Option(Int)) {
  todo
}
fn custom_parse_int_list() -> form.Parser(List(Int)) {
  todo
}

fn divisible_by_two(
  num num: Int,
) -> Result(Int, String) {
  let den = 2

  num
  |> int.remainder(den)
  |> result.replace_error("Divided by 0")
  |> result.try(fn(rem) {
    case rem == 0 {
      True -> Ok(num)
      False -> Error("Must be divisible by " <> int.to_string(den))
    }
  })
}

fn parser_dir() -> form.Parser(Dir) {
  util.formal_scalar_parser(
    parse: parse_enum_dir,
    type_display: "direction",
    zero: Up,
  )
}

pub type NestedForm {
  NestedForm(
    str: String,
  )
}

pub fn parse_enum_dir(str: String) -> Result(Dir, Nil) {
  todo as "here for compiler; rewritten by `derive enum`"
}
