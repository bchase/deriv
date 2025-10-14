import birl
import deriv/util
import formal/form
import gleam/option.{type Option}
import gleam/string
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
  case str {
    "Up" -> Ok(Up)
    "Down" -> Ok(Down)
    _ -> Error(Nil)
  }
}


pub fn form_form() -> form.Form(Form) {
  form.new({
    use str <- form.field("str", { form.parse_string })
    use str_option <- form.field("str_option", {
      form.parse_string |> form.parse_optional
    })
    use str_list <- form.field("str_list", {
      form.parse_string |> form.parse_list
    })
    use int <- form.field("int", { form.parse_int })
    use int_option <- form.field("int_option", {
      form.parse_int |> form.parse_optional
    })
    use int_list <- form.field("int_list", { form.parse_int |> form.parse_list })
    use float <- form.field("float", { form.parse_float })
    use float_option <- form.field("float_option", {
      form.parse_float |> form.parse_optional
    })
    use float_list <- form.field("float_list", {
      form.parse_float |> form.parse_list
    })
    use bool <- form.field("bool", { form.parse_checkbox })
    use bool_option <- form.field("bool_option", {
      form.parse_checkbox |> form.parse_optional
    })
    use bool_list <- form.field("bool_list", {
      form.parse_checkbox |> form.parse_list
    })
    use parse_int <- form.field("parse_int", { custom_parse_int() })
    use parse_int_option <- form.field("parse_int_option", {
      custom_parse_int_option()
    })
    use parse_int_option_inner <- form.field("parse_int_option_inner", {
      custom_parse_int() |> form.parse_optional
    })
    use parse_int_list <- form.field("parse_int_list", {
      custom_parse_int_list()
    })
    use parse_int_list_inner <- form.field("parse_int_list_inner", {
      custom_parse_int() |> form.parse_list
    })
    use email <- form.field("email", { form.parse_email })
    use phone_number <- form.field("phone_number", { form.parse_phone_number })
    use colour <- form.field("colour", { form.parse_colour })
    use uri <- form.field("uri", { form.parse_url })
    use date <- form.field("date", { form.parse_date })
    use time <- form.field("time", { form.parse_time })
    use date_time <- form.field("date_time", { form.parse_date_time })
    use birl_time <- form.field("birl_time", { util.birl_time_iso8601_parser() })
    use email_confirm <- form.field("email_confirm", {
      form.parse_email |> form.check_confirms(email)
    })
    use bool_accepted <- form.field("bool_accepted", {
      form.parse_checkbox |> form.check_accepted
    })
    use float_check_less <- form.field("float_check_less", {
      form.parse_float |> form.check_float_less_than(1.5)
    })
    use float_check_more <- form.field("float_check_more", {
      form.parse_float |> form.check_float_more_than(3.0)
    })
    use int_check_less <- form.field("int_check_less", {
      form.parse_int |> form.check_int_less_than(1)
    })
    use int_check_more <- form.field("int_check_more", {
      form.parse_int |> form.check_int_more_than(3)
    })
    use str_check_not_empty <- form.field("str_check_not_empty", {
      form.parse_string |> form.check_not_empty
    })
    use str_check_length_less <- form.field("str_check_length_less", {
      form.parse_string |> form.check_string_length_less_than(1)
    })
    use str_check_length_more <- form.field("str_check_length_more", {
      form.parse_string |> form.check_string_length_more_than(3)
    })
    use dir <- form.field("dir", { parser_dir() })
    form.success(Form(
      str:,
      str_option:,
      str_list:,
      int:,
      int_option:,
      int_list:,
      float:,
      float_option:,
      float_list:,
      bool:,
      bool_option:,
      bool_list:,
      parse_int:,
      parse_int_option:,
      parse_int_option_inner:,
      parse_int_list:,
      parse_int_list_inner:,
      email:,
      phone_number:,
      colour:,
      uri:,
      date:,
      time:,
      date_time:,
      birl_time:,
      email_confirm:,
      bool_accepted:,
      float_check_less:,
      float_check_more:,
      int_check_less:,
      int_check_more:,
      str_check_not_empty:,
      str_check_length_less:,
      str_check_length_more:,
      dir:,
    ))
  })
}

pub fn zero_dir() -> Dir {
  Up
}

pub fn enum_dir_str(x: Dir) -> String {
  string.inspect(x)
}

pub fn display_enum_dir(x: Dir) -> String {
  case x {
    Up -> "up"
    Down -> "down"
  }
}
