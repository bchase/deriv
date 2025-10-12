import deriv/util
import formal/form
import gleam/string
import gleam/list
import gleam/option.{type Option}
import gleam/time/calendar
import gleam/uri

// BASIC
//   - override parse (inner/outer)
//     * //$ form parser func_name
//     * //$ form parser inner func_name
//     * //$ form parser outer func_name
//   - use `parse_email` for emails
//     * //$ form parse as email
//   - specify validations
//     * //$ form validate
//   - use user-defined func with `formal/form.check` -- `fn(b) -> Result(b, String)`
//     * //$ form check with func_name
// TYPES
//   - derive `Option(List(t))`
//   - derive `Time` w/ opts (like `json`)
//   - derive `Uuid`
// ? - derive `Dict` (not sure how to parse keys...)
// PUNT
//   - specify parse/check on nested fields

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
    nested: NestedForm,
    //

    parse_int: Int,
    //$ form parser custom_parse_int

    parse_int_option: Option(Int),
    //$ form parser custom_parse_int_option
    parse_int_option_inner: Option(Int),
    //$ form parser custom_parse_int

    parse_int_list: List(Int),
    //$ form parser custom_parse_int_list
    parse_int_list_inner: List(Int),
    //$ form parser custom_parse_int

    //parse_int: Int,
    ////$ form parser custom_parse_int
    //parse_int_outer_explicit: Int,
    ////$ form parser outer custom_parse_int

    //parse_int_option_inner_implicit: Option(Int),
    ////$ form parser custom_parse_int
    //parse_int_option_inner_explicit: Option(Int),
    ////$ form parser inner custom_parse_int
    //parse_int_option_outer: Option(Int),
    ////$ form parser outer custom_parse_int_option

    //parse_int_list_inner_implicit: List(Int),
    ////$ form parser custom_parse_int
    //parse_int_list_inner_explicit: List(Int),
    ////$ form parser inner custom_parse_int
    //parse_int_list_outer: List(Int),
    ////$ form parser outer custom_parse_int_list

    email: String,
    //$ form parse_email
    phone_number: String,
    //$ form parse_phone_number
    colour: String,
    //$ form parse_colour

    uri: uri.Uri,
    //$ form parse_uri

    date: calendar.Date,
    //$ form parse_date
    date_time: #(calendar.Date, calendar.TimeOfDay),
    //$ form parse_date_time
    time: calendar.TimeOfDay,
    //$ form parse_time

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
  )
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

pub type NestedForm {
  NestedForm(
    str: String,
  )
}

pub fn form_form() -> form.Form(Form) {
  form.new({
    use str <- form.field("str", {
      form.parse_string
    })
    use str_option <- form.field("str_option", {
      form.parse_string
      |> form.parse_optional
    })
    use str_list <- form.field("str_list", {
      form.parse_string
      |> form.parse_list
    })
    use int <- form.field("int", {
      form.parse_int
    })
    use int_option <- form.field("int_option", {
      form.parse_int
      |> form.parse_optional
    })
    use int_list <- form.field("int_list", {
      form.parse_int
      |> form.parse_list
    })
    use float <- form.field("float", {
      form.parse_float
    })
    use float_option <- form.field("float_option", {
      form.parse_float
      |> form.parse_optional
    })
    use float_list <- form.field("float_list", {
      form.parse_float
      |> form.parse_list
    })
    use bool <- form.field("bool", {
      form.parse_checkbox
    })
    use bool_option <- form.field("bool_option", {
      form.parse_checkbox
      |> form.parse_optional
    })
    use bool_list <- form.field("bool_list", {
      form.parse_checkbox
      |> form.parse_list
    })
    use parse_int <- form.field("parse_int", {
      custom_parse_int()
    })
    use parse_int_option <- form.field("parse_int_option_", {
      custom_parse_int_option()
    })
    use parse_int_option_inner <- form.field("parse_int_option_inner", {
      custom_parse_int()
      |> form.parse_optional
    })
    use parse_int_list <- form.field("parse_int_list", {
      custom_parse_int_list()
    })
    use parse_int_list_inner <- form.field("parse_int_list_inner", {
      custom_parse_int()
      |> form.parse_list
    })
    // use parse_int <- form.field("parse_int", {
    //   custom_parse_int()
    // })
    // use parse_int_outer_explicit <- form.field("parse_int_outer_explicit", {
    //   custom_parse_int()
    // })
    // use parse_int_option_inner_implicit <- form.field("parse_int_option_inner_implicit", {
    //   custom_parse_int()
    //   |> form.parse_optional
    // })
    // use parse_int_option_inner_explicit <- form.field("parse_int_option_inner_explicit", {
    //   custom_parse_int()
    //   |> form.parse_optional
    // })
    // use parse_int_option_outer <- form.field("parse_int_option_outer", {
    //   custom_parse_int_option()
    // })
    // use parse_int_list_inner_implicit <- form.field("parse_int_list_inner_implicit", {
    //   custom_parse_int()
    //   |> form.parse_list
    // })
    // use parse_int_list_inner_explicit <- form.field("parse_int_list_inner_explicit", {
    //   custom_parse_int_list()
    // })
    // use parse_int_list_outer <- form.field("parse_int_list_outer", {
    //   custom_parse_int_list()
    // })
    // NESTED
    use nested_str <- form.field("nested[str]", {
      form.parse_string
    })
    // use _ <- nested_form_form(prefix: ["nested"])

    use email <- form.field("email", {
      form.parse_email
    })
    use phone_number <- form.field("phone_number", {
      form.parse_phone_number
    })
    use colour <- form.field("colour", {
      form.parse_colour
    })

    use uri <- form.field("uri", {
      form.parse_url
    })

    use date <- form.field("date", {
      form.parse_date
    })
    use date_time <- form.field("date_time", {
      form.parse_date_time
    })
    use time <- form.field("time", {
      form.parse_time
    })

    use email_confirm <- form.field("email_confirm", {
      form.parse_email
      |> form.check_confirms(email)
    })
    use bool_accepted <- form.field("bool_accepted", {
      form.parse_checkbox
      |> form.check_accepted
    })
    use float_check_less <- form.field("float_check_less", {
      form.parse_float
      |> form.check_float_less_than(1.5)
    })
    use float_check_more <- form.field("float_check_more", {
      form.parse_float
      |> form.check_float_more_than(3.0)
    })
    use int_check_less <- form.field("int_check_less", {
      form.parse_int
      |> form.check_int_less_than(1)
    })
    use int_check_more <- form.field("int_check_more", {
      form.parse_int
      |> form.check_int_more_than(3)
    })
    use str_check_not_empty <- form.field("str_check_not_empty", {
      form.parse_string
      |> form.check_not_empty
    })
    use str_check_length_less <- form.field("str_check_length_less", {
      form.parse_string
      |> form.check_string_length_less_than(1)
    })
    use str_check_length_more <- form.field("str_check_length_more", {
      form.parse_string
      |> form.check_string_length_more_than(3)
    })

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
     // parse_int:,
     // parse_int_outer_explicit:,
     // parse_int_option_inner_implicit:,
     // parse_int_option_inner_explicit:,
     // parse_int_option_outer:,
     // parse_int_list_inner_implicit:,
     // parse_int_list_inner_explicit:,
     // parse_int_list_outer:,
     nested: NestedForm(
      str: nested_str,
     ),
     email:,
     phone_number:,
     colour:,
     uri:,
     date:,
     date_time:,
     time:,
     email_confirm:,
     bool_accepted:,
     float_check_less:,
     float_check_more:,
     int_check_less:,
     int_check_more:,
     str_check_not_empty:,
     str_check_length_less:,
     str_check_length_more:,
    ))
  })
}

// pub fn nested_form_form(
//   prefix prefix: List(String),
// ) -> form.Form(NestedForm) {
//   let name__ = field_name(_, prefix:)

//   form.new({
//     use str <- form.field(name__("str"), {
//       form.parse_string
//     })

//     form.success(NestedForm(
//       str:,
//     ))
//   })
// }

// fn field_name(
//   name name: String,
//   prefix prefix: List(String),
// ) -> String {
//   case prefix {
//     [] -> {
//       name
//     }

//     [first, ..rest] -> {
//       let rest =
//         rest
//         |> list.append([name])
//         |> list.map(fn(str) { "[" <> str <> "]" })
//         |> string.join("")

//       first <> rest
//     }
//   }
// }

    // str
    // str_option
    // str_list
    // int
    // int_option
    // int_list
    // float
    // float_option
    // float_list
    // bool
    // bool_option
    // bool_list
    // nested
    // parse_int
    // parse_int_option
    // parse_int_option_inner
    // parse_int_list
    // parse_int_list_inner
    // email
    // phone_number
    // colour
    // uri
    // date
    // time
    // email_confirm
    // bool_accepted
    // float_check_less
    // float_check_more
    // int_check_less
    // int_check_more
    // str_check_not_empty
    // str_check_length_less
    // str_check_length_more

pub type FormField {
  FormStr
  FormStrOption
  FormStrList
  FormInt
  FormIntOption
  FormIntList
  FormFloat
  FormFloatOption
  FormFloatList
  FormBool
  FormBoolOption
  FormBoolList
  FormParseInt
  FormParseIntOption
  FormParseIntOptionInner
  FormParseIntList
  FormParseIntListInner
  FormEmail
  FormPhoneNumber
  FormColour
  FormUri
  FormDate
  FormTime
  FormEmailConfirm
  FormBoolAccepted
  FormFloatCheckLess
  FormFloatCheckMore
  FormIntCheckLess
  FormIntCheckMore
  FormStrCheckNotEmpty
  FormStrCheckLengthLess
  FormStrCheckLengthMore
}

type FormFieldLookups = util.DerivedFormLookups(FormField)

pub fn form_field_lookups() -> FormFieldLookups {
  let field_to_name = fn(field) {
    case field {
      FormStr -> "str"
      FormStrOption -> "str_option"
      FormStrList -> "str_list"
      FormInt -> "int"
      FormIntOption -> "int_option"
      FormIntList -> "int_list"
      FormFloat -> "float"
      FormFloatOption -> "float_option"
      FormFloatList -> "float_list"
      FormBool -> "bool"
      FormBoolOption -> "bool_option"
      FormBoolList -> "bool_list"
      FormParseInt -> "parse_int"
      FormParseIntOption -> "parse_int_option"
      FormParseIntOptionInner -> "parse_int_option_inner"
      FormParseIntList -> "parse_int_list"
      FormParseIntListInner -> "parse_int_list_inner"
      FormEmail -> "email"
      FormPhoneNumber -> "phone_number"
      FormColour -> "colour"
      FormUri -> "uri"
      FormDate -> "date"
      FormTime -> "time"
      FormEmailConfirm -> "email_confirm"
      FormBoolAccepted -> "bool_accepted"
      FormFloatCheckLess -> "float_check_less"
      FormFloatCheckMore -> "float_check_more"
      FormIntCheckLess -> "int_check_less"
      FormIntCheckMore -> "int_check_lore"
      FormStrCheckNotEmpty -> "str_check_lot_empty"
      FormStrCheckLengthLess -> "str_check_length_less"
      FormStrCheckLengthMore -> "str_check_length_more"
    }
  }

  let name_to_field = fn(name) {
    case name {
      "str" -> Ok(FormStr)
      "str_option" -> Ok(FormStrOption)
      "str_list" -> Ok(FormStrList)
      "int" -> Ok(FormInt)
      "int_option" -> Ok(FormIntOption)
      "int_list" -> Ok(FormIntList)
      "float" -> Ok(FormFloat)
      "float_option" -> Ok(FormFloatOption)
      "float_list" -> Ok(FormFloatList)
      "bool" -> Ok(FormBool)
      "bool_option" -> Ok(FormBoolOption)
      "bool_list" -> Ok(FormBoolList)
      "parse_int" -> Ok(FormParseInt)
      "parse_int_option" -> Ok(FormParseIntOption)
      "parse_int_option_inner" -> Ok(FormParseIntOptionInner)
      "parse_int_list" -> Ok(FormParseIntList)
      "parse_int_list_inner" -> Ok(FormParseIntListInner)
      "email" -> Ok(FormEmail)
      "phone_number" -> Ok(FormPhoneNumber)
      "colour" -> Ok(FormColour)
      "uri" -> Ok(FormUri)
      "date" -> Ok(FormDate)
      "time" -> Ok(FormTime)
      "email_confirm" -> Ok(FormEmailConfirm)
      "bool_accepted" -> Ok(FormBoolAccepted)
      "float_check_less" -> Ok(FormFloatCheckLess)
      "float_check_more" -> Ok(FormFloatCheckMore)
      "int_check_less" -> Ok(FormIntCheckLess)
      "int_check_lore" -> Ok(FormIntCheckMore)
      "str_check_lot_empty" -> Ok(FormStrCheckNotEmpty)
      "str_check_length_less" -> Ok(FormStrCheckLengthLess)
      "str_check_length_more" -> Ok(FormStrCheckLengthMore)
      _ -> Error(Nil)
    }
  }

  let field_to_type = fn(field) {
    case field {
      FormStr -> util.String
      FormStrOption -> util.Option(util.String)
      FormStrList -> util.List(util.String)
      FormInt -> util.Int
      FormIntOption -> util.Option(util.Int)
      FormIntList -> util.List(util.Int)
      FormFloat -> util.Float
      FormFloatOption -> util.Option(util.Float)
      FormFloatList -> util.List(util.Float)
      FormBool -> util.Bool
      FormBoolOption -> util.Option(util.Bool)
      FormBoolList -> util.List(util.Bool)
      FormParseInt -> util.Int
      FormParseIntOption -> util.Option(util.Int)
      FormParseIntOptionInner -> util.List(util.Int)
      FormParseIntList -> util.List(util.Int)
      FormParseIntListInner -> util.List(util.Int)
      FormEmail -> util.String
      FormPhoneNumber -> util.String
      FormColour -> util.String
      FormUri -> util.Uri
      FormDate -> util.Date
      FormTime -> util.TimeOfDay
      FormEmailConfirm -> util.String
      FormBoolAccepted -> util.Bool
      FormFloatCheckLess -> util.Float
      FormFloatCheckMore -> util.Float
      FormIntCheckLess -> util.Int
      FormIntCheckMore -> util.Int
      FormStrCheckNotEmpty -> util.String
      FormStrCheckLengthLess -> util.String
      FormStrCheckLengthMore -> util.String
    }
  }

  // let field_to_value = fn(f: form.Form(Form), field: FormField) -> String {
  //   case field {
  //     FormStr -> form.field_value(f, field_to_name(field))
  //     FormStrOption ->
  //     FormStrList -> todo
  //     FormInt -> form.field_value(f, field_to_name(field))
  //     FormIntOption -> todo
  //     FormIntList -> todo
  //     FormFloat -> todo
  //     FormFloatOption -> todo
  //     FormFloatList -> todo
  //     FormBool -> todo
  //     FormBoolOption -> todo
  //     FormBoolList -> todo
  //     FormParseInt -> todo
  //     FormParseIntOption -> todo
  //     FormParseIntOptionInner -> todo
  //     FormParseIntList -> todo
  //     FormParseIntListInner -> todo
  //     FormEmail -> todo
  //     FormPhoneNumber -> todo
  //     FormColour -> todo
  //     FormUri -> todo
  //     FormDate -> todo
  //     FormTime -> todo
  //     FormEmailConfirm -> todo
  //     FormBoolAccepted -> todo
  //     FormFloatCheckLess -> todo
  //     FormFloatCheckMore -> todo
  //     FormIntCheckLess -> todo
  //     FormIntCheckMore -> todo
  //     FormStrCheckNotEmpty -> todo
  //     FormStrCheckLengthLess -> todo
  //     FormStrCheckLengthMore -> todo
  //   }
  // }

  let field_to_default_label = fn(field) {
    field
    |> field_to_name
    |> string.split("_")
    |> list.map(string.capitalise)
    |> string.join(" ")
  }

  util.DerivedFormLookups(
    name_to_field:,
    field_to_name:,
    field_to_type:,
    field_to_dom_id: string.inspect,
    field_to_default_label:,
  )
}
