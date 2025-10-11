import gleam/string
import gleam/list
import gleam/option.{type Option}
import formal/form

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
