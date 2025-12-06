import deriv/lustre as f
import deriv/util as deriv
import formal/form
import gleam/option.{type Option}

pub type Form {
  //$ derive form lookups lustre
  Form(
    str: String,
    int: Int,
    option_str: Option(String),
    option_int: Option(Int),
    list_str: List(String),
    list_int: List(Int),
  )
}

pub type FormField {
  FormStr
  FormInt
  FormOptionStr
  FormOptionInt
  FormListStr
  FormListInt
}

pub fn form_form() -> form.Form(Form) {
  form.new({
    use str <- form.field("str", { form.parse_string })
    use int <- form.field("int", { form.parse_int })
    use option_str <- form.field("option_str", {
      form.parse_string |> form.parse_optional
    })
    use option_int <- form.field("option_int", {
      form.parse_int |> form.parse_optional
    })
    use list_str <- form.field("list_str", {
      form.parse_string |> form.parse_list
    })
    use list_int <- form.field("list_int", { form.parse_int |> form.parse_list })
    form.success(Form(
      str:,
      int:,
      option_str:,
      option_int:,
      list_str:,
      list_int:,
    ))
  })
}

pub fn form_field_lookups() -> deriv.DerivedFormLookups(FormField, Form) {
  let field_to_name = fn(field) {
    case field {
      FormStr -> "str"
      FormInt -> "int"
      FormOptionStr -> "option_str"
      FormOptionInt -> "option_int"
      FormListStr -> "list_str"
      FormListInt -> "list_int"
    }
  }
  let name_to_field = fn(name) {
    case name {
      "str" -> Ok(FormStr)
      "int" -> Ok(FormInt)
      "option_str" -> Ok(FormOptionStr)
      "option_int" -> Ok(FormOptionInt)
      "list_str" -> Ok(FormListStr)
      "list_int" -> Ok(FormListInt)
      _ -> Error(Nil)
    }
  }
  let field_to_type = fn(field) {
    case field {
      FormStr -> deriv.String
      FormInt -> deriv.Int
      FormOptionStr -> deriv.Option(deriv.String)
      FormOptionInt -> deriv.Option(deriv.Int)
      FormListStr -> deriv.List(deriv.String)
      FormListInt -> deriv.List(deriv.Int)
    }
  }
  let field_values = fn(form: Form, field) {
    case field {
      FormStr -> form.str |> deriv.list_wrap
      FormInt -> form.int |> deriv.int_to_string |> deriv.list_wrap
      FormOptionStr ->
        form.option_str
        |> option.map(deriv.list_wrap)
        |> option.unwrap([])
      FormOptionInt ->
        form.option_int
        |> option.map(deriv.int_to_string)
        |> option.map(deriv.list_wrap)
        |> option.unwrap([])
      FormListStr -> form.list_str
      FormListInt -> form.list_int |> deriv.list_map(deriv.int_to_string)
    }
  }
  deriv.DerivedFormLookups(
    name_to_field:,
    field_to_name:,
    field_to_type:,
    field_to_dom_id: deriv.inspect,
    field_to_default_label: deriv.field_to_default_label(_, field_to_name:),
    field_values:,
  )
}

pub fn example_lustre_html_form_for_form(
  submit_msg submit_msg: fn(List(#(String, String))) -> msg,
  form form: form.Form(Form),
) -> f.Element(msg) {
  let input = fn(field, label_str) {
    let input =
      f.input(
        field:,
        overrides: f.placeholder(label_str),
        err: deriv.none,
        lookup: form_field_lookups(),
        form:,
      )
    let errs =
      f.ul(
        [],
        deriv.list_map(input.field.errs, fn(err) { f.li([], [f.text(err)]) }),
      )
    f.div([], [
      f.div([], [f.label([f.for(input.field.id)], [f.text(label_str)])]),
      f.div([], [input.render(f.InputParams(class: deriv.none)), errs]),
    ])
  }
  f.form([f.on_submit(submit_msg)], [
    input(FormStr, "Str"),
    input(FormInt, "Int"),
    input(FormOptionStr, "Option Str"),
    input(FormOptionInt, "Option Int"),
    input(FormListStr, "List Str"),
    input(FormListInt, "List Int"),
    f.p([], [f.button([f.type_("submit")], [f.text("Submit")])]),
  ])
}
