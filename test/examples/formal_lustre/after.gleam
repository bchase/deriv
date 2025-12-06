import deriv/lustre as f
import deriv/util as deriv
import formal/form
import gleam/option.{type Option}

pub type Form {
  //$ derive form lookups lustre
  Form(
    str: String,
    option_str: Option(String),
  )
}

pub type FormField {
  FormStr
  FormOptionStr
}

pub fn form_form() -> form.Form(Form) {
  form.new({
    use str <- form.field("str", { form.parse_string })
    use option_str <- form.field("option_str", {
      form.parse_string |> form.parse_optional
    })
    form.success(Form(str:, option_str:))
  })
}

pub fn form_field_lookups() -> deriv.DerivedFormLookups(FormField, Form) {
  let field_to_name = fn(field) {
    case field {
      FormStr -> "str"
      FormOptionStr -> "option_str"
    }
  }
  let name_to_field = fn(name) {
    case name {
      "str" -> Ok(FormStr)
      "option_str" -> Ok(FormOptionStr)
      _ -> Error(Nil)
    }
  }
  let field_to_type = fn(field) {
    case field {
      FormStr -> deriv.String
      FormOptionStr -> deriv.Option(deriv.String)
    }
  }
  deriv.DerivedFormLookups(
    name_to_field:,
    field_to_name:,
    field_to_type:,
    field_to_dom_id: deriv.inspect,
    field_to_default_label: deriv.field_to_default_label(_, field_to_name:),
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
    input(FormOptionStr, "Option Str"),
    f.p([], [f.button([f.type_("submit")], [f.text("Submit")])]),
  ])
}
