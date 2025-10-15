import deriv/lustre as f
import deriv/util
import formal/form
import gleam/list
import gleam/option.{None}

pub type Form {
  //$ derive form lookups lustre
  Form(
    str: String,
  )
}

pub type FormField {
  FormStr
}

pub fn form_form() -> form.Form(Form) {
  form.new({
    use str <- form.field("str", { form.parse_string })
    form.success(Form(str:))
  })
}

pub fn form_field_lookups() -> util.DerivedFormLookups(FormField, Form) {
  let field_to_name = fn(field) {
    case field {
      FormStr -> "str"
    }
  }
  let name_to_field = fn(name) {
    case name {
      "str" -> Ok(FormStr)
      _ -> Error(Nil)
    }
  }
  let field_to_type = fn(field) {
    case field {
      FormStr -> util.String
    }
  }
  util.DerivedFormLookups(
    name_to_field:,
    field_to_name:,
    field_to_type:,
    field_to_dom_id: util.inspect,
    field_to_default_label: util.field_to_default_label(_, field_to_name:),
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
        err: None,
        lookup: form_field_lookups(),
        form:,
      )
    let errs =
      f.ul([], list.map(input.field.errs, fn(err) { f.li([], [f.text(err)]) }))
    f.div([], [
      f.div([], [f.label([f.for(input.field.id)], [f.text(label_str)])]),
      f.div([], [input.render(f.InputParams(class: None)), errs]),
    ])
  }
  f.form([f.on_submit(submit_msg)], [
    input(FormStr, "Str"),
    f.p([], [f.button([f.type_("submit")], [f.text("Submit")])]),
  ])
}
