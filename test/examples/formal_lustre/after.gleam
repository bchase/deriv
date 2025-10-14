import deriv/lustre as f
import deriv/util
import formal/form
import gleam/list
import gleam/option.{None}
import gleam/string
import lustre/element
import lustre/element/html
import lustre/event

pub type Form {
  //$ derive form lustre example
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
    field_to_dom_id: string.inspect,
    field_to_default_label: util.field_to_default_label(_, field_to_name:),
  )
}

pub fn example_lustre_html_form_for_form(
  submit_msg submit_msg: fn(List(#(String, String))) -> msg,
  form form: form.Form(Form),
  lookup lookup: util.DerivedFormLookups(FormField, Form),
) -> element.Element(msg) {
  let input = fn(field, label_str) {
    let input =
      f.input(field:, overrides: f.label(label_str), err: None, lookup:, form:)
    let errs =
      html.ul(
        [],
        list.map(input.field.errs, fn(err) { html.li([], [html.text(err)]) }),
      )
    html.div([], [input.render(f.InputParams(class: None)), errs])
  }
  html.form([event.on_submit(submit_msg)], [input(FormStr, "Str")])
}
