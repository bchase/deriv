import gleam/string
import gleam/list
import formal/form
import gleam/option.{type Option, Some, None}
import lustre/element/html
import lustre/element
import lustre/attribute as attr
import lustre/event
import deriv/util

// OVERRIDES

pub opaque type InputOverrides {
  InputOverrides(
    placeholder: Option(String),
    required: Bool,
  )
}

pub fn no_overrides() -> InputOverrides {
  zero_overrides()
}

fn zero_overrides() -> InputOverrides {
  InputOverrides(
    placeholder: None,
    required: False,
  )
}

pub fn placeholder(
  placeholder placeholder: String,
) -> InputOverrides {
  zero_overrides()
  |> placeholder_(placeholder:)
}

pub fn required(
  required required: Bool,
) -> InputOverrides {
  zero_overrides()
  |> required_(required:)
}

pub fn placeholder_(
  overrides overrides: InputOverrides,
  placeholder placeholder: String,
) -> InputOverrides {
  InputOverrides(..overrides, placeholder: Some(placeholder))
}

pub fn required_(
  overrides overrides: InputOverrides,
  required required: Bool,
) -> InputOverrides {
  InputOverrides(..overrides, required: required)
}

// IMPLEMENTATION

pub type InputRender(msg) {
  InputRender(
    field: InputField(msg),
    render: fn(InputParams) -> Element(msg),
  )
}

pub type InputParams {
  InputParams(
    class: Option(String),
  )
}

pub fn input(
  field field: field,
  //
  overrides overrides: InputOverrides,
  //
  err err: Option(fn(form.FieldError) -> String),
  //
  lookup lookup: util.DerivedFormLookups(field, form),
  form form: form.Form(form),
) -> InputRender(msg) {
  input_(field:, overrides:, err:, lookup:, form:, render: fn(_field, attrs) {
    html.input(attrs)
  })
}

pub fn input_(
  field field: field,
  //
  overrides overrides: InputOverrides,
  //
  err err: Option(fn(form.FieldError) -> String),
  render render: fn(InputField(msg), List(attr.Attribute(msg))) -> Element(msg),
  //
  lookup lookup: util.DerivedFormLookups(field, form),
  form form: form.Form(form),
) -> InputRender(msg) {
  let field = build_input_field(field:, lookup:, err:, overrides:, form:)

  InputRender(field:, render: fn(params: InputParams) {
    field
    |> build_input_attrs(class: params.class, set_type: True)
    |> render(field, _)
  })
}

pub fn input_labelled_bs(
  input input: InputRender(msg),
  label label: String,
  wrapper_class wrapper_class: Option(String),
) -> Element(msg) {
  let wrapper =
    case input.field.type_ {
      util.Bool | util.Option(util.Bool) ->
        html.div([], _)

      _ ->
        html.span([], _)
    }

  let bs_input_class =
    case gleam_type_to_input_type(input.field.type_) {
      "checkbox" -> []
      _ -> ["form-control"]
    }

  let bs_err_class =
    case input.field.errs {
      [] -> []
      _has_errs -> ["is-invalid"]
    }

  let bs_classes =
    [
      bs_input_class,
      bs_err_class,
    ]
    |> list.flatten
    |> string.join(" ")
    |> Some

  let wrapped_input =
    wrapper([
      input.render(InputParams(class: bs_classes)),
      errs_bs(input.field.errs),
    ])

  let wrapper_attrs =
    case wrapper_class {
      Some(class) -> [attr.class(class)]
      None -> []
    }

  // attr.class("col-md-" <> int.to_string(col_width))
  html.div(wrapper_attrs, [
    html.label([
      attr.for(input.field.id),
    ], [
      html.text(label),
    ]),

    wrapped_input,
  ])
}

fn errs_bs(errs: List(String)) -> Element(msg) {
  case errs {
    [] ->
      html.text("")

    strs ->
      html.div([
        attr.class("invalid-feedback"),
      ], [
        html.ul([],
          strs
          |> list.map(fn(str) {
            html.li([], [html.text(str) ])
          }),
        ),
      ])
  }
}

pub type InputField(msg) {
  InputField(
    id: String,
    type_: util.GleamType,
    name: String,
    required: Bool,
    placeholder: Option(String),
    value: String,
    errs: List(String),
  )
}

fn build_input_field(
  field field: field,
  lookup lookup: util.DerivedFormLookups(field, form),
  overrides overrides: InputOverrides,
  err err: Option(fn(form.FieldError) -> String),
  form form: form.Form(form),
) -> InputField(msg) {
  let name = lookup.field_to_name(field)
  let type_ = lookup.field_to_type(field)

  let id = lookup.field_to_dom_id(field)
  let required = False

  let placeholder = overrides.placeholder

  let value =
    form
    |> form.field_value(name)

  let errs =
    case err {
      Some(to_str) -> {
        form
        |> form.field_errors(name)
        |> list.map(to_str)
      }

      None -> {
        form
        |> form.field_error_messages(name)
      }
    }

  InputField(id:, type_:, name:, required:, placeholder:, value:, errs:)
}

fn build_input_attrs(
  input x: InputField(msg),
  class class: Option(String),
  set_type set_type: Bool,
  // overrides overrides: InputOverrides,
) -> List(attr.Attribute(msg)) {
  let input_type = gleam_type_to_input_type(x.type_)

  let classes =
    case class  {
      Some(class) -> [attr.class(class)]
      None -> []
    }

  let type_ =
    case set_type {
      True -> [attr.type_(input_type)]
      False -> []
    }

  let placeholder =
    case x.placeholder {
      Some(placeholder) -> [attr.placeholder(placeholder)]
      None -> []
    }

  [
    attr.name(x.name),
    attr.id(x.id),
    attr.value(x.value),
    // attr.required(required),
  ]
  |> list.append(classes)
  |> list.append(type_)
  |> list.append(placeholder)
}

fn gleam_type_to_input_type(t: util.GleamType) -> String {
  case t {
    util.String -> "text"
    util.Int -> "number"
    util.Float -> "number"
    util.Bool -> "checkbox"
    util.Option(t) -> gleam_type_to_input_type(t)
    util.List(t) -> gleam_type_to_input_type(t)
    util.Uri |
    util.Date |
    util.TimeOfDay ->
      panic as "unimplemented"
  }
}

// lustre re-exports

pub type Element(msg) = element.Element(msg)
pub const div = html.div
pub const p = html.p
pub const button = html.button
pub const form = html.form
pub const ul = html.ul
pub const li = html.li
pub const label = html.label
pub const text = html.text
pub const for = attr.for
pub const type_ = attr.type_
pub const on_submit = event.on_submit
