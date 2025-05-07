import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/string
import gleam/list
import gleam/result
import gleam/http
import gleam/regexp
import lustre/element.{type Element, text}
import lustre/element/html
import lustre/attribute as attr
import form.{type Form, type Fields, type Lookups, type GleamType, Form, String, Int, Float, Bool, Option, List, Enum, Uuid}

pub type InputErrs(field) = Dict(#(field, List(Int)), List(String))

pub opaque type InputOverrides {
  InputOverrides(
    label: Option(String),
    placeholder: Option(String),
  )
}

pub type Endpoint {
  Endpoint(
    method: http.Method,
    action: String,
  )
}

pub type RenderFunc(field, msg) = fn(Fields(field), InputErrs(field), Endpoint) -> Element(msg)

pub const overrides =
  InputOverrides(
    label: None,
    placeholder: None,
  )

pub fn override_label(overrides: InputOverrides, label: String) -> InputOverrides {
  InputOverrides(..overrides, label: Some(label))
}

pub fn override_placeholder(overrides: InputOverrides, placeholder: String) -> InputOverrides {
  InputOverrides(..overrides, placeholder: Some(placeholder))
}

pub fn no_fields() -> Fields(field) {
  dict.new()
}

pub fn no_errors() -> InputErrs(field) {
  dict.new()
}

pub fn form(
  id id: String,
  method method: http.Method,
  action action: String,
  attrs attrs: List(attr.Attribute(msg)),
  children children: List(Element(msg)),
) -> Element(msg) {
  let method_str =
    case method {
      http.Get -> "get"
      http.Post -> "post"
      http.Patch -> "patch"
      http.Put -> "put"
      http.Delete -> "delete"
      http.Head -> "head"
      http.Options -> "options"
      http.Connect -> "connect"
      http.Trace -> "connect"
      http.Other(other) -> other |> string.lowercase
    }

  let attrs =
    [
      attr.id(id),
      attr.enctype("multipart/form-data"),
      attr.attribute("hx-"<>method_str, action),
      attr.attribute("hx-disabled-elt", "find button[type=submit], find input[type=submit]"),

      // FAILURE
      attr.attribute("hx-target-422", "#" <> id),
      attr.attribute("hx-swap", "outerHTML"),
      attr.attribute("hx-swap-oob", "outerHTML"),
    ]
    |> list.append(attrs)

  html.form(attrs, children)
}

pub type Input(msg) {
  Input(
    id: String,
    type_: GleamType,
    name: String,
    required: Bool,
    label: String,
    placeholder: Option(String),
    value: Option(attr.Attribute(msg)),
    errs: List(String),
  )
}

pub fn build_input(
  field: field,
  fields: Fields(field),
  lookup: Lookups(field),
  errs: InputErrs(field),
  overrides: InputOverrides,
) -> Input(msg) {
  let id = lookup.field_to_id(field)
  let name = lookup.field_to_name(field)
  let type_ = lookup.field_to_type(field)
  let required = lookup.field_is_required(field)

  let label =
    overrides.label
    |> option.unwrap(lookup.field_to_label(field))

  let placeholder = overrides.placeholder

  let errs =
    errs
    |> dict.get(#(field, [])) // TODO arrays
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    // TODO
    |> result.unwrap([])

  let value = value_scalar(fields, field, type_)

  Input(id:, type_:, name:, required:, label:, placeholder:, value:, errs:)
}

pub fn values_array(
  fields: Fields(field),
  field: field,
  type_: GleamType,
) -> Option(List(attr.Attribute(msg))) {
  case dict.get(fields, field) {
    Ok(vals) ->
      vals
      |> list.map(fn(val) {
        case type_ {
          Bool -> attr.checked(True)
          _ -> attr.value(val)
        }
      })
      |> Some

    _ -> None
  }
}

pub fn value_scalar(
  fields: Fields(field),
  field: field,
  type_: GleamType,
) -> Option(attr.Attribute(msg)) {
  case dict.get(fields, field) {
    Ok([val, ..]) ->
      case type_ {
        Bool ->
          case form.parse_bool(val) {
            Ok(True) -> attr.checked(True)
            _ -> attr.checked(False)
          }

        _ ->
          attr.value(val)
      }
      |> Some

    _ -> None
  }
}

pub fn input(
  field: field,
  fields: Fields(field),
  lookup: Lookups(field),
  errs: InputErrs(field),
  col_width: Int,
  overrides: InputOverrides,
) -> Element(msg) {
  let x = build_input(field, fields, lookup, errs, overrides)
  let attrs = build_input_attrs(x, None, True)

  html.div([
    attr.class("col-md-" <> int.to_string(col_width))
  ], [
    html.label([
      attr.for(x.id),
    ], [
      text(x.label),
    ]),

    case x.type_ {
      Bool | Option(Bool) ->
        html.div([], [
          html.input(attrs),
          errs_element(x.errs),
        ])

      _ ->
        html.span([], [
          html.input(attrs),
          errs_element(x.errs),
        ])
    },
  ])
}

pub type SelectOpt {
  SelectOpt(
   label: String,
   value: String,
  )
}

pub fn select(
  field: field,
  fields: Fields(field),
  lookup: Lookups(field),
  errs: InputErrs(field),
  col_width: Int,
  overrides: InputOverrides,
  placeholder: Option(String),
  opts: List(SelectOpt),
  selected: Option(String),
) -> Element(msg) {
  let x = build_input(field, fields, lookup, errs, overrides)
  let attrs = build_input_attrs(x, Some("form-select"), False)

  let opts =
    list.map(opts, fn(opt) {
      let attrs =
        [
          attr.value(opt.value),
        ]

      let conditional_attrs =
        case selected {
          Some(sel) if opt.value == sel ->
            [ attr.selected(True) ]

          _ ->
            []
        }

      let attrs =
        list.append(attrs, conditional_attrs)

      html.option(attrs, opt.label)
    })

  let opts =
    case placeholder {
      Some(placeholder) ->
        html.option(
          [
            attr.value(""),
            attr.disabled(True),
            attr.selected(option.is_none(selected)),
          ],
          placeholder,
        )
        |> list.wrap
        |> list.append(opts)

      None ->
        opts
    }


  html.div([
    attr.class("col-md-" <> int.to_string(col_width))
  ], [
    html.label([
      attr.for(x.id),
    ], [
      text(x.label),
    ]),

    html.div([], [
      html.select(attrs, opts),
      errs_element(x.errs),
    ])
  ])
}

pub fn inputs(
  field: field,
  fields: Fields(field),
  lookup: Lookups(field),
  errs: InputErrs(field),
  col_width: Int,
  overrides: InputOverrides,
) -> Element(msg) {
  let x = build_input(field, fields, lookup, errs, overrides)
  let attrs = build_input_attrs(x, None, True)

  // let values = values_array(fields, field, x.type_)

  //   // errs
  // }

  html.div([
    attr.class("col-md-" <> int.to_string(col_width))
  ], [
    html.label([
      attr.for(x.id),
    ], [
      text(x.label),
    ]),

    case x.type_ {
      Bool ->
        html.div([], [
          html.input(attrs),
          errs_element(x.errs),
        ])

      _ ->
        html.span([], [
          html.input(attrs),
          errs_element(x.errs),
        ])
    },
  ])
}

pub fn build_input_attrs(
  x: Input(msg),
  class class: Option(String),
  type_ type_: Bool,
) -> List(attr.Attribute(msg)) {
  let input_type = gleam_type_to_input_type(x.type_)

  let class =
    case class, input_type {
      Some(class), _ -> class
      _, "checkbox" -> ""
      _, _ -> "form-control"
    }

  let attrs =
    [
      attr.name(x.name),
      attr.id(x.id),
      attr.class(class),
      // attr.required(required),
    ]

  let attrs =
    case type_ {
      True -> attrs |> list.append([attr.type_(input_type)])
      False -> attrs
    }

  let attrs =
    case x.value {
      Some(value) ->
        [value, ..attrs]

      None ->
        attrs
    }

  let attrs =
    case overrides.placeholder {
      Some(placeholder) -> [attr.placeholder(placeholder), ..attrs]
      None -> attrs
    }

  let attrs =
    case x.errs {
      [] -> attrs
      _has_errs -> [attr.class("is-invalid"), ..attrs]
    }

  attrs
}

fn input_type_label(t: form.GleamType) -> String {
  case t {
    String -> "text"
    Int -> "integer"
    Float -> "float"
    Bool -> "boolean" // TODO
    Option(t) -> input_type_label(t)
    List(t) -> input_type_label(t)
    Enum(ident:) -> todo
    Uuid -> "UUID"
  }
}

fn gleam_type_to_input_type(t: GleamType) -> String {
  case t {
    String -> "text"
    Int -> "number"
    Float -> "number"
    Bool -> "checkbox"
    Option(t) -> gleam_type_to_input_type(t)
    List(t) -> gleam_type_to_input_type(t)
    Enum(ident:) -> todo
    Uuid -> "hidden"
  }
}

fn errs_element(errs: List(String)) -> Element(msg) {
  case errs {
    [] ->
      text("")

    strs ->
      html.div([
        attr.class("invalid-feedback"),
      ], [
        html.ul([],
          strs
          |> list.map(fn(str) {
            html.li([], [text(str) ])
          }),
        ),
      ])
  }
}

pub fn parse_and_validate(
  form_data form_data: List(#(String, String)),
  lookup lookup: Lookups(field),
  decode decode: fn(Fields(field)) -> Result(form, form.Err(field)),
  validate validate: fn(form) -> Result(form, form.Err(field)),
  to_form_err to_form_err: fn(Fields(field), InputErrs(field)) -> err,
) -> Result(Form(form, field), err) {
  let fields = form.fields(form_data, lookup.name_to_field)

  {
    use form <- result.try(decode(fields))
    use form <- result.try(validate(form))
    Ok(Form(lookup:, fields:, form:))
  }
  |> result.map_error(fn(err) {
    case err {
      form.FieldMissingError(field:) -> {
        [#(#(field, []), ["required"])]
        |> dict.from_list
        |> to_form_err(fields, _)
      }

      form.FieldDecodeError(field:, errs: _errs) -> {
        let type_ = lookup.field_to_type(field)

        let err_msg =
          case type_ {
            String ->
              "must be text"

            Int | Float ->
              "must be a number"

            Enum(ident: _ident) ->
              todo

            _ -> {
              let type_name = input_type_label(type_)
              let assert Ok(initial_vowel_re) = "^[AEIOUaeiou]" |> regexp.from_string
              let article =
                case regexp.check(initial_vowel_re, type_name) {
                  True -> "an"
                  False -> "a"
                }

              "must be " <> article <> " " <> type_name
            }
          }

        [#(#(field, []), [err_msg])]
        |> dict.from_list
        |> to_form_err(fields, _)
      }

      form.FormInvalid(errs:) ->
        errs
        |> to_form_err(fields, _)

      form.FieldLookupScalarHasMultipleValuesErr(field:) -> {
        let inspect = lookup.field_to_id(field)

        panic as { "`FieldLookupScalarHasMultipleValuesErr`: " <> inspect }
      }
    }
  })
}
