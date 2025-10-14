import deriv/lustre as f
import deriv/util
import formal/form
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/time/calendar
import gleam/uri
import lustre/attribute as attr
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub type Form {
  //$ derive form lustre example
  Form(
    str: String,
  )
}
