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
