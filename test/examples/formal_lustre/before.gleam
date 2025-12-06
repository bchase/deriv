import gleam/option.{type Option}

pub type Form {
  //$ derive form lookups lustre
  Form(
    str: String,
    option_str: Option(String),
  )
}
