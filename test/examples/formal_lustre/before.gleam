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
