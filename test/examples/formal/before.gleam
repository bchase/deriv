import gleam/option.{type Option}

pub type Form {
  //$ derive form
  Form(
    str: String,
    str_option: Option(String),
    str_list: List(String),
    //
    int: Int,
    int_option: Option(Int),
    int_list: List(Int),
    //
    float: Float,
    float_option: Option(Float),
    float_list: List(Float),
    //
    bool: Bool,
    bool_option: Option(Bool),
    bool_list: List(Bool),
    //
    nested: NestedForm,
  )
}

pub type NestedForm {
  NestedForm(
    str: String,
  )
}
