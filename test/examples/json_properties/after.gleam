import deriv/util
import gleam/dict.{type Dict}

pub type Type {
  //$ derive json properties
  Variant1(
    field1: String,
    field2: String,
    //$ json named override
    field3: String,
    //$ json named nested.override
  )
  Variant2(other_field: String)
}

pub fn json_properties_for_type() -> Dict(String, List(String)) {
  [
    #("Variant1", ["field1", "override", "nested.override"]),
    #("Variant2", ["other_field"]),
  ]
  |> dict.from_list
}
