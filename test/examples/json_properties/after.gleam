import deriv/util
import gleam/dict.{type Dict}
import gleam/option.{type Option}

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

pub type Mono {
  //$ derive json properties
  Mono(
    foo: String,
    bar_bar: String,
    //$ json named bar
    foo_bar: String,
    //$ json named foo.bar
  )
}

pub type Nesting {
  //$ derive json properties
  Nesting(
    ignore: Ignored,
    //$ json properties ignore
    option: Option(String),
    list: List(String),
    nested: Nested,
    nested_list: List(NestedItem),
  )
}

pub type Ignored {
  Ignored(
    should_see_this: String,
  )
}

pub type Nested {
  Nested(
    scalar: String,
    nested: NestedDouble,
  )
}

pub type NestedDouble {
  NestedDouble(
    double: String,
    nested: NestedTriple,
  )
}

pub type NestedTriple {
  NestedTriple(
    triple: String,
  )
}

pub type NestedItem {
  NestedItem(
    list: String,
  )
}

pub fn json_properties_for_type() -> Dict(String, List(String)) {
  [
    #("Variant1", ["field1", "override", "nested.override"]),
    #("Variant2", ["other_field"]),
  ]
  |> dict.from_list
}

pub fn json_properties_for_mono() -> List(String) {
  ["foo", "bar", "foo.bar"]
}

pub fn json_properties_for_nesting() -> List(String) {
  [
    "option", "list", "nested.scalar", "nested.nested.double",
    "nested.nested.nested.triple", "nested_list.list",
  ]
}
