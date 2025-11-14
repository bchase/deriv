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
    rename: String,
    //$ json named alias
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
