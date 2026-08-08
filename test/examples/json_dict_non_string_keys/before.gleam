import gleam/dict.{type Dict}

pub type StrKeyDict =
  //$ derive json decode encode
  Dict(String, String)

pub type IntKeyDict =
  //$ derive json decode encode
  Dict(Int, String)

pub type FloatKeyDict =
  //$ derive json decode encode
  Dict(Float, String)

pub type BoolKeyDict =
  //$ derive json decode encode
  Dict(Bool, String)

pub type Foo {
  //$ derive json decode encode
  Foo(
    foo: Dict(Int, String),
  )
}
