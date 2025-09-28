import gleam/dict.{type Dict}

// simple type alias
pub type Fields =
  //$ derive json decode encode
  Dict(String, String)

// recursive type aliases
pub type Foo =
  //$ derive json decode encode
  Bar
pub type Bar =
  //$ derive json decode encode
  String

pub type Listy(t) =
  //$ derive json decode encode
  List(t)
