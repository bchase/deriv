import gleam/dict.{type Dict}

// simple type alias
pub type Fields =
  //$ derive json decode encode
  Dict(String, String)

pub type Listy(t) =
  //$ derive json decode encode
  List(t)
