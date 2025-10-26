import gleam/dict.{type Dict}

pub type IntKeyDict =
  //$ derive json decode
  Dict(Int, String)

pub type FloatKeyDict =
  //$ derive json decode
  Dict(Float, String)

pub type BoolKeyDict =
  //$ derive json decode
  Dict(Bool, String)
