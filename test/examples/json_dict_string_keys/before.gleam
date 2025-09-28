import gleam/dict.{type Dict}

pub type DictFieldType {
  //$ derive json decode encode
  DictFieldType(
    dict: Dict(String, Int),
  )
}
