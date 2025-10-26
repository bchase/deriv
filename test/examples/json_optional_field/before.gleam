import gleam/option.{type Option}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
  )
}
