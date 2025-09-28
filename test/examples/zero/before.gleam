import birl.{type Time}
import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type Player {
  //$ derive zero
  Player(
    id: Uuid,
    name: String,
    alias: Option(String),
    bday: Time,
    age: Int,
    kd: Float,
    hobbies: List(String),
    active: Bool,
  )
}
