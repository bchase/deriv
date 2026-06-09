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
    last_active: Time,
    //$ zero time_zero_override
  )
}

pub type Unit {
  //$ derive zero
  Unit
}

pub type Param(t) {
  //$ derive zero
  Param(
    inferred: String,
    passed: t,
  )
}

pub fn time_zero_override() -> Time { birl.unix_epoch }
