import birl.{type Time}
import deriv/util
import gleam/option.{type Option, None}
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

pub fn zero_player() -> Player {
  Player(
    util.zero_uuid(),
    "",
    None,
    util.zero_time(),
    0,
    0.0,
    [],
    False,
    time_zero_override(),
  )
}

pub fn zero_unit() -> Unit {
  Unit
}

pub fn zero_param(passed passed: t) -> Param(t) {
  Param("", passed)
}
