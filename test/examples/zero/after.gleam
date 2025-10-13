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
  )
}

pub type Foo {
  //$ derive zero
  Bar
}

pub fn zero_player() -> Player {
  Player(util.zero_uuid(), "", None, util.zero_time(), 0, 0.0, [], False)
}

pub fn zero_foo() -> Foo {
  Bar
}
