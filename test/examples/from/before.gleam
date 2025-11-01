import birl.{type Time}
import examples/from/authe/a.{type AutheA}
import examples/from/authe/b.{type AutheB}
import examples/from/friend/person.{type Person}
import examples/from/friend/pet.{type Pet}
import youid/uuid.{type Uuid}

pub type AutheTokens {
  //$ derive from examples/from/authe/a.AutheA
  //$ derive from examples/from/authe/b.AutheB
  Authe(
    id: Uuid,
    //$ from examples/from/authe/a.AutheA.authe_id
    //$ from examples/from/authe/b.AutheB.authe_id
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}

pub type Friend {
  //$ derive from examples/from/friend/person.Person
  //$ derive from examples/from/friend/pet.Pet
  Friend(
    name: String,
    //$ from examples/from/friend/person.Person.first_name
  )
}

pub type HasAge {
  //$ derive from examples/from/friend/person.Person
  //$ derive from examples/from/friend/pet.Pet
  HasAge(
    human_years: Int,
    //$ from examples/from/friend/person.Person.age
    //$ from examples/from/friend/pet.Pet.age using pet.to_human_years
  )
}

pub type A {
  A(
    all: Int,
    mt: Int,
    t: Int,
    mtf: Int,
    tf: Int,
  )
}

fn to_time(value: A) -> Time {
  todo
}

pub type Overrides {
  //$ derive from examples/from/before.A
  Overrides(
    all: Time,
    //$ from using birl.from_unix

    mt: Time,
    //$ from examples/from/before.A using birl.from_unix
    t: Time,
    //$ from A using birl.from_unix

    mtf: Time,
    //$ from examples/from/before.A.mtf using birl.from_unix
    tf: Time,
    //$ from A.tf using birl.from_unix
  )
}

pub type OverrideUsingTypeNotField {
  //$ derive from examples/from/before.A
  OverrideUsingTypeNotField(
    mt: Time,
    //$ from A* using to_time

    t: Time,
    //$ from examples/from/before.A* using to_time
  )
}
