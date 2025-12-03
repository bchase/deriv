import birl.{type Time}
import examples/from/authe/a.{type AutheA}
import examples/from/authe/b
import examples/from/friend/person.{type Person}
import examples/from/friend/pet as p
import gleam/option.{type Option, None}
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
    //$ from examples/from/friend/pet.Pet.age using p.to_human_years
  )
}

pub type A {
  A(
    all: Int,
    all_star: Int,
    mt: Int,
    t: Int,
    mtf: Int,
    tf: Int,
    f: Int,
    o: Option(Int),
    oi: Option(Int),
  )
}

pub fn to_time(_) -> Time {
  birl.from_unix(0)
}

pub type Overrides {
  //$ derive from examples/from/before.A
  Overrides(
    all: Time,
    //$ from using birl.from_unix
    all_star: Time,
    //$ from * using birl.from_unix

    mt: Time,
    //$ from examples/from/before.A using birl.from_unix
    t: Time,
    //$ from A using birl.from_unix

    mtf: Time,
    //$ from examples/from/before.A.mtf using birl.from_unix
    tf: Time,
    //$ from A.tf using birl.from_unix

    f: Time,
    //$ from *.f using birl.from_unix

    o: Option(Time),
    //$ from *.o using inner birl.from_unix

    oi: Option(Time),
    //$ from using inner birl.from_unix
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

pub type PetPlus {
  //$ derive from examples/from/friend/pet.Pet
  PetPlus(
    name: String,
    missing: Float,
  )
}

pub type Newtype {
  Newtype(
    name: String,
  )
}

pub type Named {
  Named(
    name: String,
  )
}

pub type Constr {
  //$ derive from Named
  Constr(
    newtype: Newtype,
    //$ from Named.name using Newtype
  )
}

pub type Id(resource) {
  Id(id: String)
}

pub type ThingList {
  ThingList(
    id: Id(Thing),
    name: String,
  )
}

pub type ThingShow {
  ThingShow(
    id: Id(Thing),
    name: String,
  )
}

pub type Thing {
  // derive from ThingList
  // derive from ThingShow
  Thing(
    id: String,
    name: String,
  )
}

pub fn suppress_warning_authe_a() ->  AutheA {
  let uuid = uuid.v7_from_millisec(0)
  a.AutheA(uuid, "", "", uuid, uuid, "", None, None, "", "")
}
pub fn suppress_warning_authe_b() ->  b.AutheB {
  let uuid = uuid.v7_from_millisec(0)
  b.AutheB(uuid, "", "", uuid, uuid, "", None, None, "", "")
}
pub const suppress_warning_person: Person = person.Person(0, "", "", 0)
pub const suppress_warning_pet: p.Pet = p.Pet(0, "", p.PetAge(p.Cat, 0))
