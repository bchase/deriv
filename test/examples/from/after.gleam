import birl.{type Time}
import examples/from/authe/a.{type AutheA}
import examples/from/authe/b.{type AutheB}
import examples/from/friend/person.{type Person}
import examples/from/friend/pet.{type Pet}
import gleam/option.{type Option}
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
    all_star: Int,
    mt: Int,
    t: Int,
    mtf: Int,
    tf: Int,
    f: Int,
    o: Option(Int),
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

pub fn from_authe_a_to_authe_tokens(value: AutheA) -> AutheTokens {
  Authe(
    id: value.authe_id,
    encrypted_access_token: value.encrypted_access_token,
    encrypted_refresh_token: value.encrypted_refresh_token,
  )
}

pub fn from_authe_b_to_authe_tokens(value: AutheB) -> AutheTokens {
  Authe(
    id: value.authe_id,
    encrypted_access_token: value.encrypted_access_token,
    encrypted_refresh_token: value.encrypted_refresh_token,
  )
}

pub fn from_person_to_friend(value: Person) -> Friend {
  Friend(name: value.first_name)
}

pub fn from_pet_to_friend(value: Pet) -> Friend {
  Friend(name: value.name)
}

pub fn from_person_to_has_age(value: Person) -> HasAge {
  HasAge(human_years: value.age)
}

pub fn from_pet_to_has_age(value: Pet) -> HasAge {
  HasAge(human_years: value.age |> pet.to_human_years)
}

pub fn from_a_to_overrides(value: A) -> Overrides {
  Overrides(
    all: value.all |> birl.from_unix,
    all_star: value.all_star |> birl.from_unix,
    mt: value.mt |> birl.from_unix,
    t: value.t |> birl.from_unix,
    mtf: value.mtf |> birl.from_unix,
    tf: value.tf |> birl.from_unix,
    f: value.f |> birl.from_unix,
    o: value.o |> option.map(birl.from_unix),
  )
}

pub fn from_a_to_override_using_type_not_field(
  value: A,
) -> OverrideUsingTypeNotField {
  OverrideUsingTypeNotField(mt: value |> to_time, t: value |> to_time)
}
