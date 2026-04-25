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

pub type PetPlusMismatch {
  //$ derive from PetPlus
  PetPlusMismatch(
    name: String,
    missing: Int,
    //$ from ignored
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

pub fn from_authe_a_to_authe_tokens(authe_a authe_a: AutheA) -> AutheTokens {
  Authe(
    id: authe_a.authe_id,
    encrypted_access_token: authe_a.encrypted_access_token,
    encrypted_refresh_token: authe_a.encrypted_refresh_token,
  )
}

pub fn from_authe_b_to_authe_tokens(authe_b authe_b: b.AutheB) -> AutheTokens {
  Authe(
    id: authe_b.authe_id,
    encrypted_access_token: authe_b.encrypted_access_token,
    encrypted_refresh_token: authe_b.encrypted_refresh_token,
  )
}

pub fn from_person_to_friend(person person: Person) -> Friend {
  Friend(name: person.first_name)
}

pub fn from_pet_to_friend(pet pet: p.Pet) -> Friend {
  Friend(name: pet.name)
}

pub fn from_person_to_has_age(person person: Person) -> HasAge {
  HasAge(human_years: person.age)
}

pub fn from_pet_to_has_age(pet pet: p.Pet) -> HasAge {
  HasAge(human_years: pet.age |> p.to_human_years)
}

pub fn from_a_to_overrides(a a: A) -> Overrides {
  Overrides(
    all: a.all |> birl.from_unix,
    all_star: a.all_star |> birl.from_unix,
    mt: a.mt |> birl.from_unix,
    t: a.t |> birl.from_unix,
    mtf: a.mtf |> birl.from_unix,
    tf: a.tf |> birl.from_unix,
    f: a.f |> birl.from_unix,
    o: a.o |> option.map(birl.from_unix),
    oi: a.oi |> option.map(birl.from_unix),
  )
}

pub fn from_a_to_override_using_type_not_field(
  a a: A,
) -> OverrideUsingTypeNotField {
  OverrideUsingTypeNotField(mt: a |> to_time, t: a |> to_time)
}

pub fn from_pet_to_pet_plus(pet pet: p.Pet, missing missing: Float) -> PetPlus {
  PetPlus(name: pet.name, missing:)
}

pub fn from_named_to_constr(named named: Named) -> Constr {
  Constr(newtype: named.name |> Newtype)
}

pub fn from_pet_plus_to_pet_plus_mismatch(
  pet_plus pet_plus: PetPlus,
  missing missing: Int,
) -> PetPlusMismatch {
  PetPlusMismatch(name: pet_plus.name, missing:)
}
