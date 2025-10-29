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
    created_at__type_unspecified: Int,
    created_at__type_qualified__field__implicit: Int,
    created_at__type_unqualified__field__implicit: Int,
    created_at__type_qualified__field__explicit: Int,
    created_at__type_unqualified__field__explicit: Int,
    created_at__type_qualified__type: Int,
    created_at__type_unqualified__type: Int,
  )
}

fn to_time(value: A) -> Time {
  todo
}

pub type Overrides {
  //$ derive from examples/from/before.A
  Overrides(
    created_at__type_unspecified: Time,
    //$ from using birl.from_unix

    created_at__type_qualified__field__implicit: Time,
    //$ from examples/from/before.A.created_at__type_qualified__field__implicit using birl.from_unix
    created_at__type_unqualified__field__implicit: Time,
    //$ from A.created_at__type_qualified__field__implicit using birl.from_unix

    created_at__type_qualified__field__explicit: Time,
    //$ from examples/from/before.A.created_at__type_qualified__field__explicit using birl.from_unix
    created_at__type_unqualified__field__explicit: Time,
    //$ from A.created_at__type_qualified__field__explicit using birl.from_unix

    created_at__type_qualified__type: Time,
    //$ from examples/from/before.A using to_time
    created_at__type_unqualified__type: Time,
    //$ from A using to_time
  )
}
