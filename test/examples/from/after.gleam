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
