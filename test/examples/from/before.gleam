import examples/from/authe/a.{type AutheA}
import examples/from/authe/b.{type AutheB}
import examples/from/friend/person.{type Person}
import examples/from/friend/pet.{type Pet}
import youid/uuid.{type Uuid}

pub type AutheTokens {
  //$ derive unify examples/from/authe/a.AutheA
  //$ derive unify examples/from/authe/b.AutheB
  Authe(
    id: Uuid,
    //$ unify field examples/from/authe/a.AutheA authe_id
    //$ unify field examples/from/authe/b.AutheB authe_id
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}

pub type Friend {
  //$ derive unify examples/from/friend/person.Person
  //$ derive unify examples/from/friend/pet.Pet
  Friend(
    name: String,
    //$ unify field examples/from/friend/person.Person first_name
  )
}
