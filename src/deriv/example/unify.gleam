import gleam/option.{type Option, Some, None}
import youid/uuid.{type Uuid}

// // //$ unify func PROJ/some/path/a.AutheA authe_type_a
// // //$ unify func PROJ/some/path/b.AutheB authe_type_b

// src/PROJ/types.gleam
pub type AutheTokens {
  //$ derive unify(PROJ/some/path/a.AutheA,PROJ/some/path/b.AutheB)
  Authe(
    id: Uuid,
    //$ unify field PROJ/some/path/a.AutheA authe_id
    //$ unify field PROJ/some/path/b.AutheA authe_id
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}

pub fn authe_type_a(value: AutheTypeA) -> AutheTokens {
  Authe(
   id: value.authe_id,
   encrypted_access_token: value.encrypted_access_token,
   encrypted_refresh_token: value.encrypted_refresh_token,
  )
}

pub fn authe_type_b(value: AutheTypeB) -> AutheTokens {
  Authe(
   id: value.authe_id,
   encrypted_access_token: value.encrypted_access_token,
   encrypted_refresh_token: value.encrypted_refresh_token,
  )
}

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

// src/PROJ/some/path/a.gleam
pub type AutheTypeA {
  AutheTypeA(
    user_id: Uuid,
    name: String,
    email: String,
    org_id: Uuid,
    authe_id: Uuid,
    provider: String,
    uid: Option(String),
    aid: Option(String),
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}

// src/PROJ/some/path/b.gleam
pub type AutheTypeB {
  AutheTypeB(
    user_id: Uuid,
    name: String,
    email: String,
    org_id: Uuid,
    authe_id: Uuid,
    provider: String,
    uid: Option(String),
    aid: Option(String),
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}
