import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type AutheA {
  AutheA(
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
