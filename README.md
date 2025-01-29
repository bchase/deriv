# `deriv`

[![Package Version](https://img.shields.io/hexpm/v/deriv)](https://hex.pm/packages/deriv)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/deriv/)

```sh
gleam add deriv@1
```

## Usage

```gleam
import youid/uuid.{type Uuid}

pub type User {
  User(
    id: Uuid,
    name: String,
  )
} //$ derive json(decode,encode)

pub type Post {
  Post(
    id: Uuid,
    title: String,
    body: String, //$ json(named(content))
    draft: Bool,
    tags: List(String),
    created_by: User,
  )
} //$ derive json(decode,encode)
```

```
$ gleam run -m deriv
```

```gleam
import decode.{type Decoder}
import deriv/util
import gleam/json.{type Json}
import gleam/list
import youid/uuid.{type Uuid}

pub type User {
  User(
    id: Uuid,
    name: String,
  )
} //$ derive json(decode,encode)

pub type Post {
  Post(
    id: Uuid,
    title: String,
    body: String, //$ json(named(content))
    draft: Bool,
    tags: List(String),
    created_by: User,
  )
} //$ derive json(decode,encode)

pub fn decoder_user() -> Decoder(User) {
  decode.one_of([decoder_user_user()])
}

pub fn decoder_user_user() -> Decoder(User) {
  decode.into({
    use id <- decode.parameter
    use name <- decode.parameter
    User(id:, name:)
  })
  |> decode.field("id", util.decoder_uuid())
  |> decode.field("name", decode.string)
}

pub fn encode_user(value: User) -> Json {
  case value {
    User(..) as value ->
      json.object([
        #("id", util.encode_uuid(value.id)),
        #("name", json.string(value.name)),
      ])
  }
}

pub fn decoder_post() -> Decoder(Post) {
  decode.one_of([decoder_post_post()])
}

pub fn decoder_post_post() -> Decoder(Post) {
  decode.into({
    use id <- decode.parameter
    use title <- decode.parameter
    use body <- decode.parameter
    use draft <- decode.parameter
    use tags <- decode.parameter
    use created_by <- decode.parameter
    Post(id:, title:, body:, draft:, tags:, created_by:)
  })
  |> decode.field("id", util.decoder_uuid())
  |> decode.field("title", decode.string)
  |> decode.field("content", decode.string)
  |> decode.field("draft", decode.bool)
  |> decode.field("tags", decode.list(decode.string))
  |> decode.field("created_by", decoder_user())
}

pub fn encode_post(value: Post) -> Json {
  case value {
    Post(..) as value ->
      json.object([
        #("id", util.encode_uuid(value.id)),
        #("title", json.string(value.title)),
        #("content", json.string(value.body)),
        #("draft", json.bool(value.draft)),
        #("tags", json.preprocessed_array(list.map(value.tags, json.string))),
        #("created_by", encode_user(value.created_by)),
      ])
  }
}
```

Further documentation can be found at <https://hexdocs.pm/deriv>.

## Development

```sh
make watch-tests # NOTE: requires `entr`
```
