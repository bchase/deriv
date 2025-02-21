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
  //$ derive json decode encode
  User(id: Uuid, name: String)
}

pub type Post {
  //$ derive json decode encode
  Post(
    id: Uuid,
    title: String,
    body: String,
    //$ json named content
    draft: Bool,
    tags: List(String),
    created_by: User,
    meta_nested_key: String,
    //$ json named meta.nested.key
  )
}
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
  //$ derive json decode encode
  User(id: Uuid, name: String)
}

pub type Post {
  //$ derive json decode encode
  Post(
    id: Uuid,
    title: String,
    body: String,
    //$ json named content
    draft: Bool,
    tags: List(String),
    created_by: User,
    meta_nested_key: String,
    //$ json named meta.nested.key
  )
}

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
    use meta_nested_key <- decode.parameter
    Post(id:, title:, body:, draft:, tags:, created_by:, meta_nested_key:)
  })
  |> decode.field("id", util.decoder_uuid())
  |> decode.field("title", decode.string)
  |> decode.field("content", decode.string)
  |> decode.field("draft", decode.bool)
  |> decode.field("tags", decode.list(decode.string))
  |> decode.field("created_by", decoder_user())
  |> decode.subfield(["meta", "nested", "key"], decode.string)
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
        #(
          "meta",
          json.object([
            #(
              "nested",
              json.object([#("key", json.string(value.meta_nested_key))]),
            ),
          ]),
        ),
      ])
  }
}
```

Further documentation can be found at <https://hexdocs.pm/deriv>.

## Development

```sh
make watch-tests # NOTE: requires `entr`
```
