import gleeunit
import gleeunit/should
import gleam/string
import deriv/types.{type File, File}
import deriv

pub fn main() {
  gleeunit.main()
}

pub fn json_test() {
  let input = "
import youid/uuid.{type Uuid}

pub type Foo {
  Foo(
    uuid: Uuid,
    id: Int,
    name: String, //$ json(encode(foo(bar)))
    active: Bool,
  )
} //$ derive json(decode,encode)

pub type Bar {
  Bar(
    baz: Bool,
  )
} //$ derive json(decode)
"
|> string.trim

let output = "
import deriv/example/foo as m1
import decode/zero.{type Decoder} as decode
import deriv/util
import gleam/json.{type Json}

pub fn decoder_foo() -> Decoder(m1.Foo) {
  use uuid <- decode.field(\"uuid\", util.decoder_uuid())
  use id <- decode.field(\"id\", decode.int)
  use name <- decode.field(\"name\", decode.string)
  use active <- decode.field(\"active\", decode.bool)

  decode.success(m1.Foo(uuid:, id:, name:, active:))
}

pub fn encode_foo(value: m1.Foo) -> Json {
  json.object([
    #(\"uuid\", util.encode_uuid(value.uuid)),
    #(\"id\", json.int(value.id)),
    #(\"name\", json.string(value.name)),
    #(\"active\", json.bool(value.active)),
  ])
}

pub fn decoder_bar() -> Decoder(m1.Bar) {
  use baz <- decode.field(\"baz\", decode.bool)

  decode.success(m1.Bar(baz:))
}
"
|> string.trim

  let files = [ File(module: "deriv/example/foo", src: input, idx: 1) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/deriv/example/foo/json.gleam")

  write.src
  |> should.equal(output)
}
