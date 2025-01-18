import gleeunit
import gleeunit/should
import gleam/string
import deriv/types.{type File, File}
import deriv
import gleam/io

pub fn main() {
  gleeunit.main()
}

pub fn json_test() {
  let input = "
import youid/uuid.{type Uuid}

pub type Foo {
  Foo(
    uuid: Uuid,
    id: Int, //$ json(named(int_id))
    name: String,
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
import decode.{type Decoder}
import deriv/util
import gleam/json.{type Json}

pub fn decoder_foo() -> Decoder(m1.Foo) {
  decode.into({
    use uuid <- decode.parameter
    use id <- decode.parameter
    use name <- decode.parameter
    use active <- decode.parameter

    m1.Foo(uuid:, id:, name:, active:)
  })
  |> decode.field(\"uuid\", util.decoder_uuid())
  |> decode.field(\"id\", decode.int)
  |> decode.field(\"name\", decode.string)
  |> decode.field(\"active\", decode.bool)
}

pub fn encode_foo(value: m1.Foo) -> Json {
  json.object([
    #(\"uuid\", util.encode_uuid(value.uuid)),
    #(\"int_id\", json.int(value.id)),
    #(\"name\", json.string(value.name)),
    #(\"active\", json.bool(value.active)),
  ])
}

pub fn decoder_bar() -> Decoder(m1.Bar) {
  decode.into({
    use baz <- decode.parameter

    m1.Bar(baz:)
  })
  |> decode.field(\"baz\", decode.bool)
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

  // io.println(output)
  // io.println(write.src)

  io.println("")
  io.println("")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println(output)

  write.src
  |> should.equal(output)
}

pub fn json_multi_variant_type_test() {
  let input = "
pub type T {
  Var1(
    var1: String,
  )
  Var2(
    var2: Int,
  )
} //$ derive json(decode)
  "
  |> string.trim

 let output = "
import deriv/example/mvar as m1
import decode.{type Decoder}

pub fn decoder_t() -> Decoder(m1.T) {
  decode.one_of([
    decoder_t_var1(),
    decoder_t_var2(),
  ])
}

fn decoder_t_var1() -> Decoder(m1.T) {
  decode.into({
    use var1 <- decode.parameter

    m1.Var1(var1:)
  })
  |> decode.field(\"var1\", decode.string)
}
fn decoder_t_var2() -> Decoder(m1.T) {
  decode.into({
    use var2 <- decode.parameter

    m1.Var2(var2:)
  })
  |> decode.field(\"var2\", decode.int)
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/mvar", src: input, idx: 1) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/deriv/example/mvar/json.gleam")

  io.println("")
  io.println("")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println(output)

  write.src
  |> should.equal(output)
}

pub fn stop_warning() { io.debug("stop") }

