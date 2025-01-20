import gleeunit
import gleeunit/should
import gleam/string
import deriv/types.{type File, File, type Import, Import}
import deriv
import gleam/io
import glance

import gleam/list
import gleam/result

import simplifile

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
    ratio: Float,
    words: List(String),
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
import gleam/list

pub fn decoder_foo() -> Decoder(m1.Foo) {
  decode.into({
    use uuid <- decode.parameter
    use id <- decode.parameter
    use name <- decode.parameter
    use active <- decode.parameter
    use ratio <- decode.parameter
    use words <- decode.parameter

    m1.Foo(uuid:, id:, name:, active:, ratio:, words:)
  })
  |> decode.field(\"uuid\", util.decoder_uuid())
  |> decode.field(\"int_id\", decode.int)
  |> decode.field(\"name\", decode.string)
  |> decode.field(\"active\", decode.bool)
  |> decode.field(\"ratio\", decode.float)
  |> decode.field(\"words\", decode.list(decode.string))
}

pub fn encode_foo(value: m1.Foo) -> Json {
  json.object([
    #(\"uuid\", util.encode_uuid(value.uuid)),
    #(\"int_id\", json.int(value.id)),
    #(\"name\", json.string(value.name)),
    #(\"active\", json.bool(value.active)),
    #(\"ratio\", json.float(value.ratio)),
    #(\"words\", json.preprocessed_array(list.map(value.words, json.string))),
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

pub fn json_optional_field_test() {
  let input = "
pub type Maybe {
  Maybe(
    name: Option(String),
  )
} //$ derive json(decode,encode)
  "
  |> string.trim

 let output = "
import deriv/example/maybe as m1
import decode.{type Decoder}
import gleam/json.{type Json}

pub fn decoder_maybe() -> Decoder(m1.Maybe) {
  decode.into({
    use name <- decode.parameter

    m1.Maybe(name:)
  })
  |> decode.field(\"name\", decode.optional(decode.string))
}

pub fn encode_maybe(value: m1.Maybe) -> Json {
  json.object([
    #(\"name\", json.nullable(value.name, json.string)),
  ])
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/maybe", src: input, idx: 1) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/deriv/example/maybe/json.gleam")

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

pub fn replace_function_test() {
  let src = string.trim("
import gleam/string

fn foo(str: String) -> String {
  str
}

type Bar {
  Baz(
    boo: String,
  )
}")

  let new = string.trim("
fn other(changed: Int) -> Bool {
  True
}")

  let expected = string.trim("
import gleam/string

fn other(changed: Int) -> Bool {
  True
}

type Bar {
  Baz(
    boo: String,
  )
}")

  replace_function(src, "foo", new)
  |> should.equal(expected)
}

pub fn rewrite_function_to_file_test() {
  let dir = "/tmp/gleam/deriv"
  let filepath = dir <> "/rewrite_function_to_file_test.gleam"

  let old_src = string.trim("
import gleam/string

fn foo(str: String) -> String {
  str
}

type Bar {
  Baz(
    boo: String,
  )
}")

  let new_foo_src = string.trim("
fn other(changed: Int) -> Bool {
  True
}")

  let expected = string.trim("
import gleam/string

fn other(changed: Int) -> Bool {
  True
}

type Bar {
  Baz(
    boo: String,
  )
}")


  let assert Ok(_) = simplifile.create_directory_all("/tmp/gleam/deriv")
  let _ = simplifile.delete(filepath)
  let assert Ok(_) = simplifile.write(filepath, old_src)

  let assert Ok(_) = rewrite_function_to_file(filepath, "foo", new_foo_src)

  let assert Ok(new_src) = simplifile.read(filepath)
  let _ = simplifile.delete(filepath)

  new_src
  |> should.equal(expected)
}

// fn add_import_test() {
//   let old_src = string.trim("")
//   let new_src = string.trim("")
//   let expected = string.trim("")

//   expected
//   |> should.equal(new_src)
// }

// fn prepend_import_to_file_test() {
// }

// fn add_import(full_src: String, import_: Import) -> String {
// }

// fn add_import(full_src: String, import_: Import) -> String {
//   let assert Ok(module) = glance.module(full_src)

//   // let assert Ok(span) =
//   //   module.functions
//   //   |> list.find_map(fn(f) {
//   //     case f.definition.name == func_name {
//   //       False -> Error(Nil)
//   //       True -> Ok(f.definition.location)
//   //     }
//   //   })

//   // let eof = string.byte_size(full_src) - 1

//   // let before = string.slice(full_src, 0, span.start - 1)
//   // let after = string.slice(full_src, span.end + 1, eof)

//   // [
//   //   before,
//   //   func_src,
//   //   after,
//   // ]
//   // |> string.join("\n")
//   todo
// }

fn replace_function(full_src: String, func_name: String, func_src: String) -> String {
  let assert Ok(module) = glance.module(full_src)

  io.debug(module)

  let assert Ok(span) =
    module.functions
    |> list.find_map(fn(f) {
      case f.definition.name == func_name {
        False -> Error(Nil)
        True -> Ok(f.definition.location)
      }
    })

  let eof = string.byte_size(full_src) - 1

  let before = string.slice(full_src, 0, span.start - 1)
  let after = string.slice(full_src, span.end + 1, eof)

  [
    before,
    func_src,
    after,
  ]
  |> string.join("\n")
}

fn rewrite_function_to_file(filepath: String, func_name: String, func_src: String) -> Result(Nil, Nil) {
  use old_src <- then(simplifile.read(filepath))

  let new_src = replace_function(old_src, func_name, func_src)

  simplifile.write(filepath, new_src)
}

pub fn then(
  result: Result(a, err1),
  fun: fn(a) -> Result(b, err2),
) -> Result(b, Nil) {
  result
  |> result.map_error(log_and_discard_error)
  |> result.then(fn(x) {
    fun(x)
    |> result.map_error(log_and_discard_error)
  })
}
fn log_and_discard_error(err: err) -> Nil {
  io.debug(err)
  Nil
}

pub fn stop_warning() { io.debug("stop") }
