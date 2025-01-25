import gleam/option.{type Option, Some, None}
import gleam/function
import gleeunit
import gleeunit/should
import gleam/string
import simplifile
import deriv/types.{type File, File}
import deriv
import deriv/util
import gleam/io

import glance.{Import, UnqualifiedImport, Named}
import gleam/list

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
import decode.{type Decoder}
import deriv/util
import gleam/json.{type Json}
import gleam/list
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

pub fn decoder_foo() -> Decoder(Foo) {
  decode.into({
    use uuid <- decode.parameter
    use id <- decode.parameter
    use name <- decode.parameter
    use active <- decode.parameter
    use ratio <- decode.parameter
    use words <- decode.parameter

    Foo(uuid:, id:, name:, active:, ratio:, words:)
  })
  |> decode.field(\"uuid\", util.decoder_uuid())
  |> decode.field(\"int_id\", decode.int)
  |> decode.field(\"name\", decode.string)
  |> decode.field(\"active\", decode.bool)
  |> decode.field(\"ratio\", decode.float)
  |> decode.field(\"words\", decode.list(decode.string))
}

pub fn encode_foo(value: Foo) -> Json {
  json.object([
    #(\"uuid\", util.encode_uuid(value.uuid)),
    #(\"int_id\", json.int(value.id)),
    #(\"name\", json.string(value.name)),
    #(\"active\", json.bool(value.active)),
    #(\"ratio\", json.float(value.ratio)),
    #(\"words\", json.preprocessed_array(list.map(value.words, json.string))),
  ])
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.into({
    use baz <- decode.parameter

    Bar(baz:)
  })
  |> decode.field(\"baz\", decode.bool)
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  // io.println(output)
  // io.println(write.src)

  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

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
import decode.{type Decoder}

pub type T {
  Var1(
    var1: String,
  )
  Var2(
    var2: Int,
  )
} //$ derive json(decode)

pub fn decoder_t() -> Decoder(T) {
  decode.one_of([
    decoder_t_var1(),
    decoder_t_var2(),
  ])
}

fn decoder_t_var1() -> Decoder(T) {
  decode.into({
    use var1 <- decode.parameter

    Var1(var1:)
  })
  |> decode.field(\"var1\", decode.string)
}

fn decoder_t_var2() -> Decoder(T) {
  decode.into({
    use var2 <- decode.parameter

    Var2(var2:)
  })
  |> decode.field(\"var2\", decode.int)
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/mvar", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/example/mvar.gleam")

  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println("EXPECTED")
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
import decode.{type Decoder}
import gleam/json.{type Json}

pub type Maybe {
  Maybe(
    name: Option(String),
  )
} //$ derive json(decode,encode)

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.into({
    use name <- decode.parameter

    Maybe(name:)
  })
  |> decode.field(\"name\", decode.optional(decode.string))
}

pub fn encode_maybe(value: Maybe) -> Json {
  json.object([
    #(\"name\", json.nullable(value.name, json.string)),
  ])
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/maybe", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/example/maybe.gleam")

  // io.println(output)
  // io.println(write.src)

  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)

  write.src
  |> should.equal(output)
}

pub fn nested_type_test() {
  let input = "
pub type A {
  A(
    b: B,
  )
} //$ derive json(decode,encode)

pub type B {
  B(
    x: String,
  )
} //$ derive json(decode,encode)
  "
  |> string.trim

 let output = "
import decode.{type Decoder}
import gleam/json.{type Json}

pub type A {
  A(
    b: B,
  )
} //$ derive json(decode,encode)

pub type B {
  B(
    x: String,
  )
} //$ derive json(decode,encode)

pub fn decoder_a() -> Decoder(A) {
  decode.into({
    use b <- decode.parameter

    A(b:)
  })
  |> decode.field(\"b\", decoder_b())
}

pub fn encode_a(value: A) -> Json {
  json.object([
    #(\"b\", encode_b(value.b)),
  ])
}

pub fn decoder_b() -> Decoder(B) {
  decode.into({
    use x <- decode.parameter

    B(x:)
  })
  |> decode.field(\"x\", decode.string)
}

pub fn encode_b(value: B) -> Json {
  json.object([
    #(\"x\", json.string(value.x)),
  ])
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/nested", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/example/nested.gleam")

  // io.println(output)
  // io.println(write.src)

  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println("EXPECTED")
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

  let assert Ok(module) = glance.module(src)

  util.replace_function(module, src, func_name: "foo", func_src: new)
  |> should.equal(expected)
}

pub fn consolidate_imports_test() {
  let src = string.trim("
import foo/bar.{type Orig, Orig, orig, type Foo}
import baz
import deriv/util

fn foo(str: String) -> String {
  str
}

type Bar {
  Baz(
    boo: String,
  )
}")

  let expected_src = string.trim("
import baz
import deriv/foo
import deriv/util
import foo/bar.{type ABC as DEF, type Foo, type Orig, Bar, Boo as BOO, Orig, bar as xxx, foo, orig} as foobar

fn foo(str: String) -> String {
  str
}

type Bar {
  Baz(
    boo: String,
  )
}")

  let assert Ok(module) = glance.module(src)

  let curr_imports = [
    Import("deriv/util", None, [], []),
    Import("baz", None, [], []),
    Import(
      module: "foo/bar",
      alias: None,
      unqualified_types: [
        UnqualifiedImport("Orig", None),
        UnqualifiedImport("Foo", None),
      ],
      unqualified_values: [
        UnqualifiedImport("Orig", None),
        UnqualifiedImport("orig", None),
      ],
    ),
  ]

  curr_imports
  |> should.equal(module.imports |> list.map(fn(d) { d.definition }))

  let add_imports = [
    Import(
      module: "deriv/foo",
      alias: None,
      unqualified_types: [],
      unqualified_values: [],
    ),
    Import(
      module: "foo/bar",
      alias: Some(Named("foobar")),
      unqualified_types: [
        UnqualifiedImport(name: "Foo", alias: None),
        UnqualifiedImport(name: "ABC", alias: Some("DEF")),
      ],
      unqualified_values: [
        UnqualifiedImport(name: "Bar", alias: None),
        UnqualifiedImport(name: "Boo", alias: Some("BOO")),
        UnqualifiedImport(name: "foo", alias: None),
        UnqualifiedImport(name: "bar", alias: Some("xxx")),
      ],
    ),
  ]

  let expected_new_imports = [
    Import("baz", None, [], []),
    Import(
      module: "deriv/foo",
      alias: None,
      unqualified_types: [],
      unqualified_values: [],
    ),
    Import("deriv/util", None, [], []),
    Import(
      module: "foo/bar",
      alias: Some(Named("foobar")),
      unqualified_types: [
        UnqualifiedImport(name: "Foo", alias: None),
        UnqualifiedImport(name: "ABC", alias: Some("DEF")),
        UnqualifiedImport(name: "Orig", alias: None),
      ],
      unqualified_values: [
        UnqualifiedImport(name: "Bar", alias: None),
        UnqualifiedImport(name: "Boo", alias: Some("BOO")),
        UnqualifiedImport(name: "foo", alias: None),
        UnqualifiedImport(name: "bar", alias: Some("xxx")),
        UnqualifiedImport(name: "Orig", alias: None),
        UnqualifiedImport(name: "orig", alias: None),
      ],
    ),
  ]

  deriv.consolidate_imports(list.flatten([curr_imports, add_imports]))
  |> should.equal(expected_new_imports)

  io.println("")
  io.println("")
  io.println("///// EXPECTED /////")
  io.println(expected_src)
  io.println("")
  io.println("")
  io.println("///// DERIV /////")
  io.println(deriv.consolidate_imports_for(src, add: add_imports))

  deriv.consolidate_imports_for(src, add: add_imports)
  |> should.equal(expected_src)
}

pub fn stop_warning() { io.debug("stop") }

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

// pub fn rewrite_function_to_file_when_function_does_not_exist_test() {
//   let dir = "/tmp/gleam/deriv"
//   let filepath = dir <> "/rewrite_function_to_file_test.gleam"

//   let old_src = string.trim("
// import gleam/string

// type Bar {
//   Baz(
//     boo: String,
//   )
// }")

//   let new_foo_src = string.trim("
// fn other(changed: Int) -> Bool {
//   True
// }")

//   let expected = string.trim("
// import gleam/string

// type Bar {
//   Baz(
//     boo: String,
//   )
// }

// fn other(changed: Int) -> Bool {
//   True
// }")


//   let assert Ok(_) = simplifile.create_directory_all("/tmp/gleam/deriv")
//   let _ = simplifile.delete(filepath)
//   let assert Ok(_) = simplifile.write(filepath, old_src)

//   let assert Ok(_) = util.rewrite_functions_to_file(filepath, [#("foo", new_foo_src)])

//   let assert Ok(new_src) = simplifile.read(filepath)
//   let _ = simplifile.delete(filepath)

//   new_src
//   |> should.equal(expected)
// }

// pub fn rewrite_function_to_file_when_function_exists_test() {
//   let dir = "/tmp/gleam/deriv"
//   let filepath = dir <> "/rewrite_function_to_file_test.gleam"

//   let old_src = string.trim("
// import gleam/string

// fn foo(str: String) -> String {
//   str
// }

// type Bar {
//   Baz(
//     boo: String,
//   )
// }")

//   let new_foo_src = string.trim("
// fn other(changed: Int) -> Bool {
//   True
// }")

//   let expected = string.trim("
// import gleam/string

// fn other(changed: Int) -> Bool {
//   True
// }

// type Bar {
//   Baz(
//     boo: String,
//   )
// }")


//   let assert Ok(_) = simplifile.create_directory_all("/tmp/gleam/deriv")
//   let _ = simplifile.delete(filepath)
//   let assert Ok(_) = simplifile.write(filepath, old_src)

//   let assert Ok(_) = util.rewrite_functions_to_file(filepath, [#("foo", new_foo_src)])

//   let assert Ok(new_src) = simplifile.read(filepath)
//   let _ = simplifile.delete(filepath)

//   new_src
//   |> should.equal(expected)
// }
