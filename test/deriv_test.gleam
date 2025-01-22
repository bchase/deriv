import gleam/option.{type Option, Some, None}
import gleeunit
import gleeunit/should
import gleam/string
import simplifile
import deriv/types.{type File, File}
import deriv
import deriv/util
import gleam/io

// import glance
// import gleam/list

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
  |> should.equal("src/deriv/example/foo.gleam")

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
  |> should.equal("src/deriv/example/mvar.gleam")

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
  |> should.equal("src/deriv/example/maybe.gleam")

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

  util.replace_function(src, "foo", new)
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

  let assert Ok(_) = util.rewrite_function_to_file(filepath, "foo", new_foo_src)

  let assert Ok(new_src) = simplifile.read(filepath)
  let _ = simplifile.delete(filepath)

  new_src
  |> should.equal(expected)
}

// pub fn consolidate_imports_test() {
//   let src = string.trim("
// import foo/bar
// import baz
// import deriv/util

// fn foo(str: String) -> String {
//   str
// }

// type Bar {
//   Baz(
//     boo: String,
//   )
// }")

//   let expected = string.trim("
// import baz
// import foo/bar.{type Foo, Bar, foo} as foobar
// import deriv/util
// import deriv/foo

// fn other(changed: Int) -> Bool {
//   True
// }

// type Bar {
//   Baz(
//     boo: String,
//   )
// }")

//   let imports = [
//     Import(
//       module: "deriv/foo",
//       types: [],
//       funcs: [],
//       alias: None,
//     ),
//     Import(
//       module: "foo/bar",
//       types: [
//         ImportV(name: "Foo", alias: None),
//         ImportV(name: "ABC", alias: Some("DEF")),
//       ],
//       funcs: [
//         ImportV(name: "Bar", alias: None),
//         ImportV(name: "Boo", alias: Some("BOO")),
//         ImportV(name: "foo", alias: None),
//         ImportV(name: "bar", alias: Some("xxx")),
//       ],
//       alias: Some("foobar"),
//     ),
//   ]

//   consolidate_imports_for(src, imports)
//   |> should.equal(expected)
// }

// fn consolidate_imports_for(src: String, imports: List(Import)) -> String {
//   let assert Ok(module) = glance.module(src)

//   module.imports
//   |> list.map(to_deriv_imports)
//   |> consolidate_imports(imports)

//   todo
// }

// fn consolidate_imports(i1: List(Import), i2: List(Import)) -> List(Import) {
//   todo
// }

// fn to_deriv_imports(i: glance.Definition(glance.Import)) -> Import {
//   // glance.Import(
//   //   module: "",
//   //   alias: None,
//   //   unqualified_types: [],
//   //   unqualified_values: [],
//   // )

//   let types =
//     i.definition.unqualified_types
//     |> list.map(fn(t) {
//       ImportV(name: t.name, alias: t.alias)
//     })

//   let funcs =
//     i.definition.unqualified_values
//     |> list.map(fn(t) {
//       ImportV(name: t.name, alias: t.alias)
//     })

//   let alias =
//     case i.definition.alias {
//       Some(glance.Named(str)) -> Some(str)
//       Some(glance.Discarded(str)) -> Some("_" <> str)
//       None -> None
//     }

//   Import(
//     module: i.definition.module,
//     types: types,
//     funcs: funcs,
//     alias: alias,
//   )
// }

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

pub fn stop_warning() { io.debug("stop") }

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }


// TODO
//   - replace for inline and change for current tests
//     * consolidate imports
//       * type/func aliases
//     * if func exists replace
//     * if func DNE append
//   - write import test? (separate file writes)

// if funcs exist
//   replace
// else
//   append
