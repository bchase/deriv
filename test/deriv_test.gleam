import simplifile
import gleam/dict
import gleam/option.{type Option, Some, None}
import gleam/result
import gleeunit
import gleeunit/should
import gleam/string
import deriv/types.{File, DerivFieldOpt, DerivField}
import deriv/parser
import deriv
import deriv/common
import gleam/io
import glance

pub fn glance_print(file_path: String) -> Nil {
  let assert Ok(src) = simplifile.read(file_path)
  let assert Ok(module) = glance.module(src)

  module
  |> string.inspect
  |> io.println

  Nil
}

fn should_derive(
  example_dir_name example_dir_name: String,
) {
  let Example(before: input, after: output) = example_dir_path(example_dir_name:)

  run_and_expect_equal(input:, output:)
}

fn run_and_expect_equal(
  input input: String,
  output output: String,
) {
  let output = output |> string.trim

  let assert [write] = gen_and_build_writes(input |> string.trim)
  let gen = write.src |> string.trim

  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(gen)
  io.println("DIFF (<expected >generated)")
  io.println(common.diff(output, gen))

  write.src
  |> string.trim
  |> should.equal(output)
}

fn gen_and_build_writes(
  input: String,
) -> List(types.Write) {
  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  files
  |> deriv.gen_derivs(build_module_reader([]))
  |> deriv.build_writes
}

fn build_module_reader(
  files: List(#(String, String)),
) -> types.ModuleReader {
  fn(ident) {
    case dict.get(dict.from_list(files), ident) {
      Ok(src) ->
        src
        |> string.trim
        |> glance.module
        |> result.map_error(types.GlanceErr)

      _ ->
        panic as { "`build_module_reader` miss for ident: " <> ident }
    }
  }
}

type Example {
  Example(
    before: String,
    after: String,
  )
}

fn example_dir_path(
  example_dir_name example_dir_name: String,
) -> Example {
  let example_dir_path = "test/examples/" <> example_dir_name <> "/"

  let before_file_path = example_dir_path <> "before.gleam"
  let after_file_path = example_dir_path <> "after.gleam"

  let assert Ok(before) = simplifile.read(before_file_path)
  let assert Ok(after) = simplifile.read(after_file_path)

  Example(before:, after:)
}

// // TODO FIX -- doesn't `import gleam/string`
// pub fn dict_fields_json_test() {
//   should_derive(example_dir_name: "json_dict_string_keys")
// }
// pub fn json_dict_non_string_keys_test() {
//   should_derive(example_dir_name: "json_dict_non_string_keys")
// }
// pub fn json_decoder_local_type_alias_test() {
//   should_derive(example_dir_name: "json_decoder_local_type_alias")
// }

pub fn main() {
  gleeunit.main()
}

// TEST DERIVE ZERO

pub fn zero_test() {
  should_derive(example_dir_name: "zero")
}

// TEST DERIV JSON

pub fn json_test() {
  should_derive(example_dir_name: "json")
}

pub fn json_multi_variant_type_test() {
  should_derive(example_dir_name: "json_multi_variant_type")
}

pub fn json_multi_variant_nullary_type_test() {
  should_derive(example_dir_name: "json_multi_variant_nullary_type")
}

pub fn json_optional_field_test() {
  should_derive(example_dir_name: "json_optional_field")
}

pub fn json_nested_type_test() {
  should_derive(example_dir_name: "json_nested_type")
}

pub fn birl_json_test() {
  should_derive(example_dir_name: "json_birl")
}

pub fn unnested_json_test() {
  should_derive(example_dir_name: "json_named_to_deeply_nested")
}

pub fn json_specify_decoder_and_encode_test() {
  should_derive(example_dir_name: "json_specify_decoder_and_encode")
}

pub fn json_decoder_top_level_override_test() {
  should_derive(example_dir_name: "json_specify_top_level_decoder")
}

pub fn json_specify_default_empty_and_skip_test() {
  should_derive(example_dir_name: "json_specify_default_empty_and_skip")
}

pub fn json_decoder_parameterized_type_test() {
  should_derive(example_dir_name: "json_decoder_parameterized_type")
}

pub fn json_encode_parameterized_type_test() {
  should_derive(example_dir_name: "json_encode_parameterized_type")
}

// TODO `encode` does inner, `decoder` does outer ... make consistent, allow specify
pub fn json_encode_nested_parameterized_type_alias_test() {
  should_derive(example_dir_name: "json_encode_nested_parameterized_type_alias")
}

pub fn json_encode_nested_parameterized_type_alias_list_test() {
  should_derive(example_dir_name: "json_encode_nested_parameterized_type_alias_list")
}

// DERIVE TEST UNIFY & INTO

pub fn into_test() {
  let foo_src = "
pub type Foo {
  Foo(
    name: String,
    count: Int,
  )
}
  " |> string.trim

  let parse = fn(src) {
    glance.module(src)
    |> result.map_error(types.GlanceErr)
  }

  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      "project/asdf/foo" -> parse(foo_src)

      _ -> {
        common.debug(ident)
        panic as "`module_reader` miss in `into_test`"
      }
    }
  }

  let input = "
pub type Bar {
  //$ derive into project/asdf/foo.Foo as f
  Bar(
    title: String,
    //$ into field project/asdf/foo.Foo name
    count: Int,
  )
}
  " |> string.trim

 let output = "
pub type Bar {
  //$ derive into project/asdf/foo.Foo as f
  Bar(
    title: String,
    //$ into field project/asdf/foo.Foo name
    count: Int,
  )
}

pub fn into_foo(value: Bar) -> f.Foo {
  f.Foo(name: value.title, count: value.count)
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

  let writes =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  let assert [write] =
    writes

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

pub fn unify_authe_test() {
  let authe_a_src = "
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
  " |> string.trim

  let authe_b_src = "
pub type AutheB {
  AutheB(
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
  " |> string.trim

  let parse = fn(src) {
    glance.module(src)
    |> result.map_error(types.GlanceErr)
  }

  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      "project/authe/a" -> parse(authe_a_src)
      "project/authe/b" -> parse(authe_b_src)

      _ -> {
        common.debug(ident)
        panic as "`module_reader` miss in `unify_authe_test`"
      }
    }
  }

  let input = "
pub type AutheTokens {
  //$ derive unify project/authe/a.AutheA
  //$ derive unify project/authe/b.AutheB
  Authe(
    id: Uuid,
    //$ unify field project/authe/a.AutheA authe_id
    //$ unify field project/authe/b.AutheB authe_id
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}
  " |> string.trim

 let output = "
pub type AutheTokens {
  //$ derive unify project/authe/a.AutheA
  //$ derive unify project/authe/b.AutheB
  Authe(
    id: Uuid,
    //$ unify field project/authe/a.AutheA authe_id
    //$ unify field project/authe/b.AutheB authe_id
    encrypted_access_token: String,
    encrypted_refresh_token: String,
  )
}

pub fn authe_a(value: AutheA) -> AutheTokens {
  Authe(
    id: value.authe_id,
    encrypted_access_token: value.encrypted_access_token,
    encrypted_refresh_token: value.encrypted_refresh_token,
  )
}

pub fn authe_b(value: AutheB) -> AutheTokens {
  Authe(
    id: value.authe_id,
    encrypted_access_token: value.encrypted_access_token,
    encrypted_refresh_token: value.encrypted_refresh_token,
  )
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

  let writes =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  let assert [write] =
    writes

  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("DIFF (<expected >generated)")
  io.println(common.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn unify_friend_test() {
  let person_src = "
pub type Person {
  Person(
    id: Int,
    first_name: String,
    last_name: String,
    age: Int
  )
}
  " |> string.trim

  let pet_src = "
pub type Pet {
  Pet(
    id: Int,
    name: String,
  )
}
  " |> string.trim

  let parse = fn(src) {
    glance.module(src)
    |> result.map_error(types.GlanceErr)
  }

  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      "project/types/person" -> parse(person_src)
      "project/types/pet" -> parse(pet_src)

      _ -> {
        common.debug(ident)
        panic as "`module_reader` miss in `unify_friend_test`"
      }
    }
  }

  let input = "
import project/types/person.{type Person}
import project/types/pet.{type Pet}

pub type Friend {
  //$ derive unify project/types/person.Person
  //$ derive unify project/types/pet.Pet
  Friend(
    name: String,
    //$ unify field project/types/person.Person first_name
  )
}
  " |> string.trim

 let output = "
import project/types/person.{type Person}
import project/types/pet.{type Pet}

pub type Friend {
  //$ derive unify project/types/person.Person
  //$ derive unify project/types/pet.Pet
  Friend(
    name: String,
    //$ unify field project/types/person.Person first_name
  )
}

pub fn person(value: Person) -> Friend {
  Friend(name: value.first_name)
}

pub fn pet(value: Pet) -> Friend {
  Friend(name: value.name)
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

  let writes =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  let assert [write] =
    writes

  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("DIFF (<expected >generated)")
  io.println(common.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

// TEST GENERAL

pub fn multiple_run_and_filepath_test() {
  let Example(before: input, after: output) = example_dir_path(example_dir_name: "json")

  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(dummy_module_reader)
    |> deriv.build_writes

  let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

  let writes =
    files
    |> deriv.gen_derivs(dummy_module_reader)
    |> deriv.build_writes

  let assert [write] =
    writes

  let input = write.src

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  run_and_expect_equal(input:, output:)
}

fn dummy_module_reader(_) {
  panic as "`dummy_module_reader`"
}

// TEST HELPERS

pub fn snake_case_test() {
  common.snake_case("FooBar")
  |> should.equal("foo_bar")

  common.snake_case("Foo")
  |> should.equal("foo")

  common.snake_case("FooBarX")
  |> should.equal("foo_bar_x")

  common.snake_case("Foo123Bar")
  |> should.equal("foo123_bar")

  common.snake_case("FooBar1")
  |> should.equal("foo_bar1")

  common.snake_case("FooBar12")
  |> should.equal("foo_bar12")

  common.snake_case("FooBar123")
  |> should.equal("foo_bar123")
}

pub fn gleam_format_magic_comment_parsing_test() {
  let src = "
pub type T {
  //$ derive json decode
  A(
    foo: String,
    //$ json foo bar
    //$ json baz boo
  )
}
  "
  |> string.trim

  let assert Ok(glance.Module(custom_types: [type_], ..)) = glance.module(src)

  let assert Ok(#(_type, [deriv], field_opts)) = parser.parse_type_with_derivations(type_.definition, src)

  deriv.name
  |> should.equal("json")
  deriv.opts
  |> should.equal(["decode"])

  let expected =
    [
      #(DerivField(type_: "T", variant: "A", field: "foo"), [
        DerivFieldOpt(strs: ["json", "foo", "bar"]),
        DerivFieldOpt(strs: ["json", "baz", "boo"]),
      ]),
    ]
    |> dict.from_list

  field_opts
  |> should.equal(expected)
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

  common.replace_function(src, func_name: "foo", func_src: new)
  |> should.equal(expected)
}

pub fn replace_type_test() {
  let src = string.trim("
import gleam/string

type Bar {
  Baz(
    boo: String,
  )
}

fn foo(str: String) -> String {
  str
}")

  let new = string.trim("
type Foo {
  Hoge(
    asdf: Int,
  )
}
")

  let expected = string.trim("
import gleam/string

type Foo {
  Hoge(
    asdf: Int,
  )
}

fn foo(str: String) -> String {
  str
}")

  common.replace_type(src, type_name: "Bar", type_src: new)
  |> string.trim
  |> should.equal(expected)
}

// // test broken by glance 5.0.0 `Span` addition...
// pub fn consolidate_imports_test() {
//   let src = string.trim("
// import foo/bar.{type Orig, Orig, orig, type Foo}
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

//   let expected_src = string.trim("
// import baz
// import deriv/foo
// import deriv/util
// import foo/bar.{type ABC as DEF, type Foo, type Orig, Bar, Boo as BOO, Orig, bar as xxx, foo, orig} as foobar

// fn foo(str: String) -> String {
//   str
// }

// type Bar {
//   Baz(
//     boo: String,
//   )
// }")

//   let assert Ok(module) = glance.module(src)

//   let curr_imports = [
//     Import(common.dummy_location(), "deriv/common", None, [], []),
//     Import(common.dummy_location(), "baz", None, [], []),
//     Import(
//       location: common.dummy_location(),
//       module: "foo/bar",
//       alias: None,
//       unqualified_types: [
//         UnqualifiedImport("Orig", None),
//         UnqualifiedImport("Foo", None),
//       ],
//       unqualified_values: [
//         UnqualifiedImport("Orig", None),
//         UnqualifiedImport("orig", None),
//       ],
//     ),
//   ]

//   curr_imports
//   |> should.equal(module.imports |> list.map(fn(d) { d.definition }))

//   let add_imports = [
//     Import(
//       location: common.dummy_location(),
//       module: "deriv/foo",
//       alias: None,
//       unqualified_types: [],
//       unqualified_values: [],
//     ),
//     Import(
//       location: common.dummy_location(),
//       module: "foo/bar",
//       alias: Some(Named("foobar")),
//       unqualified_types: [
//         UnqualifiedImport(name: "Foo", alias: None),
//         UnqualifiedImport(name: "ABC", alias: Some("DEF")),
//       ],
//       unqualified_values: [
//         UnqualifiedImport(name: "Bar", alias: None),
//         UnqualifiedImport(name: "Boo", alias: Some("BOO")),
//         UnqualifiedImport(name: "foo", alias: None),
//         UnqualifiedImport(name: "bar", alias: Some("xxx")),
//       ],
//     ),
//   ]

//   let expected_new_imports = [
//     Import(common.dummy_location(), "baz", None, [], []),
//     Import(
//       location: common.dummy_location(),
//       module: "deriv/foo",
//       alias: None,
//       unqualified_types: [],
//       unqualified_values: [],
//     ),
//     Import(common.dummy_location(), "deriv/common", None, [], []),
//     Import(
//       location: common.dummy_location(),
//       module: "foo/bar",
//       alias: Some(Named("foobar")),
//       unqualified_types: [
//         UnqualifiedImport(name: "Foo", alias: None),
//         UnqualifiedImport(name: "ABC", alias: Some("DEF")),
//         UnqualifiedImport(name: "Orig", alias: None),
//       ],
//       unqualified_values: [
//         UnqualifiedImport(name: "Bar", alias: None),
//         UnqualifiedImport(name: "Boo", alias: Some("BOO")),
//         UnqualifiedImport(name: "foo", alias: None),
//         UnqualifiedImport(name: "bar", alias: Some("xxx")),
//         UnqualifiedImport(name: "Orig", alias: None),
//         UnqualifiedImport(name: "orig", alias: None),
//       ],
//     ),
//   ]

//   deriv.consolidate_imports(list.flatten([curr_imports, add_imports]))
//   |> should.equal(expected_new_imports)

//   io.println("")
//   io.println("")
//   io.println("///// EXPECTED /////")
//   io.println(expected_src)
//   io.println("")
//   io.println("")
//   io.println("///// DERIV /////")
//   io.println(deriv.consolidate_imports_for(src, add: add_imports))

//   deriv.consolidate_imports_for(src, add: add_imports)
//   |> should.equal(expected_src)
// }
