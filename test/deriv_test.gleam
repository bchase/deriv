import deriv
import deriv/internal/common
import deriv/internal/parser
import deriv/internal/types.{File, DerivFieldOpt, DerivField}
import deriv/internal/derivs/from
import glance
import gleam/dict
import gleam/option.{Some}
import gleam/string
import gleeunit
import gleeunit/should
import gleam/io
import simplifile

pub fn glance_read(file_path: String) -> glance.Module {
  let assert Ok(src) = simplifile.read(file_path)
  let assert Ok(module) = glance.module(src)
  module
}

pub fn glance_print(file_path: String) -> Nil {
  file_path
  |> glance_read
  |> string.inspect
  |> io.println
}

pub fn glance_read_and_write(
  from input: String,
  to output: String,
) -> Nil {
  input
  |> glance_read
  |> string.inspect
  |> simplifile.write(to: output, contents: _)
  |> fn(r) { case r {
    Error(err) -> {
      panic as string.inspect(err)
    }

    Ok(_) -> {
      Nil
    }
  } }
}

fn should_derive(
  example_dir_name example_dir_name: String,
) {
  let Example(before: input, after: output) = example_dir_path(example_dir_name:)

  run_and_expect_equal(input:, output:, example_dir_name:)
}

fn run_and_expect_equal(
  input input: String,
  output output: String,
  example_dir_name example_dir_name: String,
) {
  let output = output |> string.trim

  let assert [write] = gen_and_build_writes(input: input |> string.trim, example_dir_name:)
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

  gen
  |> should.equal(output)
}

fn gen_and_build_writes(
  input input: String,
  example_dir_name example_dir_name: String,
) -> List(types.Write) {
  let files = [ File(module: "examples/" <> example_dir_name <> "/before", src: input, idx: Some(1)) ]

  files
  // |> deriv.gen_derivs(build_module_reader([]))
  |> deriv.gen_derivs(common.fetch_module_(path: _, path_prefix: "test/"))
  |> deriv.build_writes
}

// fn build_module_reader(
//   files: List(#(String, String)),
// ) -> types.ModuleReader {
//   fn(ident) {
//     case dict.get(dict.from_list(files), ident) {
//       Ok(src) ->
//         src
//         |> string.trim
//         |> glance.module
//         |> result.map_error(types.GlanceErr)

//       _ ->
//         panic as { "`build_module_reader` miss for ident: " <> ident }
//     }
//   }
// }

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

pub fn main() {
  gleeunit.main()
}


pub fn from_overrides_test() {
    //created_at__type_unspecified: Time,
    ////$ from using birl.from_unix

    //created_at__type_qualified__field__implicit: Time,
    ////$ from examples/from/before.A.created_at__type_qualified__field__implicit using birl.from_unix
    //created_at__type_unqualified__field__implicit: Time,
    ////$ from A.created_at__type_qualified__field__implicit using birl.from_unix

    //created_at__type_qualified__field__explicit: Time, <----------------------------
    ////$ from examples/from/before.A.created_at__type_qualified__field__explicit using birl.from_unix
    //created_at__type_unqualified__field__explicit: Time, <----------------------------
    ////$ from A.created_at__type_qualified__field__explicit using birl.from_unix

    //created_at__type_qualified__type: Time, <----------------------------------
    ////$ from examples/from/before.A using to_time
    //created_at__type_unqualified__type: Time,
    ////$ from A using to_time
  from.build_field_override(
    DerivFieldOpt(strs: [ "from", "using", "birl.from_unix" ]),
    DerivField(type_: "", variant: "", field: "field_name"),
  )
  |> should.be_ok
  |> should.equal(from.FromFieldOverride(
    field: "field_name",
    ident: "",
    module_name: "",
    override: "",
    using: Some(from.FromFieldOverrideConv(
      module: Some("birl"),
      func: "from_unix",
      args: from.ValueDotField,
    )),
  ))

  from.build_field_override(
    DerivFieldOpt(strs: [ "from", "examples/from/before.A", "using", "to_time" ]),
    DerivField(type_: "", variant: "", field: "field_name"),
  )
  |> should.be_ok
  |> should.equal(from.FromFieldOverride(
    field: "field_name",
    ident: "examples/from/before.A",
    module_name: "A",
    override: "",
    using: Some(from.FromFieldOverrideConv(
      module: option.None,
      func: "to_time",
      args: from.EntireValue,
    )),
  ))
}

// pub fn from_test() {
//   should_derive(example_dir_name: "from")
// }

//// TEST DERIVE ENUM

//pub fn enum_test() {
//  should_derive(example_dir_name: "enum")
//}

//// TEST DERIVE FORM (`formal/form`)

//pub fn formal_form_test() {
//  should_derive(example_dir_name: "formal")
//}

//pub fn formal_lustre_form_test() {
//  // glance_read_and_write(from: "test/examples/formal_lustre/after.gleam", to: "expr.gleam")
//  should_derive(example_dir_name: "formal_lustre")
//}

//// TEST DERIVE ZERO

//pub fn zero_test() {
//  should_derive(example_dir_name: "zero")
//}

//// TEST DERIV JSON

//pub fn json_test() {
//  should_derive(example_dir_name: "json")
//}

//pub fn json_multi_variant_type_test() {
//  should_derive(example_dir_name: "json_multi_variant_type")
//}

//pub fn json_multi_variant_nullary_type_test() {
//  should_derive(example_dir_name: "json_multi_variant_nullary_type")
//}

//pub fn json_optional_field_test() {
//  should_derive(example_dir_name: "json_optional_field")
//}

//pub fn json_nested_type_test() {
//  should_derive(example_dir_name: "json_nested_type")
//}

//pub fn birl_json_test() {
//  should_derive(example_dir_name: "json_birl")
//}

//pub fn unnested_json_test() {
//  should_derive(example_dir_name: "json_named_to_deeply_nested")
//}

//pub fn json_dict_string_keys_test() {
//  should_derive(example_dir_name: "json_dict_string_keys")
//}

//pub fn json_dict_non_string_keys_test() {
//  should_derive(example_dir_name: "json_dict_non_string_keys")
//}

//pub fn json_decoder_local_type_alias_test() {
//  should_derive(example_dir_name: "json_decoder_local_type_alias")
//}

//pub fn json_specify_decoder_and_encode_test() {
//  should_derive(example_dir_name: "json_specify_decoder_and_encode")
//}

//pub fn json_decoder_top_level_override_test() {
//  should_derive(example_dir_name: "json_specify_top_level_decoder")
//}

//pub fn json_specify_default_empty_and_skip_test() {
//  should_derive(example_dir_name: "json_specify_default_empty_and_skip")
//}

//pub fn json_decoder_parameterized_type_test() {
//  should_derive(example_dir_name: "json_decoder_parameterized_type")
//}

//pub fn json_encode_parameterized_type_test() {
//  should_derive(example_dir_name: "json_encode_parameterized_type")
//}

//// TODO `encode` does inner, `decoder` does outer ... make consistent, allow specify
//pub fn json_encode_nested_parameterized_type_alias_test() {
//  should_derive(example_dir_name: "json_encode_nested_parameterized_type_alias")
//}

//pub fn json_encode_nested_parameterized_type_alias_list_test() {
//  should_derive(example_dir_name: "json_encode_nested_parameterized_type_alias_list")
//}

//// DERIVE TEST INTO & FROM

//pub fn into_test() {
//  should_derive(example_dir_name: "into")
//}

//pub fn from_test() {
//  should_derive(example_dir_name: "from")
//}

//// TEST GENERAL

//pub fn multiple_run_and_filepath_test() {
//  let Example(before: input, after: output) = example_dir_path(example_dir_name: "json")

//  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

//  let assert [write] =
//    files
//    |> deriv.gen_derivs(dummy_module_reader)
//    |> deriv.build_writes

//  let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

//  let writes =
//    files
//    |> deriv.gen_derivs(dummy_module_reader)
//    |> deriv.build_writes

//  let assert [write] =
//    writes

//  let input = write.src

//  write.filepath
//  |> should.equal("src/deriv/example/foo.gleam")

//  run_and_expect_equal(input:, output:, example_dir_name: "deriv/example/foo")
//}

//fn dummy_module_reader(_) {
//  panic as "`dummy_module_reader`"
//}

//// TEST HELPERS

//pub fn snake_case_test() {
//  common.snake_case("FooBar")
//  |> should.equal("foo_bar")

//  common.snake_case("Foo")
//  |> should.equal("foo")

//  common.snake_case("FooBarX")
//  |> should.equal("foo_bar_x")

//  common.snake_case("Foo123Bar")
//  |> should.equal("foo123_bar")

//  common.snake_case("FooBar1")
//  |> should.equal("foo_bar1")

//  common.snake_case("FooBar12")
//  |> should.equal("foo_bar12")

//  common.snake_case("FooBar123")
//  |> should.equal("foo_bar123")
//}

//pub fn gleam_format_magic_comment_parsing_test() {
//  let src = "
//pub type T {
//  //$ derive json decode
//  A(
//    foo: String,
//    //$ json foo bar
//    //$ json baz boo
//  )
//}
//  "
//  |> string.trim

//  let assert Ok(glance.Module(custom_types: [type_], ..)) = glance.module(src)

//  let assert Ok(#(_type, [deriv], field_opts)) = parser.parse_type_with_derivations(type_.definition, src)

//  deriv.name
//  |> should.equal("json")
//  deriv.opts
//  |> should.equal(["decode"])

//  let expected =
//    [
//      #(DerivField(type_: "T", variant: "A", field: "foo"), [
//        DerivFieldOpt(strs: ["json", "foo", "bar"]),
//        DerivFieldOpt(strs: ["json", "baz", "boo"]),
//      ]),
//    ]
//    |> dict.from_list

//  field_opts
//  |> should.equal(expected)
//}

//pub fn replace_function_test() {
//  let src = string.trim("
//import gleam/string

//fn foo(str: String) -> String {
//  str
//}

//type Bar {
//  Baz(
//    boo: String,
//  )
//}")

//  let new = string.trim("
//fn other(changed: Int) -> Bool {
//  True
//}")

//  let expected = string.trim("
//import gleam/string

//fn other(changed: Int) -> Bool {
//  True
//}

//type Bar {
//  Baz(
//    boo: String,
//  )
//}")

//  common.replace_function(src, func_name: "foo", func_src: new)
//  |> should.equal(expected)
//}

//pub fn replace_type_test() {
//  let src = string.trim("
//import gleam/string

//type Bar {
//  Baz(
//    boo: String,
//  )
//}

//fn foo(str: String) -> String {
//  str
//}")

//  let new = string.trim("
//type Foo {
//  Hoge(
//    asdf: Int,
//  )
//}
//")

//  let expected = string.trim("
//import gleam/string

//type Foo {
//  Hoge(
//    asdf: Int,
//  )
//}

//fn foo(str: String) -> String {
//  str
//}")

//  common.replace_type(src, type_name: "Bar", type_src: new)
//  |> string.trim
//  |> should.equal(expected)
//}

//// // test broken by glance 5.0.0 `Span` addition...
//// pub fn consolidate_imports_test() {
////   let src = string.trim("
//// import foo/bar.{type Orig, Orig, orig, type Foo}
//// import baz
//// import deriv/util

//// fn foo(str: String) -> String {
////   str
//// }

//// type Bar {
////   Baz(
////     boo: String,
////   )
//// }")

////   let expected_src = string.trim("
//// import baz
//// import deriv/foo
//// import deriv/util
//// import foo/bar.{type ABC as DEF, type Foo, type Orig, Bar, Boo as BOO, Orig, bar as xxx, foo, orig} as foobar

//// fn foo(str: String) -> String {
////   str
//// }

//// type Bar {
////   Baz(
////     boo: String,
////   )
//// }")

////   let assert Ok(module) = glance.module(src)

////   let curr_imports = [
////     Import(common.dummy_location(), "deriv/common", None, [], []),
////     Import(common.dummy_location(), "baz", None, [], []),
////     Import(
////       location: common.dummy_location(),
////       module: "foo/bar",
////       alias: None,
////       unqualified_types: [
////         UnqualifiedImport("Orig", None),
////         UnqualifiedImport("Foo", None),
////       ],
////       unqualified_values: [
////         UnqualifiedImport("Orig", None),
////         UnqualifiedImport("orig", None),
////       ],
////     ),
////   ]

////   curr_imports
////   |> should.equal(module.imports |> list.map(fn(d) { d.definition }))

////   let add_imports = [
////     Import(
////       location: common.dummy_location(),
////       module: "deriv/foo",
////       alias: None,
////       unqualified_types: [],
////       unqualified_values: [],
////     ),
////     Import(
////       location: common.dummy_location(),
////       module: "foo/bar",
////       alias: Some(Named("foobar")),
////       unqualified_types: [
////         UnqualifiedImport(name: "Foo", alias: None),
////         UnqualifiedImport(name: "ABC", alias: Some("DEF")),
////       ],
////       unqualified_values: [
////         UnqualifiedImport(name: "Bar", alias: None),
////         UnqualifiedImport(name: "Boo", alias: Some("BOO")),
////         UnqualifiedImport(name: "foo", alias: None),
////         UnqualifiedImport(name: "bar", alias: Some("xxx")),
////       ],
////     ),
////   ]

////   let expected_new_imports = [
////     Import(common.dummy_location(), "baz", None, [], []),
////     Import(
////       location: common.dummy_location(),
////       module: "deriv/foo",
////       alias: None,
////       unqualified_types: [],
////       unqualified_values: [],
////     ),
////     Import(common.dummy_location(), "deriv/common", None, [], []),
////     Import(
////       location: common.dummy_location(),
////       module: "foo/bar",
////       alias: Some(Named("foobar")),
////       unqualified_types: [
////         UnqualifiedImport(name: "Foo", alias: None),
////         UnqualifiedImport(name: "ABC", alias: Some("DEF")),
////         UnqualifiedImport(name: "Orig", alias: None),
////       ],
////       unqualified_values: [
////         UnqualifiedImport(name: "Bar", alias: None),
////         UnqualifiedImport(name: "Boo", alias: Some("BOO")),
////         UnqualifiedImport(name: "foo", alias: None),
////         UnqualifiedImport(name: "bar", alias: Some("xxx")),
////         UnqualifiedImport(name: "Orig", alias: None),
////         UnqualifiedImport(name: "orig", alias: None),
////       ],
////     ),
////   ]

////   deriv.consolidate_imports(list.flatten([curr_imports, add_imports]))
////   |> should.equal(expected_new_imports)

////   io.println("")
////   io.println("")
////   io.println("///// EXPECTED /////")
////   io.println(expected_src)
////   io.println("")
////   io.println("")
////   io.println("///// DERIV /////")
////   io.println(deriv.consolidate_imports_for(src, add: add_imports))

////   deriv.consolidate_imports_for(src, add: add_imports)
////   |> should.equal(expected_src)
//// }
