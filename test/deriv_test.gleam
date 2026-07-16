import gleam/set.{type Set}
import bchase/result.{try_err, try_fail, try_fail_} as _
import bchase/function.{always}
import gleam/pair
import gleam/result
import gleam/regexp as re
import glance as g
//
import gleam/list
import gleam/dynamic/decode
import gleam/json.{type Json}
import deriv
import deriv/internal/common
import deriv/internal/parser
import deriv/internal/types.{File, DerivFieldOpt, DerivField}
import deriv/internal/glance.{z} as dg
import deriv/util
import glance
import gleam/dict.{type Dict}
import gleam/option.{Some, type Option, None}
import gleam/string
import gleeunit
import gleeunit/should
import bchase/io.{log_err}
import simplifile
import examples/json_rewrite/after as json_example
import bchase/list.{at as list_at} as _
import deriv/internal/gen.{type Ref}
//
import gleam/otp/actor
import gleam/otp/supervision
import gleam/otp/static_supervisor as supervisor
import gleam/erlang/process.{type Subject, type Selector}
import filespy
import gleam/crypto
import gleam/bit_array as ba
import deriv/gen/types.{type TypeDef, type GleamPath, type GleamFile} as _
import deriv/gen/supervisor as gs

// convention
//   - `func` takes extra, e.g. lens, mapping func
//   - `func_` drops, e.g. whole value instead of lens into, no need to map result err

// finish
//   next
//     - finish `refs` tracking
//       * ensure general ref tracking inits & updates correctly
//       * force regen of all refs upon successful code gen write
//   improve
//     external api (types/funcs)
//       - review naming
//       - def rework
//       - change
//     magic comments
//       - gen str helpers
//         * shorthand  -- //$ gen                func foo bar
//         * named args -- //$ gen package/module.func target:Foo:type dest:bar:function

// app consumer-side
//   expr gen
//     server-side
//     X - `f` type
//       - `crud` & `sub` types
//     client-side
//       - decode returns
//       - req funcs
//   forms
//     - ...

// todo
//   organize
//     - figure out modules
//     - rename types & funcs

//   derive
//     - impl with `ExprGen`
//     ? * ... rework `VariantExpr` to `Expr(glance.Variant)`?
//     - try defining something simple, e.g. `zero`, `enum`

//   other gen ideas
//     - top-level `//$ foo.reexport module:foo/bar/baz`
//       * reexports target module consts, types, aliases, & funcs *w/ params*

//   tests
//     - write tests for code reloading / file changes
//       * saved changes with new `ExprGen` (overwrite `defs.gleam`)
//       * saved changes with `//$ gen` (overwrite src w/ gen'd code)

// MAYBE
//   improve
//     magic comments
//       - gen str helpers
//         * named args -- //$ gen package/module.func target:Foo:type dest:bar:function
//         * named args -- //$ gen package/module.func target:Foo      dest:bar
//           - ^ would need helpers like `use type_: #(g.Type, Result(g.CustomType), Nil) <- type_for_gen_param(name: "target")`
//     ? - maybe type/variant/field attributes
//       * e.g. for routes gen, `//$ app/mod.gen:route:"/api/parent/:parent_id/things"` on endpoint variant

// todo
//   - rework from hashes to file modified times?
//     * persist last `filespy` event time (file)
//   - ignore refs w/ modules `from == to`, also don't write to monad or send
// fix
// ? - adjust `gen.run` to use lookup actor instead of direct `get_custom_type` call?
//   ? * not using `LookupState.filepaths`?
// actors
// X - look up`CustomType`s
// X - watch for file changes (`filespy`)
// \ - track magic comment references
//     * init
//   X * update
//   - serve & update gen logic helpers
//     * init
//     * update
//     * reply
// \ - main
//   X * gen & write code on file change
//   X * track new references
//     * ...

// module gen
//   template
//     - module as template with... slots?
//     - means of tagging types & case statements as points of future code gen

// TODO
//   next
//     - handle multiple `//$ gen` in single file
//     - allow gleam before block, e.g. `let foo = { //$ gen ...`
//     - handle new case (not replace)

//

pub fn main() {
  gleeunit.main()
}

fn log(str, x) {
  log_(str, string.inspect(x))
}

fn log_(str, s) {
  io.println("")
  io.println("")
  io.println(str)
  io.println(s)
}

pub fn gen_named_params_test() {
  // let str = "foo:\"123\" hoge:\"fuka \\\"piyo\\\" bar:\"baz_boo\""
  let str = "foo:\"123\" hoge:\"fuka piyo\"  bar:\"baz_boo\""

  dict.from_list([
    #("foo", "123"),
    // #("hoge", "\"fuka \\\"piyo\\\""),
    #("hoge", "fuka piyo"),
    #("bar", "baz_boo"),
  ])
  |> should.equal(parser.parse_named_params(str), _)
}

pub fn gen_test() {
  let assert Ok(pwd) = gen.pwd()
  let assert Ok(toml) = gen.gleam_toml()
  let assert Ok(file) = gen.load_gleam_file("src/deriv/internal/dummy/gen/before.gleam")
  let ctx = gen.Context(pwd:, toml:, file:)

  let assert Ok(#(output, _refs)) = gen.process(ctx:)

  let assert Ok(after) = simplifile.read("src/deriv/internal/dummy/gen/after.gleam")

  log_("AFTER", after)
  log_("OUTPUT", output)

  let output = string.trim(output)
  let after = string.trim(after)

  output
  |> should.equal(after)

  Nil
}

pub fn custom_type_lookup_test() {
  let cfg = gs.build_config()
  let assert Ok(_) = supervisor.start(gs.supervisor(cfg.names))

  // process.sleep_forever()

  let assert Ok(file) = gen.load_gleam_file( "src/deriv/internal/dummy/lookup.gleam")

  // echo modules_affected_by_change_to(file:, x: X(refs: dict.new())) |> pair.second

  [
    // curr package
    gs.look_up_type(cfg.names.lookup, file:, mod: None, name: "Local"),
    gs.look_up_type(cfg.names.lookup, file:, mod: None, name: "LocalAlias"),
    gs.look_up_type(cfg.names.lookup, file:, mod: None, name: "OtherImport"),
    gs.look_up_type(cfg.names.lookup, file:, mod: Some("lookup_other"), name: "Other"),
    gs.look_up_type(cfg.names.lookup, file:, mod: Some("oo"), name: "OtherOther"),
    // dep
    gs.look_up_type(cfg.names.lookup, file:, mod: Some("glance"), name: "Span"),
    // dep at path
    gs.look_up_type(cfg.names.lookup, file:, mod: Some("id"), name: "Id"),
    // dep package name doesn't match module name
    gs.look_up_type(cfg.names.lookup, file:, mod: Some("option"), name: "Option"),
  ]
  |> list.each(should.be_ok)

  Nil
}

// pub fn gleam_format_expr_test() {
//   gleam_format_expr(g.Variable(z, "hi"), indent: 4)
//   |> should.equal("    hi")
// }

// pub fn closing_position_test() {
//   let src = "
// fn foo(bar) {
//   {
//     case True {
//       True -> 1
//       False -> { 0 }
//     }
//   }  // <-- target
// }

// fn baz(boo) {
//   todo
// }" |> string.trim

//   let target = "
//   {
//     case True {
//       True -> 1
//       False -> { 0 }
//     }
//   }" |> string.trim

//   let start = 16
//   let length = 63
//   let end = start + length

//   let chars = src |> string.to_graphemes
//   chars
//   |> list_at(index: start)
//   |> should.be_ok
//   |> should.equal(curly_brackets.open)
//   chars
//   |> list_at(index: end)
//   |> should.be_ok
//   |> should.equal(curly_brackets.close)

//   src
//   |> string.length
//   |> should.not_equal(end)

//   closing_position(of: curly_brackets, in: src, after: start)
//   |> should.be_ok
//   |> should.equal(length)

//   src
//   |> string.slice(start, length + 1)
//   |> should.equal(target)

//   Nil
// }

// OLD

//pub fn glance_read(file_path: String) -> glance.Module {
//  let assert Ok(src) = simplifile.read(file_path)
//  let assert Ok(module) = glance.module(src)
//  module
//}

//pub fn glance_print(file_path: String) -> Nil {
//  file_path
//  |> glance_read
//  |> string.inspect
//  |> io.println
//}

//pub fn glance_read_and_write(
//  from input: String,
//  to output: String,
//) -> Nil {
//  input
//  |> glance_read
//  |> string.inspect
//  |> simplifile.write(to: output, contents: _)
//  |> fn(r) { case r {
//    Error(err) -> {
//      panic as string.inspect(err)
//    }

//    Ok(_) -> {
//      Nil
//    }
//  } }
//}

//fn should_derive(
//  example_dir_name example_dir_name: String,
//) {
//  let Example(before: input, after: output) = example_dir_path(example_dir_name:)

//  run_and_expect_equal(input:, output:, example_dir_name:)
//}

//fn run_and_expect_equal(
//  input input: String,
//  output output: String,
//  example_dir_name example_dir_name: String,
//) {
//  let output = output |> string.trim

//  let assert [write] = gen_and_build_writes(input: input |> string.trim, example_dir_name:)
//  let gen = write.src |> string.trim

//  // let assert Ok(_) = simplifile.write(to: "expected.txt", contents: output)
//  // let assert Ok(_) = simplifile.write(to: "generated.txt", contents: gen)

//  case gen == output {
//    True -> Nil
//    False -> {
//      io.println("")
//      io.println("")
//      io.println("EXPECTED")
//      io.println(output)
//      io.println("")
//      io.println("")
//      io.println("GENERATED")
//      io.println(gen)
//      io.println("DIFF (<expected >generated)")
//      io.println(common.diff(output, gen))
//    }
//  }

//  gen
//  |> should.equal(output)
//}

//fn gen_and_build_writes(
//  input input: String,
//  example_dir_name example_dir_name: String,
//) -> List(types.Write) {
//  let files = [ File(module: "examples/" <> example_dir_name <> "/before", src: input, idx: Some(1)) ]

//  files
//  // |> deriv.gen_derivs(build_module_reader([]))
//  |> deriv.gen_derivs(common.fetch_module_(path: _, path_prefix: "test/"))
//  |> deriv.build_writes
//}

//// fn build_module_reader(
////   files: List(#(String, String)),
//// ) -> types.ModuleReader {
////   fn(ident) {
////     case dict.get(dict.from_list(files), ident) {
////       Ok(src) ->
////         src
////         |> string.trim
////         |> glance.module
////         |> result.map_error(types.GlanceErr)

////       _ ->
////         panic as { "`build_module_reader` miss for ident: " <> ident }
////     }
////   }
//// }

//type Example {
//  Example(
//    before: String,
//    after: String,
//  )
//}

//fn example_dir_path(
//  example_dir_name example_dir_name: String,
//) -> Example {
//  let example_dir_path = "test/examples/" <> example_dir_name <> "/"

//  let before_file_path = example_dir_path <> "before.gleam"
//  let after_file_path = example_dir_path <> "after.gleam"

//  let assert Ok(before) = simplifile.read(before_file_path)
//  let assert Ok(after) = simplifile.read(after_file_path)

//  Example(before:, after:)
//}

//// START DERIV EXAMPLE TESTS

//// TEST DERIV JSON REWRITE
//pub fn json_rewrite_test() {
//  should_derive(example_dir_name: "json_rewrite")
//}

//pub fn json_opts_test() {
//  should_derive(example_dir_name: "json_opts")
//}

//pub fn json_misc_test() {
//  should_derive(example_dir_name: "json_misc")
//}

//pub fn json_guard_test() {
//  should_derive(example_dir_name: "json_guard")
//}

//pub fn json_encoded_value_decodes_to_identical_test() {
//  let foo = json_example.zero_foo()
//  foo
//  |> json_example.encode_foo
//  |> json.to_string
//  // |> fn(str) {
//  //   io.println(str)
//  //   str
//  // }
//  |> json.parse(json_example.decoder_foo())
//  |> should.be_ok
//  |> should.equal(foo)
//}

//pub fn json_properties_test() {
//  should_derive(example_dir_name: "json_properties")
//}

//pub fn json_set_test() {
// should_derive(example_dir_name: "json_set")
//}

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

//// TEST DERIVE FUNCTOR

//pub fn functor_test() {
// should_derive(example_dir_name: "functor")
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

////pub fn birl_json_test() {
////  should_derive(example_dir_name: "json_birl")
////}

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

//pub fn json_decoder_parameterized_type_test() {
//  should_derive(example_dir_name: "json_decoder_parameterized_type")
//}
//pub fn json_encode_parameterized_type_test() {
//  should_derive(example_dir_name: "json_encode_parameterized_type")
//}
//pub fn json_encode_nested_parameterized_type_alias_test() {
//  should_derive(example_dir_name: "json_encode_nested_parameterized_type_alias")
//}
//// // TODO started reworking, but not sure the types even make sense...
//// pub fn json_encode_nested_parameterized_type_alias_list_test() {
////   should_derive(example_dir_name: "json_encode_nested_parameterized_type_alias_list")
//// }

//// DERIVE TEST INTO & FROM

//pub fn from_test() {
//  should_derive(example_dir_name: "from")
//}

//pub fn into_test() {
//  should_derive(example_dir_name: "into")
//}

//pub fn into_simple_test() {
//  should_derive(example_dir_name: "into_simple")
//}

//// TEST GENERAL

//// pub fn multiple_run_and_filepath_test() {
////   let Example(before: input, after: output) = example_dir_path(example_dir_name: "json")

////   let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

////   let assert [write] =
////     files
////     |> deriv.gen_derivs(dummy_module_reader)
////     |> deriv.build_writes

////   let files = [ File(module: "deriv/example/foo", src: write.src, idx: Some(1)) ]

////   let writes =
////     files
////     |> deriv.gen_derivs(dummy_module_reader)
////     |> deriv.build_writes

////   let assert [write] =
////     writes

////   let input = write.src

////   write.filepath
////   |> should.equal("src/deriv/example/foo.gleam")

////   run_and_expect_equal(input:, output:, example_dir_name: "deriv/example/foo")
//// }

//// fn dummy_module_reader(_) {
////   panic as "`dummy_module_reader`"
//// }

//// TEST HELPERS

//pub fn is_test() {
//  "{\"foo\":\"bar\"}"
//  |> json.parse({
//    use str <- decode.field("foo", util.is("bar"))
//    decode.success(str)
//  })
//  |> should.be_ok
//  |> should.equal(Nil)

//  "{\"foo\":\"boom\"}"
//  |> json.parse({
//    use str <- decode.field("foo", util.is("bar"))
//    decode.success(str)
//  })
//  |> should.be_error

//  "{\"boom\":\"bar\"}"
//  |> json.parse({
//    use str <- decode.field("foo", util.is("bar"))
//    decode.success(str)
//  })
//  |> should.be_error
//}

//pub fn hyphen_case_test() {
//  common.hyphen_case("-")
//  |> should.equal("-")

//  common.hyphen_case("FooBar")
//  |> should.equal("foo-bar")

//  common.hyphen_case("Foo")
//  |> should.equal("foo")

//  common.hyphen_case("FooBarX")
//  |> should.equal("foo-bar-x")

//  common.hyphen_case("Foo123Bar")
//  |> should.equal("foo123-bar")

//  common.hyphen_case("FooBar1")
//  |> should.equal("foo-bar1")

//  common.hyphen_case("FooBar12")
//  |> should.equal("foo-bar12")

//  common.hyphen_case("FooBar123")
//  |> should.equal("foo-bar123")
//}

//pub fn snake_case_test() {
//  common.snake_case("_")
//  |> should.equal("_")

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

////pub fn gleam_format_magic_comment_parsing_test() {
////  let src = "
////pub type T {
////  //$ derive json decode
////  A(
////    foo: String,
////    //$ json foo bar
////    //$ json baz boo
////  )
////}
////  "
////  |> string.trim

////  let assert Ok(glance.Module(custom_types: [type_], ..)) = glance.module(src)

////  let assert Ok(#(_type, [deriv], field_opts)) = parser.parse_type_with_derivations(type_.definition, src)

////  deriv.name
////  |> should.equal("json")
////  deriv.opts
////  |> should.equal(["decode"])

////  let expected =
////    [
////      #(DerivField(type_: "T", variant: "A", field: "foo"), [
////        DerivFieldOpt(strs: ["json", "foo", "bar"], raw: "json foo bar"),
////        DerivFieldOpt(strs: ["json", "baz", "boo"], raw: "json baz boo"),
////      ]),
////    ]
////    |> dict.from_list

////  field_opts
////  |> should.equal(expected)
////}

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

//pub fn splice_out_span_test() {
//  let span = glance.Span(start: 6, end: 10)

//  "hello world how are you"
//  |> dg.splice_out_span(span)
//  |> should.equal(#("hello ", " how are you"))
//}

//pub fn deriv_decode_failure_test() {
//  json.parse("", util.decode_failure("Nil"))
//  |> should.be_error
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
