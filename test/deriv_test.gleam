import shellout
import glance_printer
import gleam/int
import gleam/pair
import gleam/result
import gleam/bool
import glance as g
import gleam/regexp as re
import gleam/bit_array
//
import gleam/list
import gleam/dynamic/decode
import gleam/json
import deriv
import deriv/internal/common
import deriv/internal/parser
import deriv/internal/types.{File, DerivFieldOpt, DerivField}
import deriv/internal/glance.{z} as dg
import deriv/util
import glance
import gleam/dict
import gleam/option.{Some, type Option, None}
import gleam/string
import gleeunit
import gleeunit/should
import gleam/io
import simplifile
import examples/json_rewrite/after as json_example
import bchase/list.{at as list_at} as _

pub fn main() {
  gleeunit.main()
}

// TODO
//   next
//     - allow gleam before block, e.g. `let foo = { //$ gen ...`
//     - handle multiple `//$ gen` in single file
//     - handle new case (not replace)
//     - ensure AST wrapped in `g.Block`
//   tidy
//     - refactor
//     - clean up tests
//     - define module structure
//     - move to own module

const gen_magic_comment_start = "//$ gen"

pub fn deriv_test() {
  let assert Ok(src) = simplifile.read("./test/gen/before.gleam")
  let assert Ok(module) = g.module(src)

  let output = process(src:, module:)

  let assert Ok(after) = simplifile.read("./test/gen/after.gleam")

  log_("AFTER", after)
  log_("OUTPUT", output)

  output
  |> should.equal(after)

  Nil
}

pub fn process(
  module module: g.Module,
  src src: String,
) -> String {
  use <- bool.guard(!{ src |> string.contains(gen_magic_comment_start) }, src)

  let lines = src |> string.split("\n")

  module.functions
  |> list.map(fn(func) { func.definition })
  |> list.map(build_gens(func: _, lines:))
  |> list.fold(src, run)
}

pub fn gleam_format_expr_test() {
  gleam_format_expr(g.Variable(z, "hi"), indent: 4)
  |> should.equal("    hi")
}

// pub fn gen_span_test() {
//   let assert Ok(file) = simplifile.read("./test/gen/before.gleam")
//   let assert Ok(g.Module(_, _, _, _, funcs)) = g.module(file)
//   let funcs = funcs |> list.map(fn(def) { def.definition })
//   let assert [func] = funcs

//   let length = func.location.end - func.location.start
//   let func_src = string.slice(file, func.location.start, length)
//   let func = Func(def: func, src: func_src)
//   let assert [gen] = build_gens(func)

//   echo gen

//   // echo gen_target(gen)

//   // todo
//   // |> should.equal(g.Span(116, 196))

//   Nil
// }

// fn gen_target(
//   gen gen: Gen,
//   src src: String,
// ) -> g.Span {
//   case bracket_pos(gen) {
//     None ->
//       g.Span(start: gen.pos, end: gen.pos + { gen.comment |> string.length })

//     Some(opening_bracket_pos) -> {
//       closing_position(of: curly_brackets, in: todo)
//       todo
//     }
//   }
// }

// pub fn whitespace_before_test() {
// }
// whitespace_before(
// ) {
// }
pub fn closing_position_test() {
  let src = "
fn foo(bar) {
  {
    case True {
      True -> 1
      False -> { 0 }
    }
  }  // <-- target
}

fn baz(boo) {
  todo
}" |> string.trim

  let target = "
  {
    case True {
      True -> 1
      False -> { 0 }
    }
  }" |> string.trim

  let start = 16
  let length = 63
  let end = start + length

  let chars = src |> string.to_graphemes
  chars
  |> list_at(index: start)
  |> should.be_ok
  |> should.equal(curly_brackets.open)
  chars
  |> list_at(index: end)
  |> should.be_ok
  |> should.equal(curly_brackets.close)

  src
  |> string.length
  |> should.not_equal(end)

  closing_position(of: curly_brackets, in: src, after: start)
  |> should.be_ok
  |> should.equal(length)

  src
  |> string.slice(start, length + 1)
  |> should.equal(target)

  Nil
}

type Pair {
  Pair(
    open: String,
    close: String,
  )
}

const curly_brackets = Pair("{", "}")

type ClosingPairAcc {
  ClosingPairAcc(
    pos: Int,
    open: Int,
  )
}

fn closing_position(
  of pair: Pair,
  in str: String,
  after pos: Int,
) -> Result(Int, Nil) {
  let lines =
    str
    |> string.drop_start(pos + 1)
    |> string.split("\n")
    |> list.map(string.append(_, "\n"))

  list.fold_until(lines, ClosingPairAcc(pos: 0, open: 1), fn(acc, line) {
    use <- bool.guard(acc.open <= 0, list.Stop(acc))

    list.fold_until(string.to_graphemes(line), acc, fn(acc, ch) {
      use <- bool.guard(acc.open <= 0, list.Stop(acc))

      case ch == pair.open, ch == pair.close {
        True, _ -> ClosingPairAcc(..acc, open: acc.open + 1)
        _, True -> ClosingPairAcc(..acc, open: acc.open - 1)
        _, _ -> acc
      }
      |> fn(acc) { list.Continue(ClosingPairAcc(..acc, pos: acc.pos + 1)) }
    })
    |> list.Continue
  })
  |> fn(result) {
    case result {
      ClosingPairAcc(pos:, open:) if open <= 0 -> Ok(pos)
      _ -> Error(Nil)
    }
  }
}

type Gen {
  Gen(
    str: String,
    comment: String,
    indent: Int,
    pos: Int,
    new: Bool,
  )
}

fn build_gens(
  func func: g.Function,
  lines lines: List(String),
) -> List(Gen) {
  let assert Ok(start_re) =
    "^((\\s*)([{]\\s*)?)([/][/][$]\\s*?gen\\s+(.+)$)"
    |> re.from_string

  list.fold(lines, #(None, [], func.location.start), fn(acc, line) {
    let #(gen, gens, pos) = acc
    let next_pos = pos + string.length(line)

    {
      use #(gen_str, comment, has_block, indent, until_comment) <- result.try(
        case re.scan(start_re, line) {
          [re.Match(_, [pre_comment, indent, bracket, Some(comment), Some(gen_str)])] ->
            Ok(#(
              gen_str,
              comment,
              bracket |> option.is_some,
              indent |> option.unwrap("") |> string.length,
              pre_comment |> option.unwrap("") |>  string.length,
            ))

          _ ->
            Error(Nil)
        }
      )

      let gen = Gen(
        str: gen_str,
        comment:,
        indent:,
        pos: pos + until_comment,
        new: !has_block,
      )

      Ok(#(None, [gen, ..gens], next_pos))
    }
    |> result.unwrap(#(gen, gens, next_pos))
  })
  |> fn(t) {
    let #(gen, gens, _pos) = t

    case gen {
      Some(gen) -> [gen, ..gens]
      None -> gens
    }
  }
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

fn run(
  src src: String,
  gens gens: List(Gen),
) -> String {
  gens
  |> list.fold(#(src, 0), fn(acc, gen) {
    let #(old, offset) = acc

    // build & format `glance.Expression` as `String`
    let expr = gen_expr(gen:)
    let expr_src = gleam_format_expr(expr:, indent: gen.indent)

    // add magic comment back to gen'd `glance.Expression` src
    let expr_src =
      case expr_src |> string.split("\n") {
        [] ->
          expr_src // impossible

        [_] ->
          panic as { "`gen " <> gen.str <> "` must generate a multiline block, but failed to" }

        [opening_bracket, ..rest] ->
          [opening_bracket <> " " <> gen.comment, ..rest] |> string.join("\n")
      }

    // calc span to overwrite
    let span = gen_span(gen:, src:)

    // construct new src
    let #(start, end) = dg.splice_out_span(old, span)
    let new = string.join([
      start,
      expr_src |> string.trim_start,
      end,
    ], "")

    // calc diff for new `offset`
    let diff = string.length(new) - string.length(old)

    #(new, offset + diff)
  })
  |> pair.first
}

fn gen_span(
  gen gen: Gen,
  src src: String,
) -> g.Span {
  case bracket_pos(gen) {
    None ->
      g.Span(start: gen.pos, end: gen.pos + { gen.comment |> string.length })

    Some(start) -> {
      src
      |> closing_position(in: _, of: curly_brackets, after: start)
      |> fn(result) {
        case result {
          Error(Nil) -> panic as {
            "couldn't find the closing bracket of existing code gen block"
          }

          Ok(end) ->
            g.Span(start:, end: end + start)
        }
      }
    }
  }
}

fn bracket_pos(
  gen gen: Gen,
) -> Option(Int) {
  use <- bool.guard(gen.new, None)
  Some(gen.pos + gen.indent + 1)
}

fn gen_expr(
  gen gen: Gen,
) -> g.Expression {
  g.Block(z, [g.Expression(
    g.Case(z, [g.Variable(z, "foo")], [
      g.Clause(patterns: [[g.PatternDiscard(z, "")]], guard: None, body: g.Variable(z, "foo")),
    ]),
  )])
}

fn gleam_format_expr(
  expr expr: g.Expression,
  indent indent: Int,
) -> String {
  g.Module([], [], [], [], [g.Definition([], g.Function(
    z, "main", g.Public, [], return: None, body: [g.Expression(
      expr
    )]
  ))])
  |> glance_printer.print
  |> string.split("\n")
  |> list.drop(1)
  |> fn(lines) { list.take(lines, list.length(lines) - 2) }
  |> fn(lines) {
    case indent {
      2 -> lines
      0 -> lines |> list.map(string.drop_start(_, 2))
      1 -> lines |> list.map(string.drop_start(_, 1))
      _ -> {
        let ws = list.repeat(" ", indent - 2) |> string.join("")
        lines |> list.map(string.append(to: ws, suffix: _))
      }
    }
  }
  |> string.join("\n")
}

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
