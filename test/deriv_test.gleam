import shellout
import glance_printer
import gleam/int
import gleam/pair
import gleam/result
import gleam/bool
import glance as g
import gleam/regexp as re
//
import gleam/list
import gleam/dynamic/decode
import gleam/json
import deriv
import deriv/internal/common
import deriv/internal/parser
import deriv/internal/types.{File, DerivFieldOpt, DerivField}
import deriv/internal/glance as dg
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

pub fn main() {
  gleeunit.main()
}

pub fn deriv_test() {
  let fs = gen_funcs()
  True |> should.be_false
  // let assert Ok(re) = re.from_string("foo(.*)")
  // echo re.scan(re, "foo")

  // fs |> list.map(fn(f) { f.0.name })
  // |> echo

  list.length(fs)
  |> should.equal(2)

  Nil
}

const gen_magic_comment_start = "//$ gen"

fn gen_funcs() -> List(Func)  {
  echo "hi"
  echo gleam_format_expr(g.Variable(z, "hi"), indent: 4)
  echo "bye"

  let assert Ok(file) = simplifile.read("./test/deriv_test.gleam")
  let assert Ok(g.Module(_, _, _, _, funcs)) = g.module(file)
  let funcs = funcs |> list.map(fn(def) { def.definition })

  list.filter_map(funcs, fn(func) {
    let length = func.location.end - func.location.start
    let func_src = string.slice(file, func.location.start, length)

    let func = Func(def: func, src: func_src)

    use <- bool.guard(!{ func_src |> string.contains(gen_magic_comment_start) }, Error(Nil))

    echo func.def.name
    // echo func.def
    // let assert [gen] = gen_comment_locations(func)
    let gens = gen_comment_locations(func)
    // echo file |> string.drop_start(func.def.location.start + gen.position)
    // echo gen.new
    edit(func, gens)

    Ok(func)
  })
}

pub fn gleam_format_expr_test() {
  gleam_format_expr(g.Variable(z, "hi"), indent: 4)
  |> should.equal("    hi")
}

pub fn replace_test() {
  replace(str: "foobarbaz", span: g.Span(start: 3, end: 6), with: "boo")
  |> should.equal("fooboobaz")
}

type Func {
  Func(
    def: g.Function,
    src: String,
  )
}

type Gen1 {
  Gen1(
    str: String,
    comment: String,
    indent: Int,
    pos: Int,
    line_pos: Int,
    new: Bool,
  )
}

type Gen {
  Gen(
    str: String,
    indent: Int,
    start: Int,
    end: Option(Int),
  )
}

type Line {
  Line(
    str: String,
    indent: Int,
    idx: Int,
  )
}

fn find_gen_end_line(
  gen gen: Gen,
  lines lines: List(#(String, Int)),
  after after: Int,
) -> Result(Line, Nil) {
  let assert Ok(end_re) =
    { "^(\\s*)[/][/][$]\\s*END\\s+gen\\s+" <> gen.str <> "$" }
    |> re.from_string

  lines
  |> list.find_map(fn(t) {
    use <- bool.guard(t.1 <= after, Error(Nil))

    let #(line, idx) = t

    use #(gen_str, indent) <- result.try(
      case re.scan(end_re, line) {
        [re.Match(_, [indent, Some(gen_str)])] ->
          Ok(#(gen_str, indent |> option.unwrap("") |> string.length))

        _ ->
          Error(Nil)
      }
    )

    Ok(Line(str: gen_str, indent:, idx:))
  })
}

fn gen_lines(
  src src: String,
) {
  let lines =
    src
    |> string.split("\n")
    |> list.index_map(pair.new)

  let assert Ok(start_re) =
    "^(\\s*)[/][/][$]\\s*(END\\s+)?gen\\s+(.+)$"
    |> re.from_string

  lines
  |> list.fold(#(None, []), fn(acc, t) {
    let #(gen, gens) = acc

    let #(line, idx) = t

    {
      use #(gen_str, indent, is_end) <- result.try(
        case re.scan(start_re, line) {
          [re.Match(_, [indent, end, Some(gen_str)])] ->
            Ok(#(
              gen_str,
              indent |> option.unwrap("") |> string.length,
              end |> option.is_some,
            ))

          _ ->
            Error(Nil)
        }
      )

      case gen, is_end {
        None, False ->
          // found new `gen`
          Ok(#(Some(gen), gens))

        Some(Gen(str: curr_gen_str, ..) as gen), True if curr_gen_str == gen_str ->
          // found end for exisiting `gen`
          Ok(#(None, [Gen(..gen, end: Some(idx))]))

        Some(Gen(str: curr_gen_str, ..) as gen), True ->
          // found end for exisiting `gen`
          Ok(#(None, [Gen(..gen, end: Some(idx))]))

        Some(gen), False ->
          // found new `gen` without finding end for prev `gen`
          // Ok(#())
          todo

        None, True ->
          // found end before first finding a `gen`
          todo
      }

      // use line <- try_unwrap(default: Ok(#(Some(gen), gens)), result: {
      //   find_gen_end_line(gen:, lines:, after: idx)
      //   // let assert Ok(end_re) =
      //   //   { "^(\\s*)[/][/][$]\\s*END\\s+gen\\s+" <> gen_str <> "$" }
      //   //   |> re.from_string

      //   // case re.scan(start_re, line) {
      //   //   [] ->
      //   //     Error(Nil)

      //   //   _ ->
      //   //     Ok(todo)
      //   // }
      // })

      // Ok(#(None, [Gen(..gen, end: Some(line.idx)), ..gens]))
      todo
    }
    |> result.unwrap(#(gen, gens))
  })
  |> fn(t) {
    let #(gen, gens) = t

    case gen {
      Some(gen) -> [gen, ..gens]
      None -> gens
    }
  }
}

//

fn gen_comment_locations(
  func func: Func,
) -> List(Gen1) {
  let lines =
    func.src
    |> string.split("\n")
    |> list.index_map(pair.new)

  let assert Ok(start_re) =
    "^((\\s*)([{]\\s*)?)([/][/][$]\\s*?gen\\s+(.+)$)"
    |> re.from_string

  lines
  |> list.fold(#(None, [], func.def.location.start), fn(acc, t) {
    let #(gen, gens, pos) = acc
    let #(line, _idx) = t
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

      let gen = Gen1(str: gen_str, comment:, indent:, pos: pos + until_comment, line_pos: pos, new: !has_block)

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

fn spans(
  func func: Func,
) -> List(#(g.Span, Option(String))) {
  let assert Ok(span_re) =
    "((\\w+)[(])?Span[(](\\d+)\\s*[,]\\s*(\\d+)[)]" |> re.from_string

  func.def.body
  |> string.inspect
  |> re.scan(span_re, _)
  |> list.filter_map(fn(m) {
    case m {
      re.Match(_, [_, expr, Some(start), Some(end)]) ->
        {
          use start <- result.try(int.parse(start))
          use end <- result.try(int.parse(end))
          Ok(#(g.Span(start:, end:), expr))
        }

      _ ->
        Error(Nil)
    }
  })
}

type Replacement{
  Replacement(
    pos: Int,
    gen: Gen1,
  )
}

type Edited(t) {
  Edited(t)
}

type EditErr {
  Miss
  Collision
}

fn edit(
  func func: Func,
  gens gens: List(Gen1),
// ) -> #(Edited(Func), List(Gen1)) {
) -> #(Edited(String), List(Gen1)) {
  // let #(rs, collisions) =
  //   list.map(rs, fn(r) {
  //     span |>
  //   })
  //   |> todo
  //   |> result.partition

  let spans = spans(func:)

  let #(gens, misses) =
    list.map(gens, fn(r) {
      list.find_map(spans, fn(t) {
        let #(span, _expr) = t

        case r.pos >= span.start && r.pos <= span.end {
          True -> Ok(#(span, r))
          False -> Error(Nil)
        }
      })
      |> result.replace_error(r)
    })
    |> result.partition

  let #(new, misses) = misses |> list.partition(fn(m) { m.new })

  use <- bool.lazy_guard(!list.is_empty(misses), fn() { panic as "misses!" })

  let #(regens, collisions) =
    gens
    |> list.group(pair.first)
    |> dict.map_values(fn(_, xs) { list.map(xs, pair.second) })
    |> dict.to_list
    |> list.map(fn(t) {
      let #(span, gens) = t
      case gens {
        [gen] -> Ok(#(span, gen))
        [] -> Error(Nil) // impossible case?
        _ -> Error(Nil)
      }
    })
    |> result.partition

  use <- bool.lazy_guard(!list.is_empty(collisions), fn() { panic as "collisions!" })

  let gens =
    regens
    |> list.map(fn(t) { #(Some(t.0), t.1) })
    |> list.append(new |> list.map(pair.new(None, _)))
    |> list.sort(fn(a, b) {
      int.compare(
        a.1.pos,
        b.1.pos,
      )
    })

  let src =
    gens
    |> list.fold(#(func.src, 0), fn(acc, t) {
      let #(#(old, offset), #(span, gen)) = #(acc, t)

      let expr = gen_expr(gen:)
      let expr_src = gleam_format_expr(expr:, indent: gen.indent)

      let #(span, expr_src) =
        case span {
          Some(span) ->
            #(span, expr_src)

          None -> {
            #(g.Span(start: gen.pos, end: gen.pos + string.length(gen.comment)), expr_src)
          }
        }

      let expr_src =
        case expr_src |> string.split("\n") {
          [] ->
            expr_src // impossible

          [_] ->
            panic as { "`gen " <> gen.str <> "` must generate a multiline block, but failed to" }

          [opening_bracket, ..rest] ->
            [opening_bracket <> " " <> gen.comment, ..rest] |> string.join("\n")
        }

      // let span = g.Span(
      //   start: func.def.location.start - span.start + offset,
      //   end: func.def.location.start - span.end + offset,
      // )
      // echo span
      let span = g.Span(
        start: span.start + offset - func.def.location.start,
        end: span.end + offset - func.def.location.start,
      )
      echo span
      let slice = string.slice(func.src, span.start, span.end)
      let newlines = slice |> string.split("\n") |> list.length |> int.subtract(1)
      let span = g.Span(..span, end: span.end - newlines)
      // panic as "glance spans are different than src locations because of formatting etc..."

      let new = old |> replace(span:, with: expr_src)
      let diff = string.length(new) - string.length(old)

      // echo span
      echo "old"
      io.println(old)
      echo "new"
      io.println(new)

      #(new, offset + diff)
    })
    |> pair.first

  io.println(func.src)
  io.println(src)

  #(Edited(src), [])
}

fn replace(
  str str: String,
  span span: g.Span,
  with new: String,
) -> String {
  let begin = string.drop_end(str, span.end)
  let end = string.drop_start(str, span.end)

  begin <> new <> end
}

fn gen_expr(
  gen gen: Gen1,
) -> g.Expression {
  g.Case(z, [g.Variable(z, "foo")], [
    g.Clause(patterns: [[g.PatternDiscard(z, "")]], guard: None, body: g.Variable(z, "foo"))
  ])
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

//

fn slice_nothing() {
  //$ gen target
}

fn slice_replace(bool: Bool) {
  { //$ gen target
    case bool {
      True -> todo
      False -> todo
    }
  }
}

pub fn gleam_format(src: String) -> String {
  let escaped_src =
    src
    |> string.replace(each: "\"", with: "\\\"")
    |> string.replace(each: "'", with: "\\'")

  let cmd = "echo \"" <> escaped_src <> "\" | gleam format --stdin"
  let assert Ok(formatted_enc) = shellout.command(run: "sh", with: ["-c", cmd], in: ".", opt: [])

  formatted_enc
}

const z = g.Span(0, 0)

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
