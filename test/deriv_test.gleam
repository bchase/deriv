 import gleam/int
 import gleam/dynamic/decode.{type Decoder}
 import gleam/json.{type Json}
 import gleam/dict.{type Dict}
 import gleam/option.{type Option, Some, None}
 import gleam/result
 import gleam/list
 import gleeunit
 import gleeunit/should
 import gleam/string
 import deriv/types.{type File, File, DerivFieldOpt, DerivField}
 import deriv/parser
 import deriv
 import deriv/util
 import youid/uuid
 import gleam/io
 import shellout
 import simplifile
 import glance.{Import, UnqualifiedImport, Named}

 pub fn suppress_io_warnings() { io.debug(Nil) }

 pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }

 pub fn dummy_module_reader(_) {
   panic as "`dummy_module_reader`"
 }

 pub fn main() {
   gleeunit.main()
 }

pub fn json_test() {
  let input = "
import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type Foo {
  //$ derive json decode encode
  Foo(
    uuid: Uuid,
    id: Int, //$ json named int_id
    name: String,
    active: Bool,
    ratio: Float,
    words: List(String),
    maybe_list: Option(List(String)),
  )
}

pub type Bar {
  //$ derive json decode
  Bar(
    baz: Bool,
  )
}
  "
  |> string.trim

 let output = "
import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None}
import youid/uuid.{type Uuid}

pub type Foo {
  //$ derive json decode encode
  Foo(
    uuid: Uuid,
    id: Int, //$ json named int_id
    name: String,
    active: Bool,
    ratio: Float,
    words: List(String),
    maybe_list: Option(List(String)),
  )
}

pub type Bar {
  //$ derive json decode
  Bar(
    baz: Bool,
  )
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use uuid <- decode.field(\"uuid\", util.decoder_uuid())
  use id <- decode.field(\"int_id\", decode.int)
  use name <- decode.field(\"name\", decode.string)
  use active <- decode.field(\"active\", decode.bool)
  use ratio <- decode.field(\"ratio\", decode.float)
  use words <- decode.field(\"words\", decode.list(decode.string))
  use maybe_list <- decode.optional_field(
    \"maybe_list\",
    None,
    decode.optional(decode.list(decode.string)),
  )
  decode.success(Foo(uuid:, id:, name:, active:, ratio:, words:, maybe_list:))
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #(\"uuid\", util.encode_uuid(value.uuid)),
        #(\"int_id\", json.int(value.id)),
        #(\"name\", json.string(value.name)),
        #(\"active\", json.bool(value.active)),
        #(\"ratio\", json.float(value.ratio)),
        #(\"words\", json.array(value.words, json.string)),
        #(
          \"maybe_list\",
          json.nullable(value.maybe_list, json.array(_, json.string)),
        ),
      ])
  }
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.one_of(decoder_bar_bar(), [])
}

pub fn decoder_bar_bar() -> Decoder(Bar) {
  use baz <- decode.field(\"baz\", decode.bool)
  decode.success(Bar(baz:))
}
  "
  |> string.trim

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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_multi_variant_type_test() {
  let input = "
pub type T {
  //$ derive json decode encode
  X(foo: String)
  Y(bar: Int)
}
  "
  |> string.trim

 let output = "
import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type T {
  //$ derive json decode encode
  X(foo: String)
  Y(bar: Int)
}

pub fn decoder_t() -> Decoder(T) {
  decode.one_of(decoder_t_x(), [decoder_t_y()])
}

pub fn decoder_t_x() -> Decoder(T) {
  use _deriv_var_constr <- decode.field(\"_var\", util.is(\"X\"))
  use foo <- decode.field(\"foo\", decode.string)
  decode.success(X(foo:))
}

pub fn decoder_t_y() -> Decoder(T) {
  use _deriv_var_constr <- decode.field(\"_var\", util.is(\"Y\"))
  use bar <- decode.field(\"bar\", decode.int)
  decode.success(Y(bar:))
}

pub fn encode_t(value: T) -> Json {
  case value {
    X(..) as value ->
      json.object([
        #(\"_var\", json.string(\"X\")),
        #(\"foo\", json.string(value.foo)),
      ])
    Y(..) as value ->
      json.object([#(\"_var\", json.string(\"Y\")), #(\"bar\", json.int(value.bar))])
  }
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/mvar", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(dummy_module_reader)
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
  io.println("")
  io.println("")
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.src
  |> should.equal(output)
}

pub fn json_optional_field_test() {
  let input = "
pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
  )
}
  "
  |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{None}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
  )
}

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.one_of(decoder_maybe_maybe(), [])
}

pub fn decoder_maybe_maybe() -> Decoder(Maybe) {
  use name <- decode.optional_field(
    \"name\",
    None,
    decode.optional(decode.string),
  )
  decode.success(Maybe(name:))
}

pub fn encode_maybe(value: Maybe) -> Json {
  case value {
    Maybe(..) as value ->
      json.object([#(\"name\", json.nullable(value.name, json.string))])
  }
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/maybe", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(dummy_module_reader)
    |> deriv.build_writes

  write.filepath
  |> should.equal("src/deriv/example/maybe.gleam")

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
  io.println(util.diff(output, write.src))

  write.src
  |> should.equal(output)
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

pub fn nested_type_test() {
  let input = "
pub type A {
  //$ derive json decode encode
  A(
    b: B,
  )
}

pub type B {
  //$ derive json decode encode
  B(
    x: String,
  )
}
  "
  |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type A {
  //$ derive json decode encode
  A(
    b: B,
  )
}

pub type B {
  //$ derive json decode encode
  B(
    x: String,
  )
}

pub fn decoder_a() -> Decoder(A) {
  decode.one_of(decoder_a_a(), [])
}

pub fn decoder_a_a() -> Decoder(A) {
  use b <- decode.field(\"b\", decoder_b())
  decode.success(A(b:))
}

pub fn encode_a(value: A) -> Json {
  case value {
    A(..) as value -> json.object([#(\"b\", encode_b(value.b))])
  }
}

pub fn decoder_b() -> Decoder(B) {
  decode.one_of(decoder_b_b(), [])
}

pub fn decoder_b_b() -> Decoder(B) {
  use x <- decode.field(\"x\", decode.string)
  decode.success(B(x:))
}

pub fn encode_b(value: B) -> Json {
  case value {
    B(..) as value -> json.object([#(\"x\", json.string(value.x))])
  }
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/nested", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(dummy_module_reader)
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
  io.println("")
  io.println("")
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

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

  util.replace_function(src, func_name: "foo", func_src: new)
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

  util.replace_type(src, type_name: "Bar", type_src: new)
  |> string.trim
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

pub fn birl_json_test() {
  let input = string.trim("
import birl.{type Time}

pub type DateTimeExamples {
  //$ derive json decode encode
  DateTimeExamples(
    t0: Time,
    t1: Time, //$ json birl iso8601
    t2: Time, //$ json birl naive
    t3: Time, //$ json birl http
    t4: Time, //$ json birl unix
    t5: Time, //$ json birl unix_milli
    t6: Time, //$ json birl unix_micro
  )
}
")

  let output = string.trim("
import birl.{type Time}
import deriv/util
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type DateTimeExamples {
  //$ derive json decode encode
  DateTimeExamples(
    t0: Time,
    t1: Time, //$ json birl iso8601
    t2: Time, //$ json birl naive
    t3: Time, //$ json birl http
    t4: Time, //$ json birl unix
    t5: Time, //$ json birl unix_milli
    t6: Time, //$ json birl unix_micro
  )
}

pub fn decoder_date_time_examples() -> Decoder(DateTimeExamples) {
  decode.one_of(decoder_date_time_examples_date_time_examples(), [])
}

pub fn decoder_date_time_examples_date_time_examples() -> Decoder(
  DateTimeExamples,
) {
  use t0 <- decode.field(\"t0\", util.decoder_birl_parse())
  use t1 <- decode.field(\"t1\", util.decoder_birl_parse())
  use t2 <- decode.field(\"t2\", util.decoder_birl_from_naive())
  use t3 <- decode.field(\"t3\", util.decoder_birl_from_http())
  use t4 <- decode.field(\"t4\", util.decoder_birl_from_unix())
  use t5 <- decode.field(\"t5\", util.decoder_birl_from_unix_milli())
  use t6 <- decode.field(\"t6\", util.decoder_birl_from_unix_micro())
  decode.success(DateTimeExamples(t0:, t1:, t2:, t3:, t4:, t5:, t6:))
}

pub fn encode_date_time_examples(value: DateTimeExamples) -> Json {
  case value {
    DateTimeExamples(..) as value ->
      json.object([
        #(\"t0\", util.encode_birl_to_iso8601(value.t0)),
        #(\"t1\", util.encode_birl_to_iso8601(value.t1)),
        #(\"t2\", util.encode_birl_to_naive(value.t2)),
        #(\"t3\", util.encode_birl_to_http(value.t3)),
        #(\"t4\", util.encode_birl_to_unix(value.t4)),
        #(\"t5\", util.encode_birl_to_unix_milli(value.t5)),
        #(\"t6\", util.encode_birl_to_unix_micro(value.t6)),
      ])
  }
}
")

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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn dict_fields_json_test() {
  let input = string.trim("
pub type DictFieldType {
  //$ derive json decode encode
  DictFieldType(
    dict: Dict(String, Int),
  )
}
")

  let output = string.trim("
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type DictFieldType {
  //$ derive json decode encode
  DictFieldType(
    dict: Dict(String, Int),
  )
}

pub fn decoder_dict_field_type() -> Decoder(DictFieldType) {
  decode.one_of(decoder_dict_field_type_dict_field_type(), [])
}

pub fn decoder_dict_field_type_dict_field_type() -> Decoder(DictFieldType) {
  use dict <- decode.field(\"dict\", decode.dict(decode.string, decode.int))
  decode.success(DictFieldType(dict:))
}

pub fn encode_dict_field_type(value: DictFieldType) -> Json {
  case value {
    DictFieldType(..) as value ->
      json.object([#(\"dict\", json.dict(value.dict, string.inspect, json.int))])
  }
}
")

  // let assert Ok(module) = glance.module(output)
  // io.debug(module)

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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn unnested_json_test() {
  let input = "
pub type Unnested {
  //$ derive json decode encode
  Unnested(
    unnested: Int, //$ json named foo.bar.baz
  )
}
  "
  |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Unnested {
  //$ derive json decode encode
  Unnested(
    unnested: Int, //$ json named foo.bar.baz
  )
}

pub fn decoder_unnested() -> Decoder(Unnested) {
  decode.one_of(decoder_unnested_unnested(), [])
}

pub fn decoder_unnested_unnested() -> Decoder(Unnested) {
  use unnested <- decode.subfield([\"foo\", \"bar\", \"baz\"], decode.int)
  decode.success(Unnested(unnested:))
}

pub fn encode_unnested(value: Unnested) -> Json {
  case value {
    Unnested(..) as value ->
      json.object([
        #(
          \"foo\",
          json.object([
            #(\"bar\", json.object([#(\"baz\", json.int(value.unnested))])),
          ]),
        ),
      ])
  }
}
  "
  |> string.trim

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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn snake_case_test() {
  util.snake_case("FooBar")
  |> should.equal("foo_bar")

  util.snake_case("Foo")
  |> should.equal("foo")

  util.snake_case("FooBarX")
  |> should.equal("foo_bar_x")

  util.snake_case("Foo123Bar")
  |> should.equal("foo123_bar")

  util.snake_case("FooBar1")
  |> should.equal("foo_bar1")

  util.snake_case("FooBar12")
  |> should.equal("foo_bar12")

  util.snake_case("FooBar123")
  |> should.equal("foo_bar123")
}

pub fn json_specify_decoder_and_encode_test() {
  let input = "
pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
    //$ json decoder decoder_name
    //$ json encode encode_func_name
  )
}
  "
  |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{None}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
    //$ json decoder decoder_name
    //$ json encode encode_func_name
  )
}

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.one_of(decoder_maybe_maybe(), [])
}

pub fn decoder_maybe_maybe() -> Decoder(Maybe) {
  use name <- decode.optional_field(\"name\", None, decoder_name())
  decode.success(Maybe(name:))
}

pub fn encode_maybe(value: Maybe) -> Json {
  case value {
    Maybe(..) as value ->
      json.object([#(\"name\", json.nullable(value.name, encode_func_name))])
  }
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/maybe", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(dummy_module_reader)
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
  io.println("")
  io.println("")
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.src
  |> should.equal(output)
}

pub fn unify_authe_test() {
  let authe_a_src = "
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
        io.debug(ident)
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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn into_test() {
  // glance.module("
// pub fn foo() {
  // f.Foo
// }
  // " |> string.trim)
  // |> io.debug
  // |> io.debug
  // |> io.debug
  // |> io.debug
  // |> io.debug

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
        io.debug(ident)
        panic as "`module_reader` miss in `into_test`"
      }
    }
  }

// TODO
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
        io.debug(ident)
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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn zero_test() {
  let input = "
import birl.{type Time}
import gleam/option.{type Option}
import youid/uuid.{type Uuid}

pub type Player {
  //$ derive zero
  Player(
    id: Uuid,
    name: String,
    alias: Option(String),
    bday: Time,
    age: Int,
    kd: Float,
    hobbies: List(String),
    active: Bool,
  )
}
  " |> string.trim

  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      _ -> {
        io.debug(ident)
        panic as "`module_reader` miss in `zero_test`"
      }
    }
  }

 let output = "
import birl.{type Time}
import gleam/option.{type Option, None}
import youid/uuid.{type Uuid}

pub type Player {
  //$ derive zero
  Player(
    id: Uuid,
    name: String,
    alias: Option(String),
    bday: Time,
    age: Int,
    kd: Float,
    hobbies: List(String),
    active: Bool,
  )
}

pub fn zero_player() -> Player {
  Player(util.zero_uuid(), \"\", None, util.zero_time(), 0, 0.0, [], False)
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
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_parameterized_type_decoder_test() {
  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      // "project/asdf/foo" -> {
      //   let foo_src = "
// pub type Field(t) {
  // Field(
    // id: String,
    // value: t,
  // )
// }
      //   " |> string.trim

      //   let parse = fn(src) {
      //     glance.module(src)
      //     |> result.map_error(types.GlanceErr)
      //   }

      //   parse(foo_src)
      // }

      _ -> {
        io.debug(ident)
        panic as "`module_reader` miss in `parameterized_type_test`"
      }
    }
  }

// TODO
  let input = "
pub type Field(key, val) {
  //$ derive json decode
  Field(
    key: key,
    val: val,
  )
}

pub type Foo {
  //$ derive json decode
  Foo(
    scalar: Field(String, String),
    list: List(Field(String, String)),
    option: Option(Field(String, String)),
    option_list: Option(List(Field(String, String))),
  )
}
  " |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{None}

pub type Field(key, val) {
  //$ derive json decode
  Field(
    key: key,
    val: val,
  )
}

pub type Foo {
  //$ derive json decode
  Foo(
    scalar: Field(String, String),
    list: List(Field(String, String)),
    option: Option(Field(String, String)),
    option_list: Option(List(Field(String, String))),
  )
}

pub fn decoder_field(
  decoder_key: Decoder(key),
  decoder_val: Decoder(val),
) -> Decoder(Field(key, val)) {
  decode.one_of(decoder_field_field(decoder_key, decoder_val), [])
}

pub fn decoder_field_field(
  decoder_key: Decoder(key),
  decoder_val: Decoder(val),
) -> Decoder(Field(key, val)) {
  use key <- decode.field(\"key\", decoder_key)
  use val <- decode.field(\"val\", decoder_val)
  decode.success(Field(key:, val:))
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use scalar <- decode.field(
    \"scalar\",
    decoder_field(decode.string, decode.string),
  )
  use list <- decode.field(
    \"list\",
    decode.list(decoder_field(decode.string, decode.string)),
  )
  use option <- decode.optional_field(
    \"option\",
    None,
    decode.optional(decoder_field(decode.string, decode.string)),
  )
  use option_list <- decode.optional_field(
    \"option_list\",
    None,
    decode.optional(decode.list(decoder_field(decode.string, decode.string))),
  )
  decode.success(Foo(scalar:, list:, option:, option_list:))
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
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_parameterized_type_encoder_test() {
  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      // "project/asdf/foo" -> {
      //   let foo_src = "
// pub type Field(t) {
  // Field(
    // id: String,
    // value: t,
  // )
// }
      //   " |> string.trim

      //   let parse = fn(src) {
      //     glance.module(src)
      //     |> result.map_error(types.GlanceErr)
      //   }

      //   parse(foo_src)
      // }

      _ -> {
        io.debug(ident)
        panic as "`module_reader` miss in `parameterized_type_test`"
      }
    }
  }

// TODO
  let input = "
pub type Field(key, val) {
  //$ derive json encode
  Field(
    key: key,
    val: val,
  )
}

pub type Foo {
  //$ derive json encode
  Foo(
    scalar: Field(String, String),
    list: List(Field(String, String)),
    option: Option(Field(String, String)),
    option_list: Option(List(Field(String, String))),
  )
}
  " |> string.trim

 let output = "
import gleam/json.{type Json}
import gleam/list

pub type Field(key, val) {
  //$ derive json encode
  Field(
    key: key,
    val: val,
  )
}

pub type Foo {
  //$ derive json encode
  Foo(
    scalar: Field(String, String),
    list: List(Field(String, String)),
    option: Option(Field(String, String)),
    option_list: Option(List(Field(String, String))),
  )
}

pub fn encode_field(
  value: Field(key, val),
  encode_key: fn(key) -> Json,
  encode_val: fn(val) -> Json,
) -> Json {
  case value {
    Field(..) as value ->
      json.object([
        #(\"key\", encode_key(value.key)),
        #(\"val\", encode_val(value.val)),
      ])
  }
}

pub fn encode_foo(value: Foo) -> Json {
  case value {
    Foo(..) as value ->
      json.object([
        #(\"scalar\", encode_field(value.scalar, json.string, json.string)),
        #(
          \"list\",
          json.array(value.list, encode_field(_, json.string, json.string)),
        ),
        #(
          \"option\",
          json.nullable(value.option, encode_field(_, json.string, json.string)),
        ),
        #(
          \"option_list\",
          json.nullable(
            value.option_list,
            json.array(_, encode_field(_, json.string, json.string)),
          ),
        ),
      ])
  }
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
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_decoder_local_type_alias_test() {
  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      // "project/asdf/foo" -> {
      //   let foo_src = "
// pub type Field(t) {
  // Field(
    // id: String,
    // value: t,
  // )
// }
      //   " |> string.trim

      //   let parse = fn(src) {
      //     glance.module(src)
      //     |> result.map_error(types.GlanceErr)
      //   }

      //   parse(foo_src)
      // }

      _ -> {
        io.debug(ident)
        panic as "`module_reader` miss in `parameterized_type_test`"
      }
    }
  }

// TODO
  let input = "
// simple type alias
pub type Fields =
  //$ derive json decode encode
  Dict(String, String)

// recursive type aliases
pub type Foo =
  //$ derive json decode encode
  Bar
pub type Bar =
  //$ derive json decode encode
  String

pub type Listy(t) =
  //$ derive json decode encode
  List(t)
  " |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

// simple type alias
pub type Fields =
  //$ derive json decode encode
  Dict(String, String)

// recursive type aliases
pub type Foo =
  //$ derive json decode encode
  Bar
pub type Bar =
  //$ derive json decode encode
  String

pub type Listy(t) =
  //$ derive json decode encode
  List(t)

pub fn decoder_fields() -> Decoder(Fields) {
  decode.dict(decode.string, decode.string)
}

pub fn encode_fields(value: Fields) -> Json {
  json.dict(value, string.inspect, json.string)
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.string
}

pub fn encode_foo(value: Foo) -> Json {
  encode_bar(value)
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.string
}

pub fn encode_bar(value: Bar) -> Json {
  json.string(value)
}

pub fn decoder_listy(decoder_t: Decoder(t)) -> Decoder(Listy(t)) {
  decode.list(decoder_t)
}

pub fn encode_listy(value: Listy(t), encode_t: fn(t) -> Json) -> Json {
  json.array(value, encode_t)
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
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_decoder_non_string_keyed_dict_test() {
  let module_reader: types.ModuleReader = fn(ident) {
    case ident {
      // "project/asdf/foo" -> {
      //   let foo_src = "
// pub type Field(t) {
  // Field(
    // id: String,
    // value: t,
  // )
// }
      //   " |> string.trim

      //   let parse = fn(src) {
      //     glance.module(src)
      //     |> result.map_error(types.GlanceErr)
      //   }

      //   parse(foo_src)
      // }

      _ -> {
        io.debug(ident)
        panic as "`module_reader` miss in `parameterized_type_test`"
      }
    }
  }

// TODO
  let input = "
pub type IntKeyDict =
  //$ derive json decode
  Dict(Int, String)

pub type FloatKeyDict =
  //$ derive json decode
  Dict(Float, String)

pub type BoolKeyDict =
  //$ derive json decode
  Dict(Bool, String)
  " |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}

pub type IntKeyDict =
  //$ derive json decode
  Dict(Int, String)

pub type FloatKeyDict =
  //$ derive json decode
  Dict(Float, String)

pub type BoolKeyDict =
  //$ derive json decode
  Dict(Bool, String)

pub fn decoder_int_key_dict() -> Decoder(IntKeyDict) {
  decode.dict(util.decoder_int_string(), decode.string)
}

pub fn decoder_float_key_dict() -> Decoder(FloatKeyDict) {
  decode.dict(util.decoder_float_string(), decode.string)
}

pub fn decoder_bool_key_dict() -> Decoder(BoolKeyDict) {
  decode.dict(util.decoder_bool_string(), decode.string)
}
  "
  |> string.trim

  let files = [ File(module: "deriv/example/foo", src: input, idx: Some(1)) ]

  let assert [write] =
    files
    |> deriv.gen_derivs(module_reader)
    |> deriv.build_writes

  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_encode_nested_parameterized_type_alias_test() {
//pub type Fields(t) =
//  //$ derive json encode decode
//  Dict(String, Field(t))

//pub type Field(t) {
//  //$ derive json encode decode
//  Field(
//    id: String,
//    touched: Bool,
//    value: t,
//  )
//}

// TODO
  let input = "
pub type Fields(t) =
  Dict(String, Field(t))

pub type Field(t) {
  Field(
    id: String,
    touched: Bool,
    value: t,
  )
}

pub type Form {
  //$ derive json encode decode
  Form(
    text_fields: Fields(String),
    list_fields: Fields(List(String)),
    override: Fields(String),
    //$ json decoder some_specific_decoder_name
    //$ json encode some_specific_encode_func
  )
}
  " |> string.trim

//pub type Fields(t) =
//  //$ derive json encode decode
//  Dict(String, Field(t))

//pub type Field(t) {
//  //$ derive json encode decode
//  Field(
//    id: String,
//    touched: Bool,
//    value: t,
//  )
//}

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Fields(t) =
  Dict(String, Field(t))

pub type Field(t) {
  Field(
    id: String,
    touched: Bool,
    value: t,
  )
}

pub type Form {
  //$ derive json encode decode
  Form(
    text_fields: Fields(String),
    list_fields: Fields(List(String)),
    override: Fields(String),
    //$ json decoder some_specific_decoder_name
    //$ json encode some_specific_encode_func
  )
}

pub fn encode_form(value: Form) -> Json {
  case value {
    Form(..) as value ->
      json.object([
        #(\"text_fields\", encode_fields(value.text_fields, json.string)),
        #(
          \"list_fields\",
          encode_fields(value.list_fields, json.array(_, json.string)),
        ),
        #(\"override\", encode_fields(value.override, some_specific_encode_func)),
      ])
  }
}

pub fn decoder_form() -> Decoder(Form) {
  decode.one_of(decoder_form_form(), [])
}

pub fn decoder_form_form() -> Decoder(Form) {
  use text_fields <- decode.field(\"text_fields\", decoder_fields(decode.string))
  use list_fields <- decode.field(
    \"list_fields\",
    decoder_fields(decode.list(decode.string)),
  )
  use override <- decode.field(\"override\", some_specific_decoder_name())
  decode.success(Form(text_fields:, list_fields:, override:))
}
  "
  |> string.trim

  input |> should_equal(output:)
}

pub fn json_encode_nested_parameterized_type_alias_list_test() {
  let input = "
pub type Validation(t) = fn(t) -> Result(t, List(String))

pub type Field1(t) {
  //$ derive json encode decode
  Field1(
    id: String,
    validations: List(Validation(t)),
    //$ json decoder decoder_fake_validation
    //$ json encode encode_fake_validation
    touched: Bool,
    value: t,
    errs: List(String),
  )
}

pub fn encode_fake_validation(
  _value: Validation(t),
) -> Json {
  json.null()
}

pub fn decoder_fake_validation() -> Decoder(Validation(t)) {
  decode.success(fn(_) { Error([]) })
}
  " |> string.trim

 let output = "
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list

pub type Validation(t) = fn(t) -> Result(t, List(String))

pub type Field1(t) {
  //$ derive json encode decode
  Field1(
    id: String,
    validations: List(Validation(t)),
    //$ json decoder decoder_fake_validation
    //$ json encode encode_fake_validation
    touched: Bool,
    value: t,
    errs: List(String),
  )
}

pub fn encode_fake_validation(
  _value: Validation(t),
) -> Json {
  json.null()
}

pub fn decoder_fake_validation() -> Decoder(Validation(t)) {
  decode.success(fn(_) { Error([]) })
}

pub fn encode_field1(value: Field1(t), encode_t: fn(t) -> Json) -> Json {
  case value {
    Field1(..) as value ->
      json.object([
        #(\"id\", json.string(value.id)),
        #(\"validations\", json.array(value.validations, encode_fake_validation)),
        #(\"touched\", json.bool(value.touched)),
        #(\"value\", encode_t(value.value)),
        #(\"errs\", json.array(value.errs, json.string)),
      ])
  }
}

pub fn decoder_field1(decoder_t: Decoder(t)) -> Decoder(Field1(t)) {
  decode.one_of(decoder_field1_field1(decoder_t), [])
}

pub fn decoder_field1_field1(decoder_t: Decoder(t)) -> Decoder(Field1(t)) {
  use id <- decode.field(\"id\", decode.string)
  use validations <- decode.field(
    \"validations\",
    decode.list(decoder_fake_validation()),
  )
  use touched <- decode.field(\"touched\", decode.bool)
  use value <- decode.field(\"value\", decoder_t)
  use errs <- decode.field(\"errs\", decode.list(decode.string))
  decode.success(Field1(id:, validations:, touched:, value:, errs:))
}
  "
  |> string.trim

  input |> should_equal(output:)
}

pub type Validation(t) = fn(t) -> Result(t, List(String))

pub type Field1(t) {
  //$ derive json encode decode
  Field1(
    id: String,
    validations: List(Validation(t)),
    //$ json decoder decoder_validations
    //$ json encode encode_validations
    touched: Bool,
    value: t,
    errs: List(String),
  )
}

pub fn encode_fake_validation(
  _value: Validation(t),
) -> Json {
  json.null()
}

pub fn decoder_fake_validation() -> Decoder(Validation(t)) {
  decode.success(fn(_) { Error([]) })
}

pub fn encode_field1(value: Field1(t), encode_t: fn(t) -> Json) -> Json {
  case value {
    Field1(..) as value ->
      json.object([
        #("id", json.string(value.id)),
        #(
          "validations",
          json.array(value.validations, encode_fake_validation),
        ),
        #("touched", json.bool(value.touched)),
        #("value", encode_t(value.value)),
        #("errs", json.array(value.errs, json.string)),
      ])
  }
}

pub fn decoder_field1(decoder_t: Decoder(t)) -> Decoder(Field1(t)) {
  decode.one_of(decoder_field1_field1(decoder_t), [])
}

pub fn decoder_field1_field1(decoder_t: Decoder(t)) -> Decoder(Field1(t)) {
  use id <- decode.field("id", decode.string)
  use validations <- decode.field(
    "validations",
    decode.list(decoder_fake_validation()),
  )
  use touched <- decode.field("touched", decode.bool)
  use value <- decode.field("value", decoder_t)
  use errs <- decode.field("errs", decode.list(decode.string))
  decode.success(Field1(id:, validations:, touched:, value:, errs:))
}

pub type Fields(t) =
  //$ derive json encode decode
  Dict(String, Field(t))

pub type Field(t) {
  //$ derive json encode decode
  Field(
    id: String,
    touched: Bool,
    value: t,
  )
}

pub type Form {
  //$ derive json encode decode
  Form(
    text_fields: Fields(String),
    list_fields: Fields(List(String)),
  )
}

pub fn encode_fields(value: Fields(t), encode_t: fn(t) -> Json) -> Json {
  json.dict(value, string.inspect, encode_field(_, encode_t))
}

pub fn decoder_fields(decoder_t: Decoder(t)) -> Decoder(Fields(t)) {
  decode.dict(decode.string, decoder_field(decoder_t))
}

pub fn encode_field(value: Field(t), encode_t: fn(t) -> Json) -> Json {
  case value {
    Field(..) as value ->
      json.object([
        #("id", json.string(value.id)),
        #("touched", json.bool(value.touched)),
        #("value", encode_t(value.value)),
      ])
  }
}

pub fn decoder_field(decoder_t: Decoder(t)) -> Decoder(Field(t)) {
  decode.one_of(decoder_field_field(decoder_t), [])
}

pub fn decoder_field_field(decoder_t: Decoder(t)) -> Decoder(Field(t)) {
  use id <- decode.field("id", decode.string)
  use touched <- decode.field("touched", decode.bool)
  use value <- decode.field("value", decoder_t)
  decode.success(Field(id:, touched:, value:))
}

pub fn encode_form(value: Form) -> Json {
  case value {
    Form(..) as value ->
      json.object([
        #("text_fields", encode_fields(value.text_fields, json.string)),
        #("list_fields", encode_fields(value.list_fields, json.array(_, json.string))),
      ])
  }
}

pub fn decoder_form() -> Decoder(Form) {
  decode.one_of(decoder_form_form(), [])
}

pub fn decoder_form_form() -> Decoder(Form) {
  use text_fields <- decode.field(
    "text_fields",
    decoder_fields(decode.string),
  )
  use list_fields <- decode.field(
    "list_fields",
    decoder_fields(decode.list(decode.string)),
  )
  decode.success(Form(text_fields:, list_fields:))
}


fn should_equal(
  input input: String,
  output output: String,
) {
  let assert [write] = gen_and_build_writes(input)

  io.println("")
  io.println("")
  io.println("EXPECTED")
  io.println(output)
  io.println("")
  io.println("")
  io.println("GENERATED")
  io.println(write.src)
  io.println("DIFF (<expected >generated)")
  io.println(util.diff(output, write.src))

  write.src
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
        panic as { "`dummy_module_reader` miss for ident: " <> ident }
    }
  }
}
