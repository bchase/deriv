import gleam/option.{type Option, Some, None}
import gleam/dict
import gleam/result
import gleam/list
import gleeunit
import gleeunit/should
import gleam/string
import deriv/types.{type File, File, DerivFieldOpt, DerivField}
import deriv/parser
import deriv
import deriv/util
import gleam/io
import deriv/example/wisp_form_data as form
// import deriv/example/unify

import glance.{Import, UnqualifiedImport, Named}
import simplifile

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
import decode.{type Decoder}
import deriv/util
import gleam/json.{type Json}
import gleam/list
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

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of([decoder_foo_foo()])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  decode.into({
    use uuid <- decode.parameter
    use id <- decode.parameter
    use name <- decode.parameter
    use active <- decode.parameter
    use ratio <- decode.parameter
    use words <- decode.parameter
    use maybe_list <- decode.parameter
    Foo(uuid:, id:, name:, active:, ratio:, words:, maybe_list:)
  })
  |> decode.field(\"uuid\", util.decoder_uuid())
  |> decode.field(\"int_id\", decode.int)
  |> decode.field(\"name\", decode.string)
  |> decode.field(\"active\", decode.bool)
  |> decode.field(\"ratio\", decode.float)
  |> decode.field(\"words\", decode.list(decode.string))
  |> decode.field(\"maybe_list\", decode.optional(decode.list(decode.string)))
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
        #(\"words\", json.preprocessed_array(list.map(value.words, json.string))),
        #(
          \"maybe_list\",
          json.nullable(value.maybe_list, json.array(_, json.string)),
        ),
      ])
  }
}

pub fn decoder_bar() -> Decoder(Bar) {
  decode.one_of([decoder_bar_bar()])
}

pub fn decoder_bar_bar() -> Decoder(Bar) {
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

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

pub fn json_multi_variant_type_test() {
  // let input = "
// pub type T {
  // X(foo: String)
  // Y(bar: Int)
// } //$ derive json(decode,encode)
  // "
  // |> string.trim

 // let output = "
// import gleam/json.{type Json}

// pub type T {
  // X(foo: String)
  // Y(bar: Int)
// } //$ derive json(decode,encode)

// pub fn encode_t(value: T) -> Json {
  // case value {
  //   X(..) as value -> json.object([#(\"foo\", json.string(value.foo))])
  //   Y(..) as value -> json.object([#(\"bar\", json.int(value.bar))])
  // }
// }
  // "
  // |> string.trim
  let input = "
pub type T {
  //$ derive json decode encode
  X(foo: String)
  Y(bar: Int)
}
  "
  |> string.trim

 let output = "
import decode.{type Decoder}
import gleam/json.{type Json}

pub type T {
  //$ derive json decode encode
  X(foo: String)
  Y(bar: Int)
}

pub fn decoder_t() -> Decoder(T) {
  decode.one_of([decoder_t_x(), decoder_t_y()])
}

pub fn decoder_t_x() -> Decoder(T) {
  decode.into({
    use foo <- decode.parameter
    X(foo:)
  })
  |> decode.field(\"foo\", decode.string)
}

pub fn decoder_t_y() -> Decoder(T) {
  decode.into({
    use bar <- decode.parameter
    Y(bar:)
  })
  |> decode.field(\"bar\", decode.int)
}

pub fn encode_t(value: T) -> Json {
  case value {
    X(..) as value -> json.object([#(\"foo\", json.string(value.foo))])
    Y(..) as value -> json.object([#(\"bar\", json.int(value.bar))])
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
import decode.{type Decoder}
import gleam/json.{type Json}

pub type Maybe {
  //$ derive json decode encode
  Maybe(
    name: Option(String),
  )
}

pub fn decoder_maybe() -> Decoder(Maybe) {
  decode.one_of([decoder_maybe_maybe()])
}

pub fn decoder_maybe_maybe() -> Decoder(Maybe) {
  decode.into({
    use name <- decode.parameter
    Maybe(name:)
  })
  |> decode.field(\"name\", decode.optional(decode.string))
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

pub fn gleam_format_magic_comment_parsing_test() {
  // //$ json(baz(boo))
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
import decode.{type Decoder}
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
  decode.one_of([decoder_a_a()])
}

pub fn decoder_a_a() -> Decoder(A) {
  decode.into({
    use b <- decode.parameter
    A(b:)
  })
  |> decode.field(\"b\", decoder_b())
}

pub fn encode_a(value: A) -> Json {
  case value {
    A(..) as value -> json.object([#(\"b\", encode_b(value.b))])
  }
}

pub fn decoder_b() -> Decoder(B) {
  decode.one_of([decoder_b_b()])
}

pub fn decoder_b_b() -> Decoder(B) {
  decode.into({
    use x <- decode.parameter
    B(x:)
  })
  |> decode.field(\"x\", decode.string)
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
import decode.{type Decoder}
import deriv/util
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
  decode.one_of([decoder_date_time_examples_date_time_examples()])
}

pub fn decoder_date_time_examples_date_time_examples() -> Decoder(
  DateTimeExamples,
) {
  decode.into({
    use t0 <- decode.parameter
    use t1 <- decode.parameter
    use t2 <- decode.parameter
    use t3 <- decode.parameter
    use t4 <- decode.parameter
    use t5 <- decode.parameter
    use t6 <- decode.parameter
    DateTimeExamples(t0:, t1:, t2:, t3:, t4:, t5:, t6:)
  })
  |> decode.field(\"t0\", util.decoder_birl_parse())
  |> decode.field(\"t1\", util.decoder_birl_parse())
  |> decode.field(\"t2\", util.decoder_birl_from_naive())
  |> decode.field(\"t3\", util.decoder_birl_from_http())
  |> decode.field(\"t4\", util.decoder_birl_from_unix())
  |> decode.field(\"t5\", util.decoder_birl_from_unix_milli())
  |> decode.field(\"t6\", util.decoder_birl_from_unix_micro())
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
import decode.{type Decoder}
import gleam/json.{type Json}

pub type DictFieldType {
  //$ derive json decode encode
  DictFieldType(
    dict: Dict(String, Int),
  )
}

pub fn decoder_dict_field_type() -> Decoder(DictFieldType) {
  decode.one_of([decoder_dict_field_type_dict_field_type()])
}

pub fn decoder_dict_field_type_dict_field_type() -> Decoder(DictFieldType) {
  decode.into({
    use dict <- decode.parameter
    DictFieldType(dict:)
  })
  |> decode.field(\"dict\", decode.dict(decode.string, decode.int))
}

pub fn encode_dict_field_type(value: DictFieldType) -> Json {
  case value {
    DictFieldType(..) as value ->
      json.object([#(\"dict\", json.dict(value.dict, fn(str) { str }, json.int))])
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
import decode.{type Decoder}
import gleam/json.{type Json}

pub type Unnested {
  //$ derive json decode encode
  Unnested(
    unnested: Int, //$ json named foo.bar.baz
  )
}

pub fn decoder_unnested() -> Decoder(Unnested) {
  decode.one_of([decoder_unnested_unnested()])
}

pub fn decoder_unnested_unnested() -> Decoder(Unnested) {
  decode.into({
    use unnested <- decode.parameter
    Unnested(unnested:)
  })
  |> decode.subfield([\"foo\", \"bar\", \"baz\"], decode.int)
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

  write.filepath
  |> should.equal("src/deriv/example/foo.gleam")

  write.src
  |> should.equal(output)
}

fn get_invariant(module: glance.Module, type_name: String) -> glance.Variant {
  let assert Ok(t) =
    module.custom_types
    |> list.find_map(fn(t) {
      case t.definition.name == type_name {
        True -> Ok(t.definition)
        False -> Error(Nil)
      }
    })

  let assert [v]: List(glance.Variant) = t.variants

  v
}

type FormDecoder {
  FormDecoder(
    type_: glance.CustomType,
    lines: List(DecodeLine),
  )
}

type DecodeLine {
  DecodeBaseLine(
    field_name: String,
    input_name: String,
    decoder: glance.Expression,
    kind: BaseKind,
  )
  DecodeCustomLine(
    field_name: String,
    type_: glance.Type,
    lines: List(DecodeLine),
  )
  DecodeCustomOptionLine(
    field_name: String,
    type_: glance.Type,
    lines: List(DecodeLine),
  )
}

type BaseKind {
  Scalar
  ScalarOption
  Array
}

fn decoder_string() -> glance.Expression {
  glance.Call(
    function: glance.FieldAccess(glance.Variable("form"), "decoder_string()"),
    arguments: [],
  )
}

fn decoder_int() -> glance.Expression {
  glance.Call(
    function: glance.FieldAccess(glance.Variable("form"), "decoder_int()"),
    arguments: [],
  )
}

fn decoder_float() -> glance.Expression {
  glance.Call(
    function: glance.FieldAccess(glance.Variable("form"), "decoder_float()"),
    arguments: [],
  )
}

fn decoder_bool() -> glance.Expression {
  glance.Call(
    function: glance.FieldAccess(glance.Variable("form"), "decoder_bool()"),
    arguments: [],
  )
}

fn dls() -> List(DecodeLine) {
  let pet_type = todo

  [
    // use name <- result.try(scalar("person[name]", decode.string, vals))
    DecodeBaseLine("name", "person[name]", decoder_string(), Scalar),
    // use age <- result.try(scalar("person[age]", decoder_int(), vals))
    DecodeBaseLine("age", "person[age]", decoder_int(), Scalar),
    // let kd = option.from_result(scalar("person[kd]", decoder_float(), vals))
    DecodeBaseLine("kd", "person[kd]", decoder_float(), ScalarOption),
    // use active <- result.try(scalar("person[active]", decoder_bool(), vals))
    DecodeBaseLine("active", "person[active]", decoder_bool(), Scalar),
    // use hobbies <- result.try(array("person[hobbies][]", decode.string, vals))
    DecodeBaseLine("hobbies", "person[hobbies][]", decoder_string(), Array),

    // use pet <- result.try({
    DecodeCustomLine("pet", pet_type, [
      // use name <- scalar_("person[pet][name]", decode.string)
      DecodeBaseLine("name", "person[pet][name]", decoder_string(), Scalar),
      // use age <- scalar_("person[name][age]", decode.int)
      DecodeBaseLine("age", "person[pet][age]", decoder_int(), Scalar),
    ]),

    // let pet = option.from_result({
    DecodeCustomOptionLine("pet", pet_type, [
      // use name <- scalar_("person[pet][name]", decode.string)
      DecodeBaseLine("name", "person[pet][name]", decoder_string(), Scalar),
      // use age <- scalar_("person[name][age]", decode.int)
      DecodeBaseLine("age", "person[pet][age]", decoder_int(), Scalar),
    ]),
  ]
}

fn get_base_type_decoder(type_name: String) -> Result(glance.Expression, Nil) {
  case type_name {
    "String" -> Ok(decoder_string())
    "Int" -> Ok(decoder_string())
    "Float" -> Ok(decoder_float())
    "Bool" -> Ok(decoder_bool())
    _ -> Error(Nil)
  }
}

fn build_names(module: glance.Module, type_name: String, prefix prefix: String) -> List(DecodeLine) {
  let v = get_invariant(module, type_name)

  list.flat_map(v.fields, fn(f) {
    case f {
      glance.LabelledVariantField(..) as f -> {
        let field_name = prefix <> "[" <> f.label <> "]"

        case f.item {
          glance.NamedType(..) as ft ->
            case ft.name, ft.parameters, get_base_type_decoder(ft.name) {
              // TODO `Option(List(t))`?
              "Option", [glance.NamedType(name:, ..) as pt], _ -> {
                case get_base_type_decoder(name) {
                  Ok(decoder) -> {
                    DecodeBaseLine(ft.name, field_name, decoder, Scalar)
                    |> list.wrap
                  }

                  _ -> {
                    DecodeCustomOptionLine(ft.name, pt, build_names(module, name, field_name))
                    |> list.wrap
                  }
                }
              }

              "List", [glance.NamedType(name:, ..)], _ -> {
                let field_name = field_name <> "[]"

                case get_base_type_decoder(name) {
                  Ok(decoder) -> {
                    DecodeBaseLine(ft.name, field_name, decoder, Scalar)
                    |> list.wrap
                  }

                  _ -> {
                    io.debug(v)
                    io.debug(ft)
                    panic as "[deriv][form] Supporting lists of non-base types are not yet implemented"
                  }
                }
              }

              _,_, Ok(decoder) -> {
                DecodeBaseLine(ft.name, field_name, decoder, Scalar)
                |> list.wrap
              }

              _, _, _ -> {
                let field_name = prefix <> "[" <> f.label <> "]"

                DecodeCustomOptionLine(ft.name, ft, build_names(module, ft.name, field_name))
                |> list.wrap
              }
            }

          _ -> {
            io.debug(v)
            io.debug(f)
            panic as "[deriv][form] Only `LabelledVariantField`s are supported"
          }
        }
      }

      _ -> {
        io.debug(f)
        panic as "[deriv][form] Only invariant `CustomType`s are supported"
      }
    }
  })
}


pub fn form_data_decode_test() {
  let assert Ok(src) = simplifile.read("src/deriv/example/wisp_form_data.gleam")
  let assert Ok(module) = glance.module(src)

  let assert Ok(f) = list.find(module.functions, fn(f) { f.definition.name == "decode_person_form" })

  io.debug(f)

  build_names(module, "PersonForm", prefix: "person")
  |> io.debug
  build_names(module, "Foo", prefix: "foo")
  |> io.debug

  let fields = [
    #("person[name]", "Brad"),
    #("person[age]", "38"),
    #("person[kd]", "1.87"),
    #("person[active]", "true"),
    #("person[hobbies][]", "Japanese"),
    #("person[hobbies][]", "Gleam"),
    #("person[hobbies][]", "Tarkov"),
    #("person[pet][name]", "Jovie"),
    #("person[pet][age]", "8"),
  ]

  form.decode_person_form(fields)
  |> should.equal(Ok(form.PersonForm(
    name: "Brad",
    age: 38,
    kd: Some(1.87),
    active: True,
    hobbies: ["Japanese", "Gleam", "Tarkov"],
    pet: Some(form.PetForm(
      name: "Jovie",
      age: 8,
    ))
  )))
}
