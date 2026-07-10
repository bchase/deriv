import gleam/bool
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
import gleam/io
import simplifile
import examples/json_rewrite/after as json_example
import bchase/list.{at as list_at} as _
import deriv/internal/gen
//
import gleam/otp/actor
import gleam/otp/supervision
import gleam/otp/static_supervisor as supervisor
import gleam/erlang/process.{type Subject, type Selector}
import filespy
import gleam/crypto
import gleam/bit_array as ba

//

// fn check_gen_variant(
//   str: String,
//   from: gen.GleamPath,
//   get_type: fn(String) -> Result(g.CustomType, Nil),
// ) -> Result(Ref, Nil) {
//   let assert Ok(whitespace) = "\\s+" |> re.from_string

//   case str |> re.split(whitespace, _) {
//     ["gen:" <> _gen, "variant:" <> target, ..] ->
//       case target |> string.split(".") {
//         [module, type_] -> {
//           use module <- try_fail(gen.parse_gleam_module_path(module), Nil)
//           Ok(Ref(from:, to: module, type_:))
//         }

//         _ ->
//           Error(Nil)
//       }

//     _ ->
//       Error(Nil)
//   }
// }

// const checkers = [
//   check_gen_variant,
// ]

// fn parse_magic_comment_contents(
//   src src: String,
// ) -> List(String) {
//   let assert Ok(magic_comment_re) =
//     "[/][/][\\$]\\s*(.+)" |> re.from_string

//   src
//   |> re.scan(magic_comment_re, _)
//   |> list.filter_map(fn(match) {
//     case match.submatches {
//       [Some(str)] -> Ok(str)
//       _ -> Error(Nil)
//     }
//   })
// }

// fn modules_affected_by_change_to(
//   file file: gen.GleamFile,
//   x x: X,
// ) -> #(X, List(gen.GleamPath)) {
//   let path = file.path

//   let get_type = todo

//   // parse magic comments
//   let magic_comment_strs =
//     parse_magic_comment_contents(file.src)

//   // curr file (`path`) refs to types in other modules
//   let refs: List(Ref) =
//     magic_comment_strs
//     |> list.flat_map(fn(str) {
//       build_refs(str:, from: path, checkers:, get_type:)
//     })
//     |> list.unique

//   // track curr file ref changes
//   let x = X(refs: x.refs |> dict.insert(path, refs))

//   // get paths for all affected modules, i.e. modules ref'ing `path`
//   let affected =
//     refs
//     |> list.map(fn(ref) {
//       x.refs
//       |> dict.get(path)
//       |> result.unwrap([])
//     })
//     |> list.flatten
//     |> list.map(fn(ref) { ref.from })
//     |> list.unique

//   #(x, affected)
// }

// fn build_refs(
//   str str: String,
//   from from: gen.GleamPath,
//   checkers checkers: List(
//     fn(
//       String,
//       gen.GleamPath,
//       fn(String) -> Result(g.CustomType, Nil),
//     ) -> Result(Ref, Nil)
//   ),
//   get_type get_type: fn(String) -> Result(g.CustomType, Nil),
// ) -> List(Ref) {
//   checkers
//   |> list.filter_map(fn(check) { check(str, from, get_type) })
//   |> list.unique
// }


// pub type X {
//   X(
//     refs: Dict(gen.GleamPath, List(Ref)),
//   )
// }

// pub type Ref {
//   Ref(
//     from: gen.GleamPath,
//     to: gen.GleamPath,
//     type_: String,
//   )
// }

// pub fn dependencies_for(
//   changed changed: gen.GleamPath,
//   x x: X,
// ) -> List(Ref) {
//   dependencies_for_(changed:, x:, depth: 0)
// }

// fn dependencies_for_(
//   changed changed: gen.GleamPath,
//   x x: X,
//   depth depth: Int,
// ) -> List(Ref) {
//   x.refs
//   |> dict.get(changed)
//   |> result.unwrap([])
//   |> list.fold([], fn(acc, ref) {
//     acc
//     |> list.append([ref])
//     |> list.append(dependencies_for_(ref.to, x, depth + 1))
//   })
// }


//

fn worker(
  actor actor: actor.Builder(state, msg, return),
) -> supervision.ChildSpecification(return) {
  supervision.worker(fn() { actor.start(actor) })
}

fn actor(
  init init: fn(flags, Subject(msg)) -> #(state, Selector(msg)),
  update update: fn(state, msg) -> actor.Next(state, msg),
  timeout timeout: Int,
  name name: process.Name(msg),
  return return: fn(state) -> return,
  flags flags: flags,
) -> actor.Builder(state, msg, return) {
  actor.new_with_initialiser(timeout, fn(self) {
    let #(state, sel) = init(flags, self)

    state
    |> actor.initialised
    |> actor.selecting(sel |> process.select(self))
    |> actor.returning(return(state))
    |> Ok
  })
  |> actor.named(name)
  |> actor.on_message(update)
}

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

type ApiReq {
  ListPeople
  GetPerson(id: String)
}

// fn server(
//   req req: ApiReq,
// ) -> Json {
//   { //$ gen variant pkg/mod.gen req
//   }
// }

fn gen_variant(
) -> Dict(String, fn(String) -> g.Expression) {
  [
    #("deriv/gen.test1", gen.test1),
    #("deriv/gen.test2", gen.test2),
  ] |> dict.from_list
}

// pieces
// X - `VariantField` decoders & expression builders (`bchase/deriv`)
// \ - splice in code get for `//$ gen` magic comments
//   - look up expr func based on `Gen.str`
//   - look up `glance.CustomType` for func param
//   - map `Gen` to expression builder
//   - ...
//
// TODO
//   next
//     - handle multiple `//$ gen` in single file
//     - allow gleam before block, e.g. `let foo = { //$ gen ...`
//     - handle new case (not replace)
//     - ensure AST wrapped in `g.Block`
//   tidy
//     - refactor
//     - clean up tests
//     - define module structure
//     - move to own module

pub fn gen_test() {
  let assert Ok(file) = gen.load_gleam_file("./test/gen/before.gleam")

  let output = gen.process(file:)

  let assert Ok(after) = simplifile.read("./test/gen/after.gleam")

  log_("AFTER", after)
  log_("OUTPUT", output)

  output
  |> should.equal(after)

  Nil
}

type Config {
  Config(
    names: Names,
  )
}

type Names {
  Names(
    app: process.Name(Msg),
    lookup: process.Name(LookupMsg),
  )
}

fn gen_supervisor(
  names names: Names,
) -> supervisor.Builder {
  supervisor.new(supervisor.OneForOne)
  |> supervisor.add(filespy_worker(notify: names.app))
  |> supervisor.add(worker(lookup_actor(name: names.lookup)))
  |> supervisor.add(worker(app_actor(name: names.app, cfg: AppConfig(lookup: names.lookup))))
}

type Msg {
  NoOp
  GotFileChange(change: filespy.Change(Nil))
  ProcessQueue
  Process(path: String)
}

type State {
  State(
    self: Subject(Msg),
    cfg: AppConfig,
    queue: Set(String),
    hashes: Dict(String, BitArray),
  )
}

type AppConfig {
  AppConfig(
    lookup: process.Name(LookupMsg),
  )
}

fn app_actor(
  name name: process.Name(Msg),
  cfg cfg: AppConfig,
) -> actor.Builder(State, Msg, Nil) {
  actor(init:, update:, timeout: 100, name:, return: always(Nil), flags: cfg)
}

fn init(
  cfg cfg: AppConfig,
  self self: Subject(Msg),
) -> #(State, Selector(Msg)) {
  State(
    self:,
    cfg:,
    queue: set.new(),
    hashes: dict.new(),
  )
  |> pair.new(process.new_selector())
}

fn sha256_hash(
  str str: String
) -> BitArray {
  str
  |> ba.from_string
  |> crypto.hash(crypto.Sha256, _)
}

fn update(
  state state: State,
  msg msg: Msg,
) -> actor.Next(State, Msg) {
  let State(self:, ..) = state
  let lookup = state.cfg.lookup |> process.named_subject

  case msg {
    NoOp |
    GotFileChange(change: filespy.Custom(Nil)) ->
      actor.continue(state)

    GotFileChange(change: filespy.Change(path:, ..)) -> {
      let ok = Ok(actor.continue(state))

      use <- bool.lazy_guard(path |> string.ends_with("gleam.toml"), fn() {
        process.send(lookup, ReloadGleamToml)
        ok
      })

      use <- bool.lazy_guard(path |> string.contains("/build/packages/"), fn() {
        process.send(lookup, ReloadFilepaths)
        ok
      })

      use <- bool.guard(!{path |> string.ends_with(".gleam")} , ok)

      use src <- try_fail_(simplifile.read(path), fn(_) {
        io.println_error("Failed to read Gleam file: " <> path)
        Error(Nil)
      })

      let hash = sha256_hash(src)

      use <- bool.guard({ state.hashes |> dict.get(path) } == Ok(hash), ok)

      process.send_after(self, 1000, ProcessQueue)

      Ok(actor.continue(State(..state, queue: state.queue |> set.insert(path), hashes: state.hashes |> dict.insert(path, hash))))
    } |> result.unwrap(actor.continue(state))

    Process(path:) -> {
      case gen.load_gleam_file(filepath: path) {
        Error(err) -> {
          io.println_error([
            "Failed to load Gleam file at path: " <> path,
            "  " <> string.inspect(err)
          ] |> string.join("\n"))

          actor.continue(state)
        }

        Ok(file) -> {
          let new = gen.process(file:)

          let hash = sha256_hash(new)

          case simplifile.write(path, new) {
            Ok(Nil) ->
              Nil

            Error(err) ->
              io.println_error([
                "Failed to write new Gleam file to path: " <> path,
                "  " <> string.inspect(err)
              ] |> string.join("\n"))
          }

          actor.continue(State(..state, hashes: state.hashes |> dict.insert(path, hash)))
        }
      }
    }

    ProcessQueue -> {
      state.queue
      |> set.each(fn(path) {
        process.send(state.self, Process(path:))
      })

      actor.continue(State(..state, queue: set.new()))
    }
  }
}

//

fn filespy_worker(
  notify notify: process.Name(Msg),
) -> supervision.ChildSpecification(Subject(filespy.Change(Nil))) {
  supervision.worker(fn() {
    filespy.new()
    |> filespy.set_initial_state(Nil)
    |> filespy.add_dir(".")
    |> filespy.set_actor_handler(fn(state, msg) {
      case msg {
        filespy.Change(..) as change -> {
          notify
          |> process.named_subject
          |> process.send(GotFileChange(change:))

          actor.continue(state)
        }

        filespy.Custom(..) ->
          actor.continue(state)
      }
    })
    |> filespy.start
  })
}

type LookupState {
  LookupState(
    self: Subject(LookupMsg),
    pwd: gen.Pwd,
    toml: gen.GleamToml,
    filepaths: List(String),
    changes: Dict(String, List(filespy.Change(Nil))),
  )
}

pub opaque type LookupMsg {
  LookupNoOp
  ReloadGleamToml
  ReloadFilepaths
  //
  Type(
    mod: Option(String),
    name: String,
    file: gen.GleamFile,
    reply: Subject(Result(g.CustomType, gen.GenErr)),
  )
}

fn lookup_actor(
  name name: process.Name(LookupMsg),
) -> actor.Builder(LookupState, LookupMsg, Nil) {
  let load_pwd = fn() {
    let assert Ok(pwd) = gen.pwd()
      as "`gen` failed to get `pwd`"
    pwd
  }

  let load_toml = fn() {
    let assert Ok(toml) = gen.gleam_toml()
      as "`gen` failed to load `gleam.toml` in pwd"
    toml
  }

  let load_filepaths = fn() {
    let assert Ok(filepaths) = gen.all_build_package_gleam_src_filepaths()
      as "`gen` failed to load `build/packages/**/src/**/*.gleam` paths"
    filepaths
  }

  actor.new_with_initialiser(100, fn(self) {
    LookupState(
      self:,
      pwd: load_pwd(),
      toml: load_toml(),
      filepaths: load_filepaths(),
      changes: dict.new(),
    )
    |> actor.initialised
    |> Ok
  })
  |> actor.named(name)
  |> actor.on_message(fn(state, msg) {
    case msg {
      LookupNoOp ->
        actor.continue(state)

      ReloadGleamToml ->
        actor.continue(LookupState(..state, toml: load_toml()))

      ReloadFilepaths ->
        actor.continue(LookupState(..state, filepaths: load_filepaths()))

      //

      Type(mod:, name: type_, file:, reply:) -> {
        gen.Context(pwd: state.pwd, toml: state.toml, file:)
        |> gen.get_custom_type(ctx: _, mod:, type_:)
        |> process.send(reply, _)

        actor.continue(state)
      }
    }
  })
}

const lookup_timeout_ms = 5_000

fn look_up_type(
  actor actor: process.Name(LookupMsg),
  mod mod: Option(String),
  name name: String,
  file file: gen.GleamFile,
) -> Result(glance.CustomType, gen.GenErr) {
  let self = process.new_subject()

  actor
  |> process.named_subject
  |> actor.send(Type(mod:, name:, file:, reply: self))

  process.receive(self, lookup_timeout_ms)
  |> result.replace_error(gen.Failed("lookup timed out (" <> string.inspect(actor) <> ")"))
  |> result.flatten
}

fn build_config() -> Config {
  Config(
    names: Names(
      app: process.new_name("deriv-app"),
      lookup: process.new_name("deriv-type-ast-lookup"),
    )
  )
}

pub fn custom_type_lookup_test() {
  let cfg = build_config()

  let assert Ok(_) = supervisor.start(gen_supervisor(cfg.names))

  // process.sleep_forever()

  let assert Ok(file) = gen.load_gleam_file( "src/deriv/internal/dummy/lookup.gleam")

  // echo modules_affected_by_change_to(file:, x: X(refs: dict.new())) |> pair.second

  [
    // curr package
    look_up_type(cfg.names.lookup, file:, mod: None, name: "Local"),
    look_up_type(cfg.names.lookup, file:, mod: None, name: "LocalAlias"),
    look_up_type(cfg.names.lookup, file:, mod: None, name: "OtherImport"),
    look_up_type(cfg.names.lookup, file:, mod: Some("lookup_other"), name: "Other"),
    look_up_type(cfg.names.lookup, file:, mod: Some("oo"), name: "OtherOther"),
    // dep
    look_up_type(cfg.names.lookup, file:, mod: Some("glance"), name: "Span"),
    // dep at path
    look_up_type(cfg.names.lookup, file:, mod: Some("id"), name: "Id"),
    // dep package name doesn't match module name
    look_up_type(cfg.names.lookup, file:, mod: Some("option"), name: "Option"),
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
