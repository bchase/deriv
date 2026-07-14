import shellout
import argv
import glint
import tom
import radiate
import gleam/option.{Some, type Option, None}
import gleam/bool
import gleam/set.{type Set}
import bchase/result.{try_fail_} as _
import bchase/function.{always}
import gleam/pair
import gleam/result
import gleam/list
import gleam/dict.{type Dict}
import gleam/string
import bchase/io.{log_err}
import simplifile
import deriv/internal/gen.{type Ref}
import gleam/otp/actor
import gleam/otp/supervision
import gleam/otp/static_supervisor as supervisor
import gleam/erlang/process.{type Subject, type Selector}
import gleam/crypto
import filespy
import gleam/bit_array as ba
import deriv/gen/types.{type TypeDef, type GleamPath, type GleamFile} as _
import deriv/gen/scan

pub fn main() -> Nil {
  glint.new()
  |> glint.with_name("gleam run -m deriv --")
  |> glint.add(at: [], do: cmd())
  |> glint.run(argv.load().arguments)
}

fn cmd() -> glint.Command(Nil) {
  use <- glint.command_help("Run `deriv` code gen watcher")

  use _named, _args, _flags <- glint.command()

  let cfg = build_config()
  let assert Ok(_) = supervisor.start(supervisor(names: cfg.names))

  process.sleep_forever()
}

//

pub type Config {
  Config(
    names: Names,
  )
}

pub type Names {
  Names(
    app: process.Name(Msg),
    lookup: process.Name(LookupMsg),
    refs: process.Name(RefsMsg),
    gens: process.Name(GensMsg),
  )
}

pub fn build_config() -> Config {
  Config(
    names: Names(
      app: process.new_name("deriv-app"),
      lookup: process.new_name("deriv-type-ast-lookup"),
      refs: process.new_name("deriv-refs-listener"),
      gens: process.new_name("deriv-code-gens-server"),
    )
  )
}

pub fn supervisor(
  names names: Names,
) -> supervisor.Builder {
  supervisor.new(supervisor.OneForOne)
  // |> supervisor.add(file_change_watching_worker(notify: names.app))
  |> supervisor.add(worker(gens_actor(name: names.gens)))
  |> supervisor.add(worker(lookup_actor(name: names.lookup)))
  |> supervisor.add(worker(refs_actor(name: names.refs)))
  |> supervisor.add(hot_code_reloading_worker(notify: names.app, gens: names.gens))
  |> supervisor.add(worker(app_actor(name: names.app, cfg: AppConfig(
    lookup: names.lookup,
    refs: names.refs,
  ))))
}

// APP ACTOR

pub opaque type Msg {
  NoOp
  GotCodeReload(path: String)
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
    refs: process.Name(RefsMsg),
  )
}

fn app_actor(
  name name: process.Name(Msg),
  cfg cfg: AppConfig,
) -> actor.Builder(State, Msg, Nil) {
  actor_named(name:, init:, sel: None, update:, timeout: 100, return: always(Nil), flags: cfg)
}

fn init(
  cfg cfg: AppConfig,
  self self: Subject(Msg),
) -> State {
  State(
    self:,
    cfg:,
    queue: set.new(),
    hashes: dict.new(),
  )
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
    GotFileChange(change: filespy.Custom(Nil)) |
    GotFileChange(change: filespy.Change(..)) -> {
      actor.continue(state)
    }

    // GotFileChange(change: filespy.Change(path:, ..)) -> {
    GotCodeReload(path:) -> {
      let noop = Ok(actor.continue(state))

      use <- bool.lazy_guard(path |> string.ends_with("gleam.toml"), fn() {
        // process.send(lookup, ReloadGleamToml)
        noop
      })

      use <- bool.lazy_guard(path |> string.contains("/build/packages/"), fn() {
        process.send(lookup, ReloadFilepaths)
        noop
      })

      use <- bool.guard(!{path |> string.ends_with(".gleam")} , noop)

      use src <- try_fail_(simplifile.read(path), fn(_) {
        io.println_error("Failed to read Gleam file: " <> path)
        Error(Nil)
      })

      let hash = sha256_hash(src)

      use <- bool.guard({ state.hashes |> dict.get(path) } == Ok(hash), noop)

      process.send_after(self, 1000, ProcessQueue)

      Ok(actor.continue(State(..state,
        queue: state.queue |> set.insert(path),
        hashes: state.hashes |> dict.insert(path, hash)),
      ))
    }
    |> result.unwrap(actor.continue(state))

    Process(path:) -> {
      use file <- try_fail_(gen.load_gleam_file(filepath: path), fn(err) {
        log_err([ "Failed to load Gleam file at path: " <> path, "  " <> string.inspect(err) ])
        Error(Nil)
      })

      use ctx <- try_fail_(fetch_context(state.cfg.lookup, file:), fn(err) {
        log_err([ "Failed fetch context: " <> string.inspect(err) ])
        Error(Nil)
      })

      use #(new, refs) <- try_fail_(gen.process(ctx:), fn(err) {
        case err {
          Ok(gen.Skip) ->
            Nil

          Error(err) -> {
            log_err([
              { "Code gen failed..." },
              { "  filepath: " <> file.filepath },
              { "  error: " <> string.inspect(err) },
            ])
          }
        }

        Error(Nil)
      })

      use <- bool.guard(new == file.src, Ok(actor.continue(state)))

      state.cfg.refs
      |> process.named_subject
      |> process.send(UpdateRefs(path: file.path, refs:))

      let hash = sha256_hash(new)

      use Nil <- try_fail_(simplifile.write(path, new), fn(err) {
        log_err([ "Failed to write code gen src to Gleam file: " <> path, string.inspect(err) ])
        Error(Nil)
      })

      Ok(actor.continue(State(..state, hashes: state.hashes |> dict.insert(path, hash))))
    }
    |> result.unwrap(actor.continue(state))

    ProcessQueue -> {
      state.queue
      |> set.each(fn(path) {
        process.send(state.self, Process(path:))
      })

      actor.continue(State(..state, queue: set.new()))
    }
  }
}

// GENS ACTOR

pub opaque type GensMsg {
  GensNoOp
  GensPostInit
  GensUpdateDirs(dirs: List(String))
}

type GensConfig {
  GensConfig(
  )
}

type GensState {
  GensState(
    cfg: GensConfig,
    self: Subject(GensMsg),
    gens: scan.ExprGenFuncs,
  )
}

fn gens_actor(
  name name: process.Name(GensMsg),
) -> actor.Builder(GensState, GensMsg, Nil) {
  let flags = GensConfig
  actor(init: gens_init, sel: None, update: gens_update, timeout: 100, return: always(Nil), flags:)
  |> actor.named(name)
}

fn gens_init(
  cfg: GensConfig,
  self: Subject(GensMsg),
) -> GensState {
  process.send(self, GensPostInit)

  GensState(
    cfg:,
    self:,
    gens: scan.empty_expr_gen_funcs(),
  )
}

fn gens_update(
  state: GensState,
  msg: GensMsg,
) -> actor.Next(GensState, b) {
  case msg {
    GensNoOp ->
      actor.continue(state)

    GensPostInit -> {
      actor.continue(state)
    }

    GensUpdateDirs(dirs:) -> {
      use find <- try_fail_(shellout.command(in: ".", opt: [],  run: "find", with: dirs), fn(_err) {
        Error(Nil)
      })

      let filepaths =
        find
        |> string.split("\n")
        |> list.map(string.trim)
        |> list.filter(string.ends_with(_, ".gleam"))

      let gens = scan.add_expr_gen_funcs(filepaths:, expr_gen_funcs: state.gens)

      scan.write_expr_gens_to_gleam_file(expr_gen_funcs: gens)

      Ok(actor.continue(GensState(..state, gens:)))
    }
    |> result.unwrap(actor.continue(state))
  }
}

// REFS ACTOR

pub opaque type RefsMsg {
  RefNoOp
  UpdateRefs(path: GleamPath, refs: List(Ref))
}

type RefState {
  RefState(
    self: Subject(RefsMsg),
    refs: Dict(GleamPath, List(Ref)),
  )
}

type RefConfig {
  RefConfig
}

fn refs_init(
  _cfg: RefConfig,
  self: Subject(RefsMsg),
) -> RefState {
  RefState(
    self:,
    refs: dict.new(),
  )
}

fn refs_update(
  state state: RefState,
  msg msg: RefsMsg,
) -> actor.Next(RefState, RefsMsg){
  case msg {
    RefNoOp ->
      actor.continue(state)

    UpdateRefs(path:, refs:) -> {
      actor.continue(RefState(..state,
        refs: state.refs |> dict.insert(path, refs)
      ))
    }
  }
}

fn refs_actor(
  name name: process.Name(RefsMsg),
) -> actor.Builder(RefState, RefsMsg, Nil) {
  let flags = RefConfig

  actor(init: refs_init, sel: None, update: refs_update, timeout: 100, return: always(Nil), flags:)
  |> actor.named(name)
}

// // FILE CHANGE WATCHING WORKER

// fn file_change_watching_worker(
//   notify notify: process.Name(Msg),
// ) -> supervision.ChildSpecification(Subject(filespy.Change(Nil))) {
//   supervision.worker(fn() {
//     filespy.new()
//     |> filespy.set_initial_state(Nil)
//     |> filespy.add_dir(".")
//     |> filespy.set_actor_handler(fn(state, msg) {
//       case msg {
//         filespy.Change(..) as change -> {
//           notify
//           |> process.named_subject
//           |> process.send(GotFileChange(change:))

//           actor.continue(state)
//         }

//         filespy.Custom(..) ->
//           actor.continue(state)
//       }
//     })
//     |> filespy.start
//   })
// }

// HOT CODE RELOADING ACTOR

fn hot_code_reloading_worker(
  notify notify: process.Name(Msg),
  gens gens: process.Name(GensMsg),
) -> supervision.ChildSpecification(Subject(filespy.Change(Nil))) {
  let assert [dir, ..dirs] as watched_dirs = watched_dirs() // TODO cache globally
    // |> fn(xs) {
    //   list.each(xs, io.println)
    //   xs
    // }

  supervision.worker(fn() {
    radiate.new()
    |> radiate.set_initializer(fn(self) {
      gens
      |> process.named_subject
      |> process.send(GensUpdateDirs(dirs: watched_dirs))

      Nil
      |> actor.initialised
      |> actor.returning(self)
      |> Ok
    })
    |> radiate.add_dir(dir)
    |> radiate.on_reload(fn(state, path) {
      notify
      |> process.named_subject
      |> process.send(GotCodeReload(path:))

      state
    })
    |> list.fold(dirs, _, radiate.add_dir)
    |> radiate.start_state
  })
}

fn watched_dirs(
) -> List(String) {
  let filepath = "gleam.toml"

  let assert Ok(gt) = gen.read_gleam_toml(filepath:)
    as { "Failed to find `gleam.toml` at: " <> filepath }

  let packages =
    tom.get_table(gt.toml, ["dependencies"])
    |> result.map(dict.keys)
    |> result.unwrap([])

  let dep_paths =
    packages
    |> list.filter_map(fn(package) {
      gen.dep_src_dir_path_(package:, in: "dependencies", toml: gt)
    })
    |> list.unique

  [ "src/", ..dep_paths ]
}

// LOOKUP ACTOR

fn fetch_context(
  actor actor: process.Name(LookupMsg),
  file file: GleamFile,
) -> Result(gen.Context, gen.GenErr) {
  let self = process.new_subject()

  actor
  |> process.named_subject
  |> actor.send(Context(file:, reply: self))

  process.receive(self, lookup_timeout_ms)
  |> result.replace_error(gen.Failed("context lookup timed out (" <> string.inspect(actor) <> ")"))
}

@internal
pub fn look_up_type(
  actor actor: process.Name(LookupMsg),
  mod mod: Option(String),
  name name: String,
  file file: GleamFile,
) -> Result(TypeDef, gen.GenErr) {
  let self = process.new_subject()

  actor
  |> process.named_subject
  |> actor.send(Type(mod:, name:, file:, reply: self))

  process.receive(self, lookup_timeout_ms)
  |> result.replace_error(gen.Failed("type lookup timed out (" <> string.inspect(actor) <> ")"))
  |> result.flatten
}

pub opaque type LookupMsg {
  LookupNoOp
  ReloadGleamToml
  ReloadFilepaths
  //
  Type(
    mod: Option(String),
    name: String,
    file: GleamFile,
    reply: Subject(Result(TypeDef, gen.GenErr)),
  )
  Context(
    file: GleamFile,
    reply: Subject(gen.Context),
  )
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
    // let re = process.new_subject()

    // let x = fn(strs: List(String)) {
    //   process.send(me, strs)
    // }

    // let sel =
    //   process.new_selector()
    //   |> process.select(me)
    //   |> process.map_selector(fn(strs) {
    // })

  actor.new_with_initialiser(100, fn(self) {
    LookupState(
      self:,
      pwd: load_pwd(),
      toml: load_toml(),
      filepaths: load_filepaths(),
      changes: dict.new(),
    )
    |> actor.initialised
    // |> actor.selecting(sel)
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

      Context(file:, reply:) -> {
        gen.Context(pwd: state.pwd, toml: state.toml, file:)
        |> process.send(reply, _)

        actor.continue(state)
      }
    }
  })
}

const lookup_timeout_ms = 5_000

// TODO mv generic

fn worker(
  actor actor: actor.Builder(state, msg, return),
) -> supervision.ChildSpecification(return) {
  supervision.worker(fn() { actor.start(actor) })
}

fn actor_named(
  name name: process.Name(msg),
  init init: fn(flags, Subject(msg)) -> state,
  sel sel: Option(fn(flags, state) -> Selector(msg)),
  update update: fn(state, msg) -> actor.Next(state, msg),
  timeout timeout: Int,
  return return: fn(state) -> return,
  flags flags: flags,
) -> actor.Builder(state, msg, return) {
  actor(init:, sel:, update:, timeout:, return:, flags:)
  |> actor.named(name)
}

fn actor(
  init init: fn(flags, Subject(msg)) -> state,
  sel sel: Option(fn(flags, state) -> Selector(msg)),
  update update: fn(state, msg) -> actor.Next(state, msg),
  timeout timeout: Int,
  return return: fn(state) -> return,
  flags flags: flags,
) -> actor.Builder(state, msg, return) {
  actor.new_with_initialiser(timeout, fn(self) {
    let state = init(flags, self)

    state
    |> actor.initialised
    |> actor.returning(return(state))
    |> fn(actor) {
      case sel {
        None ->
          actor

        Some(sel) ->
          sel(flags, state)
          |>  process.select(self)
          |> actor.selecting(actor, _)
      }
    }
    |> Ok
  })
  |> actor.on_message(update)
}
