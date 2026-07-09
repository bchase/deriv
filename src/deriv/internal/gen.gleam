import gleam/io
import bchase/unsafe
import bchase/dynamic
import simplifile
import tom
import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{Some, type Option, None}
import glance_printer
import gleam/pair
import gleam/result.{try}
import gleam/bool
import glance as g
import gleam/regexp as re
import gleam/list
import gleam/string
import deriv/internal/glance.{z} as dg
import bchase/dict.{keyed as dict_keyed} as _
import bchase/result.{try_err, try_fail, try_fail_} as _
import bchase/function.{x, always}

pub type Context {
  Context(
    pwd: Pwd,
    toml: GleamToml,
    file: GleamFile,
  )
}

pub opaque type Pwd {
  Pwd(pwd: String)
}

pub type GleamFileErr {
  GleamFilepathInvalid(filepath: String)
  GleamModulePathInvalid(path: String)
  GleamFileNotFound(path: String, err: simplifile.FileError)
  GleamFileInvalid(path: String, src: String, err: g.Error)
}

pub type GleamTomlErr {
  FileReadErr(err: simplifile.FileError)
  TomlParseErr(err: tom.ParseError)
  TomlReadErr(err: tom.GetError)
}

pub type GenErr {
  GleamFileErr(err: GleamFileErr)
  GleamDependencyFailedToResolve(package: String)
  CustomTypeNotFound(path: GleamPath, type_: String)
  ImportNotFound(path: GleamPath, name: String)
  TypeIsNotCustomType(path: GleamPath, name: String, type_: g.Type)
  Failed(msg: String)
}

pub fn pwd() -> Result(Pwd, simplifile.FileError) {
  simplifile.current_directory()
  |> result.map(fn(pwd) {
    case string.ends_with(pwd, "/") {
      True -> Pwd(pwd:)
      False -> Pwd(pwd: pwd <> "/")
    }
  })
}

// fn load_context(
//   path path: String,
//   pwd pwd: Pwd,
//   toml toml: GleamToml,
// ) -> Result(Context, GenErr) {
//   // use path <- try_err(parse_gleam_module_path(path:), GleamFileErr)
//   // use package <- try_err(package_name(path:), GleamFileErr)
//   // use dep_src_dir_path <- try(dep_src_dir_path(package:, toml:))

//   // dep_src_dir_path <>
//   todo

//   todo
// }

// fn src_path(
//   path path: String,
//   pwd pwd: Pwd,
//   toml toml: GleamToml,
// ) -> Result(String, GleamFileErr) {
//   use package <- try(package_name(path:))

//   use <- bool.lazy_guard(package == "gleam", fn() {
//     panic as "load context for gleam stdlib type..."
//   })

//   use <- bool.lazy_guard(package == toml.name, fn() {
//     Ok("src/" <> path <> ".gleam")
//   })

//   // case any_dep(name: package, toml:) {
//   //   Error(Nil) ->
//   //     Error(GleamFileNotFound(path: todo, err: todo))

//   //   Ok(dep) ->
//   // }
//   case dep_path(name: package, toml:) {
//     Error(Nil) -> todo
//     Ok() -> todo
//   }
// }

fn filepath(
  path path: String,
  toml toml: GleamToml,
) -> Result(String, GenErr) {
  // use _ <- try_fail(any_dep(path:, toml:), DependencyNotFound(path:))
  todo
}

pub type GleamPath {
  GleamPath(
    full: List(String),
    package: String,
    module: String,
  )
}

pub type GleamFile {
  GleamFile(
    path: GleamPath,
    filepath: String,
    src: String,
    ast: AST,
  )
}

pub type AST {
  AST(
    imports: Imports,
    custom_types: Dict(String, g.Definition(g.CustomType)),
    type_aliases: Dict(String, g.Definition(g.TypeAlias)),
    constants: Dict(String, g.Definition(g.Constant)),
    functions: Dict(String, g.Definition(g.Function)),
  )
}

pub type Imports {
  Imports(
    named: Dict(String, g.Definition(g.Import)),
    discarded: List(g.Definition(g.Import)),
  )
}

pub type GleamToml {
  GleamToml(
    name: String,
    toml: Dict(String, tom.Toml),
  )
}

fn all_imports(
  imports imports: Imports,
) -> List(g.Definition(g.Import)) {
  [
    imports.named |> dict.values,
    imports.discarded,
  ]
  |> list.flatten
}

fn init() -> Result(Context, GleamTomlErr) {
  use gt <- try(gleam_toml())
  // Context(path: todo, module: todo, toml: todo)
  todo
}

fn gleam_toml() -> Result(GleamToml, GleamTomlErr) {
  use src <- try_err(simplifile.read("gleam.toml"), FileReadErr)
  use toml <- try_err(tom.parse(src), TomlParseErr)
  use name <- try_err(tom.get_string(toml, ["name"]), TomlReadErr)
  Ok(GleamToml(name:, toml:))
}

type Dep {
  DepString(
    name: String,
    str: String,
  )
  DepTable(
    name: String,
    table: Dict(String, tom.Toml),
  )
}

fn any_dep(
  name name: String,
  toml gt: GleamToml,
) -> Result(Dep, Nil) {
  dep(name, "dependencies", gt)
  |> result.lazy_or(fn() {
    dep(name, "dev-dependencies", gt)
  })
}

fn dep(
  name name: String,
  in key: String,
  toml gt: GleamToml,
) -> Result(Dep, Nil) {
  dep_string(name, key, gt)
  |> result.lazy_or(fn() {
    dep_table(name, key, gt)
  })
}

fn dep_string(
  name name: String,
  in key: String,
  toml gt: GleamToml,
) -> Result(Dep, Nil) {
  use str <- try_fail(tom.get_string(gt.toml, [key, name]), Nil)
  Ok(DepString(name:, str:))
}

fn dep_table(
  name name: String,
  in key: String,
  toml gt: GleamToml,
) -> Result(Dep, Nil) {
  use table <- try_fail(tom.get_table(gt.toml, [key, name]), Nil)
  Ok(DepTable(name:, table:))
}

fn dep_src_dir_path(
  package package: String,
  toml toml: GleamToml,
) -> Result(String, GenErr) {
  use <- bool.guard(toml.name == package , Ok("src/"))

  use dep <- try_fail(any_dep(package, toml), GleamDependencyFailedToResolve(package:))

  case dep {
    DepString(..) ->
      Ok(build_packages_path(dep:))

    DepTable(table:, ..) ->
      case tom.get_string(table, ["path"]) {
        Ok(path) -> Ok(path <> "/src/")
        Error(_) -> Ok(build_packages_path(dep:))
      }
  }
}

fn build_packages_path(
  dep dep: Dep,
) -> String {
  "build/packages/" <> dep.name <> "/src/"
}

// type Dependency {
//   DependencySimple(
//     name: String,
//     version: String,
//   )
//   DependencyGit(
//     name: String,
//     repo: String,
//     ref: Option(String),
//     toml: tom.Toml,
//   )
//   DependencyPath(
//     name: String,
//     path: String,
//     toml: tom.Toml,
//   )
//   DependencyOther(
//     name: String,
//     toml: tom.Toml,
//   )
// }

// type Dependencies {
//   Dependencies(
//     core: List(Dependency),
//     dev: List(Dependency),
//   )
// }

// fn deps(
//   toml gt: GleamToml,
// ) -> List(Dependency) {
//   tom.get_table(gt.toml, ["dependencies"])
//   |> todo
//   |> result.unwrap([])

//   |> todo
// }

// fn dep_src_dir_path(
// ) -> Result(String, Nil) {
// }

pub fn ast(
  module module: g.Module,
) -> AST {
  let #(named, discarded) =
    module.imports
    |> list.map(fn(x) {
      let path = x.definition.module
      case x.definition.alias {
        None -> {
          let name =
            parse_gleam_module_path(path:)
            |> result.map(fn(path) { path.module })
            |> result.lazy_unwrap(fn() {
              io.print_error("`parse_gleam_module_path` miss: " <> path)
              "__parse_gleam_module_path:miss:" <> path
            })

          Ok(#(name, x)) // TODO `unwrap`
        }
        Some(g.Named(name)) -> Ok(#(name, x))
        Some(g.Discarded(_)) -> Error(x)
      }
    })
    |> result.partition
    |> pair.map_first(dict.from_list)

  AST(
    imports: Imports(named:, discarded:),
    custom_types: module.custom_types |> dict_keyed(fn(x) { x.definition.name }),
    type_aliases: module.type_aliases |> dict_keyed(fn(x) { x.definition.name }),
    constants: module.constants |> dict_keyed(fn(x) { x.definition.name }),
    functions: module.functions |> dict_keyed(fn(x) { x.definition.name }),
  )
}

pub fn main() {
  let assert Ok(pwd) = pwd()
  let assert Ok(toml) = gleam_toml()

  let filepath = "src/deriv/internal/dummy/lookup.gleam"
  let assert Ok(file) = load_gleam_file(filepath:)

  let ctx = Context(pwd:, toml:, file:)

  // // curr package
  // echo get_custom_type(ctx:, mod: None, type_: "Local")
  // echo get_custom_type(ctx:, mod: None, type_: "LocalAlias")
  // echo get_custom_type(ctx:, mod: None, type_: "OtherImport")
  // echo get_custom_type(ctx:, mod: Some("lookup_other"), type_: "Other")
  // echo get_custom_type(ctx:, mod: Some("oo"), type_: "OtherOther")

  // dep
  echo get_custom_type(ctx:, mod: Some("glance"), type_: "Span")
  // dep at path

  // gleam stdlib
  // gleam other package ... e.g. `gleam_erlang`
  // package name doesn't match import, e.g. `lustre_dev_tools` (same as above?)

  Nil
}

fn parse_gleam_module_path_from(
  filepath filepath: String,
) -> Result(GleamPath, GleamFileErr) {
  let err = GleamFilepathInvalid(filepath:)
  use rel <- try(filepath |> string.split("src/") |> list.last |> result.replace_error(err))
  use path <- try(rel |> string.split(".gleam") |> list.first |> result.replace_error(err))

  parse_gleam_module_path(path:)
}

fn parse_gleam_module_path(
  path path: String,
) -> Result(GleamPath, GleamFileErr) {
  case string.split(path, "/") {
    [] -> Error(Nil)
    [package, ..rest] as full->
      case rest |> list.reverse {
        [] -> Ok(GleamPath(package:, module: package, full:))
        [module, ..] -> Ok(GleamPath(package:, module:, full:))
      }
  }
  |> result.replace_error(GleamModulePathInvalid(path:))
}

fn to_relative_src_filepath(
  path path: GleamPath,
) -> String {
  path.full
  |> string.join("/")
  |> string.append(suffix: ".gleam")
}

fn load_gleam_file(
  filepath abs: String,
) -> Result(GleamFile, GleamFileErr) {
  use path <- try(parse_gleam_module_path_from(filepath: abs))
  use src <- try_err(simplifile.read(abs), GleamFileNotFound(path: abs, err: _))
  use module <- try_err(g.module(src), GleamFileInvalid(path: abs, src:, err: _))

  Ok(GleamFile(
    path:,
    filepath: abs,
    src:,
    ast: ast(module:),
  ))
}

fn load_context(
  path path: GleamPath,
  pwd pwd: Pwd,
  toml toml: GleamToml,
) -> Result(Context, GenErr) {
  use dep_src_dir_path <- try(dep_src_dir_path(package: path.package, toml:))
  let filepath = dep_src_dir_path <> { path |> to_relative_src_filepath }
  use file <- try_err(load_gleam_file(filepath:), GleamFileErr)

  Ok(Context(pwd:, toml:, file:))
}

// fn load_gleam_file_in(
//   ctx ctx: Context,
//   path path: String,
// ) -> Result(GleamFile, GleamFileErr) {
// }

// fn absolute_filepath_in(
//   ctx ctx: Context,
//   path path: String,
// ) {
// }

pub fn get_custom_type(
  mod mod: Option(String),
  type_ type_: String,
  ctx ctx: Context,
) -> Result(g.CustomType, GenErr) {
  use ctx <- try(case mod {
    None ->
      Ok(ctx)

    Some(module) -> {
      use path <- try(import_path(module:, ctx:))
      load_context(pwd: ctx.pwd, path:, toml: ctx.toml)
    }
  })

  get_custom_type_in(ctx:, type_:)
  |> result.lazy_or(fn() {
    get_custom_type_unqualified_import_in(ctx:, type_:)
  })
}

fn import_path(
  module module: String,
  ctx ctx: Context,
) -> Result(GleamPath, GenErr) {
  use g.Definition(_, import_) <- try_err(
    ctx.file.ast.imports.named |> dict.get(module),
    always(ImportNotFound(path: ctx.file.path, name: module)),
  )
  parse_gleam_module_path(path: import_.module)
  |> result.map_error(GleamFileErr)
}

fn get_custom_type_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(g.CustomType, GenErr) {
  get_custom_type_defined_in(ctx:, type_:)
  |> result.lazy_or(fn() {
    get_custom_type_aliased_in(ctx:, type_:)
  })
}

fn get_custom_type_unqualified_import_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(g.CustomType, GenErr) {
  use import_ <- try(
    all_imports(ctx.file.ast.imports)
    |> list.find(fn(import_) {
      import_.definition.unqualified_types
      |> list.any(fn(t) { t.name == type_ })
    })
    |> result.replace_error(Failed("`get_custom_type_unqualified_import_in` miss"))
  )

  let module = import_.definition.module
  use path <- try_err(parse_gleam_module_path(path: module), GleamFileErr)
  use ctx <- try(load_context(pwd: ctx.pwd, path:, toml: ctx.toml))

  get_custom_type_in(ctx:, type_:)
}

fn get_custom_type_defined_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(g.CustomType, GenErr) {
  ctx.file.ast.custom_types
  |> dict.get(type_)
  |> result.map(fn(t) { t.definition })
  |> result.replace_error(CustomTypeNotFound(type_:, path: ctx.file.path))
}

fn get_custom_type_aliased_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(g.CustomType, GenErr) {
  use alias <- try(
    ctx.file.ast.type_aliases
    |> dict.get(type_)
    |> result.map(fn(t) { t.definition })
    |> result.replace_error(CustomTypeNotFound(type_:, path: ctx.file.path))
  )

  case alias.aliased {
    g.NamedType(name:, module: None, ..) ->
      get_custom_type_in(ctx, name)

    g.NamedType(name:, module: Some(module), ..) -> {
      use g.Definition(_, import_) <- try_err(
        ctx.file.ast.imports.named |> dict.get(module),
        always(ImportNotFound(path: ctx.file.path, name: module)),
      )

      use path <- try_err(parse_gleam_module_path(path: import_.module), GleamFileErr)
      use ctx <- try(load_context(pwd: ctx.pwd, path:, toml: ctx.toml))

      get_custom_type_in(ctx, name)
    }

    t ->
      Error(TypeIsNotCustomType(path: ctx.file.path, name: type_, type_: t))
  }
}

//
//
//
//
//

const gen_magic_comment_start = "//$ gen"

pub fn process(
  module module: g.Module,
  src src: String,
) -> String {
  use <- bool.guard(!{ src |> string.contains(gen_magic_comment_start) }, src)

  module.functions
  |> list.map(build_func_gens(func: _, src:))
  |> list.sort(fn(a, b) {
    int.compare(
      a.func.definition.location.start,
      b.func.definition.location.start,
    )
  })
  |> list.fold(src, run)
}

//

fn run(
  src src: String,
  func_gens func_gens: FuncGens,
) -> String {
  let FuncGens(func:, gens:) = func_gens

  gens
  |> list.fold(#(src, 0), fn(acc, gen) {
    let #(old, offset) = acc

    // offset positions based on previous code gen results
    let gen = Gen(..gen, pos: gen.pos + offset)

    // build & format `glance.Expression` as `String`
    let expr = test1(gen:)
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

pub fn test1(
  gen gen: Gen,
) -> g.Expression {
  g.Block(z, [g.Expression(
    g.Case(z, [g.Variable(z, "foo")], [
      g.Clause(patterns: [[g.PatternDiscard(z, "")]], guard: None, body: g.Variable(z, "foo")),
    ]),
  )])
}

pub fn test2(
  gen gen: Gen,
) -> g.Expression {
  g.Block(z, [g.Expression(
    g.Case(z, [g.Variable(z, "bar")], [
      g.Clause(patterns: [[g.PatternDiscard(z, "")]], guard: None, body: g.Variable(z, "bar")),
    ]),
  )])
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

type FuncGens {
  FuncGens(
    func: g.Definition(g.Function),
    gens: List(Gen),
  )
}

pub type Gen {
  Gen(
    str: String,
    comment: String,
    indent: Int,
    pos: Int,
    new: Bool,
  )
}

fn build_func_gens(
  func func: g.Definition(g.Function),
  src src: String,
) -> FuncGens {
  let span = func.definition.location

  let func_src =
    dg.read_span(src:, span:)
    |> result.lazy_unwrap(fn() {
      panic as "tried to read func def outside of bounds of src"
    })

  let lines = func_src |> string.split("\n")

  let assert Ok(start_re) =
    "^((\\s*)([{]\\s*)?)([/][/][$]\\s*?gen\\s+(.+)$)"
    |> re.from_string

  list.fold(lines, #(None, [], span.start), fn(acc, line) {
    let #(gen, gens, pos) = acc
    let next_pos = pos + string.length(line)

    {
      use #(gen_str, comment, has_block, indent, until_comment) <- try(
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
  |> FuncGens(gens: _, func:)
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
