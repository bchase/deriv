import deriv/internal/parser
import deriv/internal/common
import bchase/io
import bchase/unsafe
import bchase/dynamic as dyn
import gleam/dynamic.{type Dynamic, nil}
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
import deriv/internal/glance.{z, format_gleam_expr} as dg
import bchase/dict.{keyed as dict_keyed} as _
import bchase/result.{try_err, try_fail, try_fail_} as _
import bchase/function.{x, always, flip}
import shellout
//
import deriv/internal/glance.{term, call, call_, pipe, dot, short} as _
import bchase/casing
import deriv/internal/types.{type Derivation, type DerivField, type DerivFieldOpt} as _
import deriv/gen/types.{type TypeGenOpts, type ExprGen, type TypeDef, TypeDef, type GleamPath, GleamPath, type GleamFile, GleamFile, type GleamToml, GleamToml, type Imports, type AST, AST, Imports, type Pwd, type Context, Context}
//
import bchase/lens.{type Lens}
import bchase/list.{push as list_push} as _
import deriv/gen/reload/defs
import bchase/monad/read_write_result.{type ReadWriteResult} as monad
import bchase/option.{guard as some} as _

// magic comments
//   conv
//     from
//       `//$ derive ...`
//       `//$ gen ...`
//     to
//       `//$ package[/module].func ...`
//   examples
//     `//$ deriv[e].json [encode] [decode]
//     `//$ deriv[e].json [encode] [decode]

// pieces
//   - [erl+js]       `deriv`            >>> -- code gen defns helpers             -- `deriv`
//   - [erl+js]       `deriv_core`       >>> -- code gen defns (`derive` & `gen`)  -- `deriv/core`
//   - [erl+js]       `deriv_util`           -- runtime helpers                    -- `deriv/util
//   - [erl+js] (dev) `deriv_gen`            -- convenience runner                 -- `$ gleam run -m `deriv/gen`
//   - [erl]    (dev) `deriv_gen_supervisor` -- code gen defns & supervisor runner -- `deriv/gen/supervisor`

// code gens node
//   - separate package from project package
//   - deps
//     * all project package deps are maintained as `path` deps
//     * if a project dep is added/changed/removed, it's reflected here
//     * on project dep change, this package is rebuilt & restarted
//   - watches project package & project `build/packages/` for changes
//     * if changed file contains code gen def
//       -
//     * then, if changed file contains code gen magic comment
//       - run code gen & save to file

// code gen defn update
//   - core (package)
//   - curr package dep
//   - curr package
//     * same file
//     * diff file

//

// type VariantExpr {
//   Foo(
//     run: fn(
//       String,
//       g.Definition(g.Function),
//       fn(g.Type) -> Result(g.CustomType, Nil),
//     ) -> g.Expression,
//   )
// }

// fn gen_variant_expr(
//   ve: VariantExpr,
//   args args: String,
//   func func: g.Definition(g.Function),
//   get_type get_type: fn(g.Type) -> Result(g.CustomType, Nil),
// ) -> g.Expression {
//   ve.run(args, func, get_type)
// }

// pub fn func() -> var.VariantExpr(g.Clause) {
//   use variant <- var.variant_name()
//   use func <- var.variant_shorthand_field("func")
//   let handler = term("handle_" <> casing.snake(variant))

//   handler |> call([])
//   |> pipe("server" |> dot("process_func") |> call_([
//     func,
//     "ref" |> short,
//     "subs" |> short,
//     "ctx" |> short,
//   ]))
//   |> var.success
// }

//

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
  GleamFileErr(err: GleamFileErr, detail: Dynamic)
  //
  GleamDependencyFailedToResolve(package: String)
  CustomTypeNotFound(path: GleamPath, type_: String)
  ImportNotFound(path: GleamPath, name: String)
  TypeIsNotCustomType(path: GleamPath, name: String, type_: g.Type)
  //
  GenStrGleamModuleParseErr(gen_str: String)
  GenNotFound(path: GleamPath, func: String, gen_str: String)
  GenExprErr(path: GleamPath, gen_str: String)
  GenExprCommentSpliceErr(path: GleamPath, gen_str: String, expr: g.Expression, expr_src: String)
  //
  GenAllFailedToMatch(expr_gen: #(GleamPath, String), errs: List(String), detail: Dynamic)
  //
  GenExprWiredWithWrongArg(expr_gen: ExprGen, def: Def)
  //
  Failed(msg: String)
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

pub fn gleam_toml() -> Result(GleamToml, GleamTomlErr) {
  read_gleam_toml(filepath: "gleam.toml")
}

pub fn read_gleam_toml(
  filepath filepath: String
) -> Result(GleamToml, GleamTomlErr) {
  use src <- try_err(simplifile.read(filepath), FileReadErr)
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
  path path: GleamPath,
  toml gt: GleamToml,
) -> Result(Dep, Nil) {
  dep(name, "dependencies", gt)
  |> result.lazy_or(fn() {
    dep(name, "dev-dependencies", gt)
  })
  |> result.lazy_or(fn() {
    dep_package_and_module_names_differ(path, gt)
  })
}

fn dep_package_and_module_names_differ(
  path path: GleamPath,
  toml gt: GleamToml,
) -> Result(Dep, Nil) {
  use package <- try(
    case all_build_package_gleam_src_filepaths() { // TODO tk build elsewhere
      Ok(filepaths) -> {
        let filepath = { path.full |> string.join("/") } <> ".gleam"

        use filepath <- try(filepaths |> list.find(string.ends_with(_, filepath)))

        let assert Ok(package_re) =
          "[/](\\w+)[/]src[/]" |> re.from_string

        use match <- try(filepath |> re.scan(package_re, _) |> list.last)

        case match {
          re.Match(_, [Some(package)]) -> Ok(package)
          _ -> Error(Nil)
        }
      }

      Error(_errs) ->
        Error(Nil)
    }
  )

  any_dep(package, path, gt)
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

pub fn dep_src_dir_path(
  path path: GleamPath,
  toml toml: GleamToml,
) -> Result(String, GenErr) {
  let package = path.package

  use <- bool.guard(toml.name == package , Ok("src/"))

  use dep <- try_fail(any_dep(package, path, toml), GleamDependencyFailedToResolve(package:))

  case dep {
    DepString(..) ->
      Ok(build_packages_path(dep:))

    DepTable(table:, ..) ->
      case tom.get_string(table, ["path"]) {
        Ok(path) ->
          Ok(path <> "/src/")

        Error(_) ->
          Ok(build_packages_path(dep:))
      }
  }
}

// skips `any_dep`
pub fn dep_src_dir_path_(
  package package: String,
  in key: String,
  toml toml: GleamToml,
) -> Result(String, GenErr) {
  use <- bool.guard(toml.name == package , Ok("src/"))

  use dep <- try_fail(dep(package, key, toml), GleamDependencyFailedToResolve(package:))

  case dep {
    DepString(..) ->
      Ok(build_packages_path(dep:))

    DepTable(table:, ..) ->
      case tom.get_string(table, ["path"]) {
        Ok(path) ->
          Ok(path <> "/src/")

        Error(_) ->
          Ok(build_packages_path(dep:))
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

pub fn all_build_package_gleam_src_filepaths(
) -> Result(List(String), #(Int, String)) {
  use output <- try(shellout.command(in: ".", opt: [], run: "find", with: ["build/packages/"]))

  output
  |> string.split("\n")
  |> list.filter(string.ends_with(_, ".gleam"))
  |> list.filter(string.contains(_, "/src/"))
  |> Ok
}

// TODO mv generic
pub fn parse_gleam_module_path_from(
  filepath filepath: String,
) -> Result(GleamPath, GleamFileErr) {
  let err = GleamFilepathInvalid(filepath:)
  use rel <- try(filepath |> string.split("src/") |> list.last |> result.replace_error(err))
  use path <- try(rel |> string.split(".gleam") |> list.first |> result.replace_error(err))

  parse_gleam_module_path(path:)
}
pub fn parse_gleam_module_path(
  path path: String,
) -> Result(GleamPath, GleamFileErr) {
  types.parse_gleam_module_path(path:)
  |> result.replace_error(GleamModulePathInvalid(path:))
}

fn to_relative_src_filepath(
  path path: GleamPath,
) -> String {
  path.full
  |> string.join("/")
  |> string.append(suffix: ".gleam")
}

pub fn load_gleam_file(
  filepath filepath: String,
) -> Result(GleamFile, GleamFileErr) {
  use path <- try(parse_gleam_module_path_from(filepath:))
  use src <- try_err(simplifile.read(filepath), GleamFileNotFound(path: filepath, err: _))
  use module <- try_err(g.module(src), GleamFileInvalid(path: filepath, src:, err: _))

  Ok(GleamFile(
    path:,
    filepath: filepath,
    src:,
    ast: ast(module:),
  ))
}

fn load_context(
  path path: GleamPath,
  pwd pwd: Pwd,
  toml toml: GleamToml,
) -> Result(Context, GenErr) {
  use dep_src_dir_path <- try(dep_src_dir_path(path:, toml:))
  let filepath = dep_src_dir_path <> { path |> to_relative_src_filepath }
  use file <- try_err(load_gleam_file(filepath:), GleamFileErr(_, nil()))

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
) -> Result(TypeDef, GenErr) {
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
  |> result.map(fn(td) { TypeDef(..td, qualified: mod) }) // TODO elsewhere?
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
  |> result.map_error(GleamFileErr(_, nil()))
}

fn get_custom_type_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(TypeDef, GenErr) {
  get_custom_type_defined_in(ctx:, type_:)
  |> result.lazy_or(fn() {
    get_custom_type_aliased_in(ctx:, type_:)
  })
}

fn get_custom_type_unqualified_import_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(TypeDef, GenErr) {
  use import_ <- try(
    types.all_imports(ctx.file.ast.imports)
    |> list.find(fn(import_) {
      import_.definition.unqualified_types
      |> list.any(fn(t) { t.name == type_ })
    })
    |> result.replace_error(Failed("`get_custom_type_unqualified_import_in` miss"))
  )

  let module = import_.definition.module
  use path <- try_err(parse_gleam_module_path(path: module), GleamFileErr(_, nil()))
  use ctx <- try(load_context(pwd: ctx.pwd, path:, toml: ctx.toml))

  get_custom_type_in(ctx:, type_:)
}

fn get_custom_type_defined_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(TypeDef, GenErr) {
  ctx.file.ast.custom_types
  |> dict.get(type_)
  |> result.map(fn(def) { TypeDef(def:, path: ctx.file.path, qualified: None) })
  |> result.replace_error(CustomTypeNotFound(type_:, path: ctx.file.path))
}

fn get_custom_type_aliased_in(
  ctx ctx: Context,
  type_ type_: String,
) -> Result(TypeDef, GenErr) {
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

      use path <- try_err(parse_gleam_module_path(path: import_.module), GleamFileErr(_, nil()))
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

// pub type Err {
//   Err(err: String)
//   GenStrGleamModuleParseErr(gen_str: String)
// }

pub type Skip {
  Skip
}

const skip: Result(#(String, List(Ref)), Result(Skip, GenErr)) =
  Error(Ok(Skip))

type Lookups {
  Lookups(
    get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
    fetch: fn(#(String, String)) -> Result(ExprGen, Nil),
  )
}

type Acc {
  Acc(
    src: String,
    imports: List(g.Definition(g.Import)),
    types: List(types.Generated(g.CustomType)),
    funcs: List(types.Generated(g.Function)),
    offset: Int,
    refs: List(Ref),
  )
}

const lens_get_type = lens.Lens(get: get_get_type, set: set_get_type)
fn get_get_type(x: Lookups) { x.get_type }
fn set_get_type(x: Lookups, get_type) { Lookups(..x, get_type:)}
const lens_fetch = lens.Lens(get: get_fetch, set: set_fetch)
fn get_fetch(x: Lookups) { x.fetch }
fn set_fetch(x: Lookups, fetch) { Lookups(..x, fetch:)}

const lens_src = lens.Lens(get: get_src, set: set_src)
fn get_src(x: Acc) { x.src }
fn set_src(x: Acc, src) { Acc(..x, src:)}
const lens_imports = lens.Lens(get: get_imports, set: set_imports)
fn get_imports(x: Acc) { x.imports }
fn set_imports(x: Acc, imports) { Acc(..x, imports:)}
const lens_types = lens.Lens(get: get_types, set: set_types)
fn get_types(x: Acc) { x.types }
fn set_types(x: Acc, types) { Acc(..x, types:)}
const lens_funcs = lens.Lens(get: get_funcs, set: set_funcs)
fn get_funcs(x: Acc) { x.funcs }
fn set_funcs(x: Acc, funcs) { Acc(..x, funcs:)}
const lens_offset = lens.Lens(get: get_offset, set: set_offset)
fn get_offset(x: Acc) { x.offset }
fn set_offset(x: Acc, offset) { Acc(..x, offset:)}
const lens_refs = lens.Lens(get: get_refs, set: set_refs)
fn get_refs(x: Acc) { x.refs }
fn set_refs(x: Acc, refs) { Acc(..x, refs:)}

fn type_gens(
  ctx ctx: Context,
) -> List(#(g.Definition(g.CustomType), parser.TypeGens)) {
  let src = ctx.file.src
  let ast = ctx.file.ast
  let cts = ast.custom_types
  let parse = fn(str) {
    str
    |> parse_gleam_module_path
    |> result.map_error(string.inspect)
  }

  cts
  |> dict.values
  |> list.filter_map(fn(ct) {
    parser.parse_type_gens(type_: ct.definition, src:, ast:, parse:)
    |> result.map(pair.new(ct, _))
  })
}

// fn type_gens(
//   ctx ctx: Context,
// ) -> List(#(g.CustomType, List(Derivation), Dict(DerivField, List(DerivFieldOpt)))) {
//   let src = ctx.file.src
//   let cts = ctx.file.ast.custom_types

//   cts
//   |> dict.values
//   |> list.map(fn(def) { def.definition })
//   |> list.filter_map(fn(ct) {
//     src
//     |> dg.read_span(span: ct.location)
//     |> result.map_error(fn(_err) {
//       io.log_err([
//         "failed to read custom type span for type gen parsing",
//         string.inspect(ct),
//       ])
//     })
//     |> result.map(fn(src) {
//       parser.parse_type_with_derivations(ct, src)
//     })
//     |> result.flatten
//   })
// }

fn gen_for_types(
  tgs tgs: List(parser.TypeGens),
) {

}

pub fn process(
  ctx ctx: Context,
) -> Result(#(String, List(Ref)), Result(Skip, GenErr)) {
  let src = ctx.file.src

  let assert Ok(gen_magic_comment_start_re) =
    // "[/][/][$]\\s*gen([:]|\\s+)" |> re.from_string
    // "[/][/][$]\\s*gen\\s+" |> re.from_string
    "[/][/][$]" |> re.from_string

  use <- bool.guard(!re.check(gen_magic_comment_start_re, src), skip)

  let acc = Acc(
    src: ctx.file.src,
    imports: [],
    types: [],
    funcs: [],
    offset: 0,
    refs: [],
  )

  let lookups = Lookups(
    get_type: fn(mod, t) { get_custom_type(mod, t, ctx) |> result.replace_error(Nil) },
    // NOTE: close over `defs.expr_gens()` here to get fresh code reload
    fetch: dict.get(defs.expr_gens(), _),
    // NOTE: close over `defs.expr_gens()` here to get fresh code reload
  )

  let fgs =
    ctx.file.ast.functions
    |> dict.values
    |> list.map(build_func_gens(func: _, src:, ctx:))
    |> list.sort(fn(a, b) {
      int.compare(
        a.func.definition.location.start,
        b.func.definition.location.start,
      )
    })
    |> list.flat_map(fn(fg) {
      fg.gens |> list.map(fn(gen) { #(gen, Function(func: fg.func), fg.ctx) })
    })

  let result =
    fgs
    |> list.map(run_func_gen)
    |> monad.sequence
    |> monad.run_(lookups, acc)

  use acc <- result.try(
    case result {
      #(Ok(_), acc) -> Ok(acc)
      #(Error(err), _) -> Error(Error(err))
    }
  )

  let tgs =
    type_gens(ctx:)
    |> list.flat_map(fn(t) {
      let #(ct, parser.TypeGens(gens:, opts:)) = t

      gens
      |> list.map(fn(t) {
        let #(#(path, func), args) = t
        let gen = TypeGen(path:, func:, args:)
        #(gen, opts, CustomType(type_: ct), ctx)
      })
    })

  let result =
    tgs
    |> list.map(run_type_gen)
    |> monad.sequence
    |> monad.run_(lookups, acc)

  result
  |> process_acc(ctx:)
}

fn process_acc(
  result result: #(Result(List(ignored), GenErr), Acc),
  ctx ctx: Context,
) -> Result(#(String, List(Ref)), Result(Skip, GenErr)) {
  case result {
    #(Ok(_), acc) -> {
      let acc: Acc = acc
      let existing_func_names =
        ctx.file.ast.functions
        |> dict.keys

      let existing_type_names =
        ctx.file.ast.custom_types
        |> dict.keys

      let func_srcs =
        acc.funcs
        |> list.filter(fn(f) {
          case f.overwrite {
            True -> True
            False -> !list.contains(existing_func_names, f.def.definition.name)
          }
        })
        |> list.group(fn(f) { f.def.definition.name })
        |> dict.to_list
        |> list.filter_map(fn(t) {
          case t.1 {
            [] -> Error(Nil)
            [f] -> Ok(f)
            _ -> {
              io.log_err([
                "An expression generator is trying to define multiple functions with the name: " <> t.0
              ])
              Error(Nil)
            }
          }
        })
        |> list.map(fn(f) {
          common.func_str(f.def)
        })

      let type_srcs =
        acc.types
        |> list.filter(fn(f) {
          case f.overwrite {
            True -> True
            False -> !list.contains(existing_type_names, f.def.definition.name)
          }
        })
        |> list.group(fn(f) { f.def.definition.name })
        |> dict.to_list
        |> list.filter_map(fn(t) {
          case t.1 {
            [] -> Error(Nil)
            [f] -> Ok(f)
            _ -> {
              io.log_err([
                "An expression generator is trying to define multiple functions with the name: " <> t.0
              ])
              Error(Nil)
            }
          }
        })
        |> list.map(fn(f) {
          common.type_str(f.def)
        })

      let src =
        [
          [acc.src |> string.trim],
          type_srcs,
          func_srcs,
        ]
        |> list.flatten
        |> string.join("\n\n")

      let imports =
        acc.imports
        |> list.map(fn(def) { def.definition })

      let src =
        common.consolidate_imports_for(src, imports)

      Ok(#(src, acc.refs))
    }

    #(Error(err), _acc) ->
      Error(Error(err))
  }
}

fn run_func_gen(
  gen_ctx: #(Gen, Def, Context),
) -> ReadWriteResult(Nil, GenErr, Lookups, Acc) {
  let #(gen, def, ctx) = gen_ctx
  use Lookups(get_type:, fetch:) <- monad.read_()

  use orig <- monad.writes(at: lens_src)
  use offset <- monad.writes(at: lens_offset)

  // offset positions based on previous code gen results
  let gen = Gen(..gen, pos: gen.pos + offset)

  use #(expr_gen, args, #(path, _func) as mf) <- monad.do(get_expr_gen_from(str: gen.str, fetch:))

  // gen expr
  use GenExpr(expr:, imports:, types:, funcs:, refs: new_refs) <- monad.do_ok_(
    build_expr(mf, expr_gen, args, def, dict.new(), get_type, ctx),
  )

  // register funcs to be added to src
  use <- monad.concat(imports, lens_imports)
  use <- monad.concat(types, lens_types)
  use <- monad.concat(funcs, lens_funcs)

  // persist refs
  use <- monad.concat(new_refs, lens_refs)
  // TODO this could be done in `build_expr` w/o having to pass these back up

  use expr <- some(expr, monad.pure(Nil))

  // ensure that expr is wrapped in a block
  let expr = case expr {
    g.Block(..) -> expr
    _ -> g.Block(z, [g.Expression(expr)])
  }

  // build & format `glance.Expression` as `String`
  let expr_src = format_gleam_expr(expr:, indent: gen.indent)

  // add magic comment back to gen'd `glance.Expression` src
  use #(x, xs) <- monad.do_ok(
    case expr_src |> string.split("\n") {
      [] | [_] -> Error(Nil)
      [x, ..xs] -> Ok(#(x, xs))
    },
    always(GenExprCommentSpliceErr(path:, gen_str: gen.str, expr:, expr_src:)),
  )
  let expr_src =
    [x <> " " <> gen.comment, ..xs]
    |> string.join("\n")
    |> string.trim_start

  // calc span to overwrite
  let span = gen_span(gen:, src: orig, offset: 0)

  // construct new src
  let new = dg.replace(span:, in: orig, with: expr_src)

  // calc diff for, and persist new `offset`
  let diff = string.length(new) - string.length(orig)
  use <- monad.add_int(diff, lens_offset)

  // persist newly gen'd src
  use <- monad.set(new, lens_src)

  monad.pure(Nil)
}

type TypeGen {
  TypeGen(
    path: GleamPath,
    func: String,
    args: String,
  )
}

fn run_type_gen(
  gen_ctx: #(TypeGen, TypeGenOpts, Def, Context),
) -> ReadWriteResult(Nil, GenErr, Lookups, Acc) {
  let #(gen, opts, def, ctx) = gen_ctx
  use Lookups(get_type:, fetch:) <- monad.read_()

  let gen_str = string.join(gen.path.full, "/") <> "." <> gen.func <> " " <> gen.args

  use #(expr_gen, args, mf) <- monad.do(get_expr_gen_from(str: gen_str, fetch:))

  // gen expr
  use GenExpr(expr: _, imports:, types:, funcs:, refs: new_refs) <- monad.do_ok_(
    build_expr(mf, expr_gen, args, def, opts, get_type, ctx) |> fn(x) {
      // panic
      // io.println(string.inspect(x))
      x
    },
  )

  // register funcs to be added to src
  use <- monad.concat(imports, lens_imports)
  use <- monad.concat(types, lens_types)
  use <- monad.concat(funcs, lens_funcs)

  // persist refs
  use <- monad.concat(new_refs, lens_refs)
  // TODO this could be done in `build_expr` w/o having to pass these back up

  monad.pure(Nil)
}

pub type GenExpr {
  GenExpr(
    expr: Option(g.Expression),
    imports: List(g.Definition(g.Import)),
    types: List(types.Generated(g.CustomType)),
    funcs: List(types.Generated(g.Function)),
    refs: List(Ref),
  )
}

pub type Ref {
  Ref(
    from: GleamPath,
    to: GleamPath,
    ident: Option(String),
  )
}

fn get_expr_gen_from(
  str str: String,
  fetch fetch: fn(#(String, String)) -> Result(ExprGen, Nil),
) -> ReadWriteResult(#(ExprGen, String, #(GleamPath, String)), GenErr, r, w) {
  use #(str, args) <- monad.do_ok(case str |> string.split(" ") {
    [] -> Error(GenStrGleamModuleParseErr(gen_str: str))
    [path, ..rest] -> Ok(#(path, rest |> string.join(" ")))
  }, function.identity)

  use #(path, func) <- monad.do_ok(case str |> string.split(".") {
    [path, func] -> Ok(#(path, func))
    _ -> Error(GenStrGleamModuleParseErr(gen_str: str))
  }, function.identity)

  use path <- monad.do_ok(
    parse_gleam_module_path(path),
    GleamFileErr(_, dyn.from("//$ " <> str)),
  )

  use expr_gen <- monad.do(monad.ok(
    fetch(#(path.full |> string.join("/"), func)),
    always(GenNotFound(path:, func:, gen_str: str))
  ))

  monad.pure(#(expr_gen, args, #(path, func)))
}

// TODO partially dup'd in `types`
fn get_named_param_type(
  str str: String,
  func func: g.Definition(g.Function),
) -> Result(#(Option(String), String), Nil) {
  use param_type <- try(get_param_type(str:, func:))

  case param_type {
    g.NamedType(module:, name:, ..) -> Ok(#(module, name))
    _ -> Error(Nil)
  }
}
fn get_param_type(
  str str: String,
  func func: g.Definition(g.Function),
) -> Result(g.Type, Nil) {
  func.definition.parameters
  |> list.find(fn(param) {
    param.label == Some(str) || param.name == g.Named(str)
  })
  |> result.map(fn(param) { param.type_ })
  |> result.map(option.to_result(_, Nil))
  |> result.flatten
}

//

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
    ctx: Context,
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
  ctx ctx: Context,
) -> FuncGens {
  let span = func.definition.location

  let func_src =
    dg.read_span(src:, span:)
    |> result.lazy_unwrap(fn() {
      panic as "tried to read func def outside of bounds of src"
    })

  let lines = func_src |> string.split("\n")

  let assert Ok(start_re) =
    "^((\\s*)([{]\\s*)?)([/][/][$]\\s*(.+)$)"
    |> re.from_string

  list.fold(lines, #(None, [], span.start), fn(acc, line) {
    let #(gen, gens, pos) = acc
    let next_pos = pos + string.length(line)

    {
      use #(gen_str, comment, has_block, indent, until_comment) <- try(
        case re.scan(start_re, line) {
          [re.Match(_, [pre_comment, indent, bracket, Some(comment), Some(gen_str)])] ->
            Ok(#(
              gen_str |> string.trim_start,
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
  |> FuncGens(gens: _, func:, ctx:)
}

fn gen_span(
  gen gen: Gen,
  src src: String,
  offset offset: Int,
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
  |> fn(span) {
    g.Span(start: span.start + offset, end: span.end + offset)
  }
}

fn bracket_pos(
  gen gen: Gen,
) -> Option(Int) {
  use <- bool.guard(gen.new, None)
  Some(gen.pos + 1)
}

//

pub type Def {
  Function(func: g.Definition(g.Function))
  CustomType(type_: g.Definition(g.CustomType))
}

fn build_expr(
  target target: #(GleamPath, String),
  gen gen: types.ExprGen,
  args args: String,
  def def: Def,
  opts opts: TypeGenOpts,
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
  ctx ctx: Context,
) -> Result(GenExpr, GenErr) {
  let args = types.Args(raw: args, named: parser.parse_named_params(args))

  case gen, def {
    types.VariantClauseCaseExprGen(clauses: gens), Function(func:) ->
      case_expr_with_variant_clauses(target:, gens:, args:, func:, get_type:, ctx:)

    types.CustomTypeDeriveExprGen(gens:), CustomType(type_:) ->
      custom_type_derive(target:, gens:, args:, type_:, opts:, get_type:, ctx:)

    types.VariantClauseCaseExprGen(..), CustomType(..) |
    types.CustomTypeDeriveExprGen(..), Function(..) ->
      Error(GenExprWiredWithWrongArg(expr_gen: gen, def:))
  }
}

fn case_expr_with_variant_clauses(
  target target: #(GleamPath, String),
  gens gens: List(types.GenVariantCaseClause(g.Clause)),
  args args: types.Args,
  func func: g.Definition(g.Function),
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
  ctx ctx: Context,
) -> Result(GenExpr, GenErr) {
  let fail = fn(msg) { Failed("[variant clause expr] " <> msg) }

  use subject <- try(args.named |> dict.get("subject") |> result.map_error(fn(_) {
    fail("requires an `subject` (fn param reference) to be specified, but none was found")
  }))

  use #(mod, type_) <- try(get_named_param_type(str: subject, func:) |> result.map_error(fn(_) {
    fail("couldn't find named param in containing function: " <> subject)
  }))

  use type_ <- try(get_type(mod, type_) |> result.map_error(fn(_) {
    fail("failed type lookup: " <> string.inspect(#(mod, type_)))
  }))

  let file = ctx.file

  let ref = Ref(from: file.path, to: type_.path, ident: Some(type_.def.definition.name))

  use #(clauses, funcs) <- try(
    type_.def.definition.variants
    |> list.map(
      build_case_clause_expr(gens:, target:, variant:_, type_:, file:, args:, func:, get_type:)
    )
    |> result.all
    |> result.map(fn(t) {
      t
      |> list.unzip
      |> pair.map_second(list.flatten)
    })
  )

  let expr = g.Case(z, subjects: [term(subject)], clauses:) |> Some
  Ok(GenExpr(expr:, imports: [], types: [], funcs:, refs: [ref]))
}

fn build_case_clause_expr(
  target target: #(GleamPath, String),
  variant variant: g.Variant,
  gens gens: List(types.GenVariantCaseClause(g.Clause)),
  type_ type_: TypeDef,
  file file: GleamFile,
  args args: types.Args,
  func func: g.Definition(g.Function),
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
) -> Result(#(g.Clause, List(types.Generated(g.Function))), GenErr) {
  use #(clause, ensure_funcs) <- try({
    gens
    |> list.map(fn(gen) { // TODO perf `fold_until`
      let #(result, write) =
        types.run_gen(gen:, expr: #(variant, type_), file:, args:, opts: dict.new(), target:, get_type:)

      result
      |> result.map(fn(clause) {
        #(clause, types.gen_funcs(write))
      })
    })
    |> fn(results) {
      case result.partition(results) {
        #([x, ..], _errs) -> Ok(x)
        #([], errs) -> Error(GenAllFailedToMatch(target, errs,
          dyn.from(#(args, variant, type_, func))),
        )
      }
    }
  })

  Ok(#(clause, ensure_funcs))
}

fn custom_type_derive(
  target target: #(GleamPath, String),
  gens gens: List(types.Gen(Nil, g.Definition(g.CustomType))),
  args args: types.Args,
  type_ type_: g.Definition(g.CustomType),
  opts opts: TypeGenOpts,
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
  ctx ctx: Context,
) -> Result(GenExpr, GenErr) {
  // let fail = fn(msg) { Failed("[custom type derive expr] " <> msg) }

  let file = ctx.file

  let #(gens, errs) =
    gens
    |> list.map(types.run_gen(gen: _, expr: type_, file:, args:, opts:, target:, get_type:))
    |> list.map(fn(t) {
      case t.0 {
        Ok(Nil) -> Ok(t.1)
        Error(err) -> Error(err)
      }
    })
    |> result.partition

  case errs {
    [] -> Nil
    _ -> io.log_err(["custom type derive had issue with: " <> string.inspect(type_), ..errs])
  }

  Ok(GenExpr(
    expr: None,
    refs: [],
    imports: gens |> list.flat_map(types.gen_imports),
    types: gens |> list.flat_map(types.gen_types),
    funcs: gens |> list.flat_map(types.gen_funcs),
  ))
}

//

fn scan_for_magic_comment_refs_to_other_modules(
  filepaths filepaths: List(String),
) -> List(Ref) {
  filepaths
  |> monad.map_m(fn(filepath) {
    case load_gleam_file(filepath:) {
      Error(err) -> {
        io.log_err([
          { "ref scan failed for `" <> filepath <> "` with:" },
          string.inspect(err)
        ])

        monad.pure(Nil)
      }

      Ok(file) -> {
        file.src

        monad.pure(Nil)
      }
    }
  })
  |> monad.run_(Nil, [])
  |> pair.second
}
