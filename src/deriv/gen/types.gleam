import bchase/lens.{type Lens}
import bchase/list.{push as list_push} as _
import bchase/monad/read_write_result as monad
import glance as g
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, type Option, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import tom

pub const expr_gen_type_name = "ExprGen"
pub type ExprGen {
  VariantClauseCaseExprGen(
    clauses: List(Gen(g.Clause, #(g.Variant, TypeDef))),
  )
  CustomTypeDeriveExprGen(
    gens: List(Gen(Nil, g.Definition(g.CustomType))),
  )
}

pub type GenVariantCaseClause(t) = Gen(t, #(g.Variant, TypeDef))

pub type TypeDef {
  TypeDef(
    path: GleamPath,
    def: g.Definition(g.CustomType),
    qualified: Option(String),
  )
}

pub type TypeGenOpts = Dict(#(String, Option(String), String), List(String))

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

pub type GleamToml {
  GleamToml(
    name: String,
    toml: Dict(String, tom.Toml),
  )
}

pub type Context {
  Context(
    // pwd: Pwd,
    toml: GleamToml,
    file: GleamFile,
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

pub fn all_imports(
  imports imports: Imports,
) -> List(g.Definition(g.Import)) {
  [
    imports.discarded,
    imports.named |> dict.values,
  ]
  |> list.flatten
}

pub fn find_import(
  imports imports: Imports,
  path path: GleamPath
) -> Result(g.Definition(g.Import), Nil) {
  let module = path.full |> string.join("/")

  imports
  |> all_imports
  |> fn(is) {
    use i <- list.find_map(is)
    use <- bool.guard(i.definition.module != module, Error(Nil))
    Ok(i)
  }
}

pub opaque type Gen(t, expr) {
  Gen(monad: monad.ReadWriteResult(t, String, Read(expr), Writeite))
}

pub opaque type Read(expr) {
  Read(
    expr: expr,
    //
    file: GleamFile,
    args: Args,
    target: #(GleamPath, String),
    opts: TypeGenOpts,
    get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
  )
}

pub type Args {
  Args(
    raw: String,
    named: Dict(String, String),
  )
}

pub opaque type Writeite {
  Writeite(
    fields: Set(String), // NOTE: fields acc, used to detect need for `with_spread`
    imports: List(g.Definition(g.Import)),
    consts: List(Generated(g.Constant)),
    types: List(Generated(g.CustomType)),
    funcs: List(Generated(g.Function)),
  )
}

pub fn gen_imports(
  write write: Writeite,
) -> List(g.Definition(g.Import)) {
  write.imports
}

pub fn gen_types(
  write write: Writeite,
) -> List(Generated(g.CustomType)) {
  write.types
}

pub fn gen_funcs(
  write write: Writeite,
) -> List(Generated(g.Function)) {
  write.funcs
}

@internal
pub const lens_opts = lens.Lens(get: get_opts, set: set_opts)
fn get_opts(x: Read(opts)) { x.opts }
fn set_opts(x: Read(opts), opts) { Read(..x, opts:)}
const lens_expr = lens.Lens(get: get_expr, set: set_expr)
fn get_expr(x: Read(expr)) { x.expr }
fn set_expr(x: Read(expr), expr) { Read(..x, expr:)}
const lens_file = lens.Lens(get: get_file, set: set_file)
fn get_file(x: Read(file)) { x.file }
fn set_file(x: Read(file), file) { Read(..x, file:)}
const lens_args = lens.Lens(get: get_args, set: set_args)
fn get_args(x: Read(expr)) { x.args }
fn set_args(x: Read(expr), args) { Read(..x, args:)}
const lens_get_type = lens.Lens(get: get_get_type, set: set_get_type)
fn get_get_type(x: Read(expr)) { x.get_type }
fn set_get_type(x: Read(expr), get_type) { Read(..x, get_type:)}
// const lens_target = lens.Lens(get: get_target, set: set_target)
// fn get_target(x: Read(expr)) { x.target }
// fn set_target(x: Read(expr), target) { Read(..x, target:)}

const lens_fields = lens.Lens(get: get_fields, set: set_fields)
fn get_fields(x: Writeite) { x.fields }
fn set_fields(x: Writeite, fields) { Writeite(..x, fields:)}
const lens_imports = lens.Lens(get: get_imports, set: set_imports)
fn get_imports(x: Writeite) { x.imports }
fn set_imports(x: Writeite, imports) { Writeite(..x, imports:)}
const lens_consts = lens.Lens(get: get_consts, set: set_consts)
fn get_consts(x: Writeite) { x.consts }
fn set_consts(x: Writeite, consts) { Writeite(..x, consts:)}
const lens_types = lens.Lens(get: get_types, set: set_types)
fn get_types(x: Writeite) { x.types }
fn set_types(x: Writeite, types) { Writeite(..x, types:)}
const lens_funcs = lens.Lens(get: get_funcs, set: set_funcs)
fn get_funcs(x: Writeite) { x.funcs }
fn set_funcs(x: Writeite, funcs) { Writeite(..x, funcs:)}

pub fn try(
  gen gen: Gen(a, expr),
  cont cont: fn(a) -> Gen(b, expr),
) -> Gen(b, expr) {
  bind(gen, cont)
}

pub fn sequence(
  gens gens: List(Gen(a, expr)),
  cont cont: fn(List(a)) -> Gen(b, expr)
) -> Gen(b, expr) {
  use xs <- bind(
    Gen({
      gens
      |> list.map(fn(gen) { gen.monad })
      |> monad.sequence
    })
  )

  cont(xs)
}

fn bind(
  gen gen: Gen(a, expr),
  cont cont: fn(a) -> Gen(b, expr),
) -> Gen(b, expr) {
  Gen(monad.bind(gen.monad, fn(x) { cont(x).monad }))
}

fn pure(
  val val: val,
) -> Gen(val, expr) {
  Gen(monad.pure(val))
}

fn fail(
  err err: String,
) -> Gen(val, expr) {
  Gen(monad.fail(err))
}

@internal
pub fn read(
  from lens: Lens(Read(expr), v),
  cont cont: fn(v) -> Gen(t, expr)
) -> Gen(t, expr) {
  Gen(monad.read(lens, fn(v) { cont(v).monad }))
}

fn writes(
  at lens: Lens(Writeite, vs),
  cont cont: fn(vs) -> Gen(t, expr)
) -> Gen(t, expr) {
  Gen(monad.writes(lens, fn(w) { cont(w).monad }))
}

fn write(
  val new: v,
  into lens: Lens(Writeite, vs),
  using f: fn(vs, v) -> vs,
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  Gen(monad.write(new, lens, f, fn() { cont().monad }))
}

fn map(
  gen gen: Gen(a, expr),
  apply f: fn(a) -> b,
) -> Gen(b, expr) {
  Gen(monad.map(gen.monad, f))
}

fn map_m(
  list xs: List(a),
  apply f: fn(a) -> Gen(b, expr),
) -> Gen(List(b), expr) {
  Gen(monad.map_m(xs, fn(x) { f(x).monad }))
}

fn ok(
  result result: Result(t, e),
  err err: fn(e) -> String
) -> Gen(t, expr) {
  Gen(monad.ok(result, err))
}

pub fn run_gen(
  gen gen: Gen(t, expr),
  expr expr: expr,
  file file: GleamFile,
  args args: Args,
  opts opts: TypeGenOpts,
  target target: #(GleamPath, String),
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
) -> #(Result(t, String), Writeite) {
  gen.monad
  |> monad.run_(
    read: Read(expr:, file:, args:, opts:, target:, get_type:),
    write: Writeite(fields: set.new(), imports: [], consts: [], types: [], funcs: []),
  )
}

//

pub type Generated(t) {
  Generated(
    def: g.Definition(t),
    overwrite: Bool,
  )
}

pub fn ensure_imports(
  def def: List(g.Definition(g.Import)),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_imports, list.append)
  cont()
}

pub fn ensure_import(
  def def: g.Definition(g.Import),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_imports, list_push)
  cont()
}

pub fn ensure_const(
  def def: g.Definition(g.Constant),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_consts, fn(defs, def) { list_push(defs, Generated(def:, overwrite: False)) })
  cont()
}

pub fn overwrite_const(
  def def: g.Definition(g.Constant),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_consts, fn(defs, def) { list_push(defs, Generated(def:, overwrite: True)) })
  cont()
}


pub fn ensure_custom_type(
  def def: g.Definition(g.CustomType),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_types, fn(defs, def) { list_push(defs, Generated(def:, overwrite: False)) })
  cont()
}

pub fn overwrite_custom_type(
  def def: g.Definition(g.CustomType),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_types, fn(defs, def) { list_push(defs, Generated(def:, overwrite: True)) })
  cont()
}

pub fn ensure_func(
  def def: g.Definition(g.Function),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_funcs, fn(defs, def) { list_push(defs, Generated(def:, overwrite: False)) })
  cont()
}

pub fn overwrite_func(
  def def: g.Definition(g.Function),
  cont cont: fn() -> Gen(t, expr),
) -> Gen(t, expr) {
  use <- write(def, lens_funcs, fn(defs, def) { list_push(defs, Generated(def:, overwrite: True)) })
  cont()
}

pub fn short(field: String) -> g.Field(t) {
  g.ShorthandField(field)
}

//

pub fn named_gen_param(
  name name: String,
  parse f: fn(String) -> Result(t, e),
) -> Gen(Result(t, e), expr) {
  use str <- bind(ensure_named_gen_param_str(name))
  pure(f(str))
}

pub fn ensure_named_gen_param(
  name name: String,
  parse f: fn(String) -> Result(t, String),
) -> Gen(t, expr) {
  use str <- bind(ensure_named_gen_param_str(name))
  ok(f(str), fn(err) {
    "named gen param " <> string.inspect(#(name, str)) <> " parse failed with err: " <> string.inspect(err)
  })
}

pub fn named_gen_param_str(
  name name: String,
) -> Gen(Result(String, Nil), expr) {
  use args <- read(lens_args)
  pure(dict.get(args.named, name))
}

pub fn ensure_named_gen_param_str(
  name name: String,
) -> Gen(String, expr) {
  use args <- read(lens_args)
  use result <- bind(named_gen_param_str(name))
  ok(result, fn(_err) {
    "named gen param " <> string.inspect(name) <> " but got: " <> string.inspect(args)
  })
}

pub fn success(val: t) -> Gen(t, expr) {
  pure(val)
}

pub fn failure(msg: String) -> Gen(t, expr) {
  fail(msg)
}

pub fn file(
  cont cont: fn(GleamFile) -> Gen(t, expr),
) -> Gen(t, expr) {
  use file <- read(lens_file)
  cont(file)
}

pub fn args(
  cont cont: fn(Args) -> Gen(t, expr),
) -> Gen(t, expr) {
  use args <- read(lens_args)
  cont(args)
}

pub fn opts(
  cont cont: fn(TypeGenOpts) -> Gen(t, g.Definition(g.CustomType)),
) -> Gen(t, g.Definition(g.CustomType)) {
  use opts <- read(lens_opts)
  cont(opts)
}

pub fn custom_type(
  cont cont: fn(g.CustomType) -> Gen(t, g.Definition(g.CustomType)),
) -> Gen(t, g.Definition(g.CustomType)) {
  use ct <- custom_type_def()
  cont(ct.definition)
}

pub fn custom_type_def(
  cont cont: fn(g.Definition(g.CustomType)) -> Gen(t, g.Definition(g.CustomType)),
) -> Gen(t, g.Definition(g.CustomType)) {
  use td <- read(lens_expr)
  cont(td)
}

pub fn variant(
  cont cont: fn(g.Variant, TypeDef) -> GenVariantCaseClause(t),
) -> GenVariantCaseClause(t) {
  use #(var, td) <- read(lens_expr)
  cont(var, td)
}

pub fn type_params(
  type_ type_: g.Type,
  cont cont: fn(List(#(g.Type, Result(TypeDef, Nil)))) -> Gen(t, expr),
) -> Gen(t, expr) {
  use params <- bind(for_named_type(type_, fn(nt) { nt.parameters }))

  use types <- bind(
    params
    |> map_m(fn(t) {
      get_type_def_for(t)
      |> map(pair.new(t, _))
    })
  )

  cont(types)
}

pub type NamedType {
  NamedType(
    name: String,
    module: Option(String),
    parameters: List(g.Type),
  )
}

fn for_named_type(
  type_ type_: g.Type,
  apply f: fn(NamedType) -> t,
) -> Gen(t, expr) {
  case type_ {
    g.NamedType(name:, module:, parameters:, ..) ->
      pure(f(NamedType( name:, module:, parameters:)))

    _ ->
      fail("expected a `glance.NamedType`, but got: " <> string.inspect(type_))
  }
}

pub fn pun_variant_named_param(
  name name: String,
  cont cont: fn(g.Field(a), g.Type, Result(TypeDef, Nil)) -> GenVariantCaseClause(t),
) -> GenVariantCaseClause(t) {
  use var, _td <- variant()

  use t <- bind(find_named_param(var, name) |> fn(result) {
    case result {
      Ok(x) -> pure(x)
      Error(_err) -> fail("no parameter named " <> string.inspect(name) <> " on: " <> string.inspect(var))
    }
  })

  use td <- bind(get_type_def_for(t))

  use <- write(name |> list.wrap |> set.from_list, lens_fields, set.union)

  cont(short(name), t, td)
}

fn find_named_param(
  variant variant: g.Variant,
  name name: String,
) -> Result(g.Type, Nil) {
  list.find_map(variant.fields, fn(field) {
    case field {
      g.LabelledVariantField(label:, item:) if label == name -> Ok(item)
      _ -> Error(Nil)
    }
  })
}

fn get_type_def_by(
  mod mod: Option(String),
  type_ type_: String,
) -> Gen(Result(TypeDef, Nil), a) {
  use get_type <- read(lens_get_type)
  pure(get_type(mod, type_))
}

fn get_type_def_for(
  type_ type_: g.Type,
) -> Gen(Result(TypeDef, Nil), a) {
  case type_ {
    g.NamedType(module:, name:, ..) ->
      get_type_def_by(module, name)

    _ ->
      pure(Error(Nil))
  }
}

pub fn variant_clause_success(
  expr expr: g.Expression,
) -> GenVariantCaseClause(g.Clause) {
  use variant, type_def <- variant()
  use fields <- writes(lens_fields)

  use file <- read(lens_file)

  let variant =
    scope_custom_type_variant(
      type_def:,
      variant:,
      in: file,
    )

  let module =
    variant.module
    |> option.map(fn(str) {
      case str |> string.ends_with(".") {
        True -> str
        False -> str <> "." // TODO patch `glance_printer` to handle this correctly
      }
    })

  let pattern = g.PatternVariant(z,
    module:,
    constructor: variant.name,
    arguments: fields |> set.map(g.ShorthandField) |> set.to_list,
    with_spread: set.is_empty(fields),
  )

  pure(g.Clause([[pattern]], None, expr))
}

pub fn variant_clause_failure(
  msg msg: String
) -> GenVariantCaseClause(t) {
  fail(msg)
}

//

pub fn local_custom_type_src(
  type_ type_: g.CustomType,
  cont cont: fn(String) -> Gen(t, expr),
) -> Gen(t, expr) {
  use GleamFile(ast:, src:, ..) <- read(lens_file)

  let t = string.inspect(type_)

  case ast.custom_types |> dict.get(type_.name) {
    Ok(g.Definition(_, g.CustomType(location: span, ..))) ->
      case read_span(src:, span:) {
        Ok(src) ->
          cont(src)

        Error(Nil) ->
          failure("couldn't read local custom type src for span " <> string.inspect(span) <> " in " <> t)
      }

    Error(Nil) ->
      failure("couldn't find local custom type: " <> t)
  }
}

//

pub type FileScoped(t) {
  FileScoped(
    module: Option(String),
    name: String,
  )
}

pub fn scope_custom_type_variant(
  type_def td: TypeDef,
  variant variant: g.Variant,
  in file: GleamFile,
) -> FileScoped(g.Variant) {
  {
    use <- bool.guard(td.path == file.path, Error(Nil))

    use i <- result.try(file.ast.imports |> find_import(td.path))

    use path <- result.try(i.definition.module |> parse_gleam_module_path |> result.replace_error(Nil))

    case i.definition.alias, path.module {
      // use name of import module if not discarded
      Some(g.Named(module)), _ |
      None, module -> {
        let module = Some(module)

        {
          use i <- list.find_map(i.definition.unqualified_values)
          use <- bool.guard(i.name != variant.name, Error(Nil))

          case i.alias {
            None ->
              Ok(FileScoped(module:, name: variant.name))

            Some(unqualified_alias) ->
              Ok(FileScoped(module: None, name: unqualified_alias))
          }
        }
        |> result.or(
          Ok(FileScoped(module:, name: variant.name))
        )
      }

      // use unqualified import value name or alias if discarded
      Some(g.Discarded( _)), _ -> {
        use i <- list.find_map(i.definition.unqualified_values)
        use <- bool.guard(i.name != variant.name, Error(Nil))
        case i.alias {
          None ->
            Ok(FileScoped(module: None, name: variant.name))

          Some(name) ->
            Ok(FileScoped(module: None, name:))
        }
      }
    }
  }
  |> result.unwrap(FileScoped(module: None, name: variant.name))
}

//

pub fn parse_gleam_module_path(
  path path: String,
) -> Result(GleamPath, Nil) {
  case string.split(path, "/") {
    [] -> Error(Nil)
    [package, ..rest] as full->
      case rest |> list.reverse {
        [] -> Ok(GleamPath(package:, module: package, full:))
        [module, ..] -> Ok(GleamPath(package:, module:, full:))
      }
  }
}

//

const z = g.Span(-1, -1)

fn read_span(
  src src: String,
  span span: g.Span,
) -> Result(String, Nil) {
  use <- bool.guard(span.end > string.length(src), Error(Nil))
  Ok(string.slice(src, span.start, span.end - span.start))
}

// TODO rename & mv
pub fn to_glance_type(
  type_ type_: g.CustomType,
  module module: Option(String),
) -> g.Type {
  g.NamedType(z,
    module:,
    name: type_.name,
    parameters: type_.parameters |> list.map(g.VariableType(z, _)),
  )
}

