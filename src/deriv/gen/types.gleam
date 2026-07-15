import bchase/lens.{type Lens}
import gleam/string
import gleam/set.{type Set}
import bchase/function.{x}
import bchase/list.{at as list_at} as _
import deriv/internal/glance.{z} as _
import glance as g
import gleam/list
import gleam/dict.{type Dict}
import gleam/option.{None, type Option}
import gleam/pair
import gleam/result
import deriv/internal/monad

pub fn relative_hot_code_reload_dir_path() -> String {
  "src/deriv/gen/reload/"
}

pub fn relative_code_gen_defs_path() -> String {
  relative_hot_code_reload_dir_path() <> "defs.gleam"
}

pub type ExprGen {
  VariantClauseCaseExprGen(clauses: List(VariantExpr))
}

pub type TypeDef {
  TypeDef(
    path: GleamPath,
    def: g.Definition(g.CustomType),
  )
}

// pub type ExprGenRef {
//   ExprGenRef(
//     module: String,
//     func: String,
//   )
// }

// pub fn expr_gen_ref(
//   path path: GleamPath,
//   func func: String,
// ) -> ExprGenRef {
//   ExprGenRef(
//     module: path.full |> string.join("/"),
//     func:,
//   )
// }

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

pub fn all_imports(
  imports imports: Imports,
) -> List(g.Definition(g.Import)) {
  [
    imports.discarded,
    imports.named |> dict.values,
  ]
  |> list.flatten
}

//

pub opaque type VariantExpr {
  VariantExpr(
    run: fn(
      #(GleamPath,String),
      g.Variant,
      String,
      fn(Option(String), String) -> Result(TypeDef, Nil),
      Set(String), // NOTE: fields acc, used to detect need for `with_spread`
      List(EnsureFunc),
    ) -> Result(#(g.Expression, Set(String), List(EnsureFunc)), String),
  )
}

pub opaque type Gen(t, expr) {
  Gen(monad: monad.ReadWriteResult(t, String, GenRead(expr), GenWrite))
}

pub opaque type GenRead(expr) {
  GenRead(
    expr: expr,
    args: String,
    module_func: #(GleamPath, String),
    get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
  )
}

pub opaque type GenWrite {
  GenWrite(
    fields: Set(String), // NOTE: fields acc, used to detect need for `with_spread`
    funcs: List(EnsureFunc),
  )
}

const lens_expr = lens.Lens(get: get_expr, set: set_expr)
fn get_expr(x: GenRead(expr)) { x.expr }
fn set_expr(x: GenRead(expr), expr) { GenRead(..x, expr:)}
const lens_args = lens.Lens(get: get_args, set: set_args)
fn get_args(x: GenRead(expr)) { x.args }
fn set_args(x: GenRead(expr), args) { GenRead(..x, args:)}
const lens_module_func = lens.Lens(get: get_module_func, set: set_module_func)
fn get_module_func(x: GenRead(expr)) { x.module_func }
fn set_module_func(x: GenRead(expr), module_func) { GenRead(..x, module_func:)}
const lens_get_type = lens.Lens(get: get_get_type, set: set_get_type)
fn get_get_type(x: GenRead(expr)) { x.get_type }
fn set_get_type(x: GenRead(expr), get_type) { GenRead(..x, get_type:)}

const lens_fields = lens.Lens(get: get_fields, set: set_fields)
fn get_fields(x: GenWrite) { x.fields }
fn set_fields(x: GenWrite, fields) { GenWrite(..x, fields:)}
const lens_funcs = lens.Lens(get: get_funcs, set: set_funcs)
fn get_funcs(x: GenWrite) { x.funcs }
fn set_funcs(x: GenWrite, funcs) { GenWrite(..x, funcs:)}

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

fn read(
  from lens: Lens(GenRead(expr), v),
  cont cont: fn(v) -> Gen(t, expr)
) -> Gen(t, expr) {
  Gen(monad.read(lens, fn(v) { cont(v).monad }))
}

fn writes(
  at lens: Lens(GenWrite, vs),
  cont cont: fn(vs) -> Gen(t, expr)
) -> Gen(t, expr) {
  Gen(monad.writes(lens, fn(w) { cont(w).monad }))
}

fn write(
  val new: v,
  into lens: Lens(GenWrite, vs),
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

fn run_gen(
  gen gen: Gen(g.Expression, expr),
  expr expr: expr,
  args args: String,
  module_func module_func: #(GleamPath, String),
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
) -> #(Result(g.Expression, String), GenWrite) {
  gen.monad
  |> monad.run_(
    read: GenRead(expr:, args:, module_func:, get_type:),
    write: GenWrite(fields: set.new(), funcs: []),
  )
}

//

pub type EnsureFunc {
  EnsureFunc(
    def: g.Definition(g.Function),
  )
}

// pub fn parse(variant variant: g.Variant, decoder decoder: VariantExpr) {
//   todo
// }

// fn variant_shorthand_field_type(
//   name name: String,
//   cont cont: fn(g.Type) -> VariantExpr,
// ) -> VariantExpr {
//   VariantExpr(fn(variant, args, get_type, fields) {
//     use #(field, f) <- result.try(
//       variant.fields
//       |> list.find_map(fn(field) {
//         case field {
//           g.LabelledVariantField(label:, item:) if label == name ->
//             Ok(#(name, item))

//           _ -> Error(Nil)
//         }
//       }),
//     )

//     cont(f).run(variant, args, get_type, [field, ..fields])
//   })
// }

pub fn try(
  result result: Result(t, String),
  cont cont: fn(t) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(mf, variant, args, get_type, fields, funcs) {
    case result {
      Error(err) -> Error(err)
      Ok(x) -> cont(x).run(mf, variant, args, get_type, fields, funcs)
    }
  })
}

pub fn ensure_func(
  def def: g.Definition(g.Function),
  cont cont: fn() -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(mf, variant, args, get_type, fields, funcs) {
    cont().run(mf, variant, args, get_type, fields, funcs |> list.append([EnsureFunc(def:)]))
  })
}

pub fn variant_shorthand_field(
  name name: String,
  cont cont: fn(g.Field(g.Expression)) -> VariantExpr,
) -> VariantExpr  {
  variant_shorthand_field_map(name, x(short, pair.first), cont)
}

pub fn variant_shorthand_type(
  name name: String,
  cont cont: fn(g.Type) -> VariantExpr,
) -> VariantExpr {
  variant_shorthand_field_map(name, pair.second, cont)
}

pub fn type_params(
  type_ type_: g.Type,
  cont cont: fn(List(#(g.Type, Result(TypeDef, Nil)))) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(mf, variant, args, get_type, fields, funcs) {
    case type_ {
      g.NamedType(parameters:, ..) ->
        parameters
        |> list.map(fn(type_) {
          case type_ {
            g.NamedType(module:, name:, ..) ->
              get_type(module, name)

            _ ->
              Error(Nil)
          }
          |> pair.new(type_, _)
        })
        |> fn(x) {
          cont(x).run(mf, variant, args, get_type, fields, funcs)
        }

      _ ->
        Error("variant param types needs `NamedType`, got: " <> string.inspect(type_))
    }
  })
}

// fn variant_foo(
//   // name name: String,
//   type_ type_: g.Type,
//   apply f: fn(#(String, g.Type)) -> t,
//   cont cont: fn(t) -> VariantExpr,
// ) -> VariantExpr {
//   VariantExpr(fn(variant, args, get_type, orig_fields) {
//     use type_ <- result.try(get_named_param(variant:, name:))

//     // variant_shorthand_field_map(name, function.identity)
//     todo
//   })
// }

fn variant_shorthand_field_map(
  name name: String,
  apply f: fn(#(String, g.Type)) -> t,
  cont cont: fn(t) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(gen_name, variant, args, get_type, fields, funcs) {
    use type_ <- result.try(get_named_param(variant:, name:) |> result.map_error(fn(_) {
      "couldn't fine named param " <> string.inspect(name) <> " in: " <> string.inspect(variant)
    }))

    cont(f(#(name, type_))).run(gen_name, variant, args, get_type, fields |> set.insert(name), funcs)
  })
}

fn get_named_param(
  variant variant: g.Variant,
  name name: String,
) -> Result(g.Type, Nil) {
  variant.fields
  |> list.find_map(fn(field) {
    case field {
      g.LabelledVariantField(label:, item:) if label == name -> Ok(item)

      _ -> Error(Nil)
    }
  })
}

pub fn variant_success(expr expr: g.Expression) -> VariantExpr {
  VariantExpr(fn(_, _, _, _, fields, funcs) { Ok(#(expr, fields, funcs)) })
}

pub fn variant_failure(msg: String) -> VariantExpr {
  VariantExpr(fn(_, _, _, _, _, _) { Error(msg) })
}

pub fn run_variant_expr(
  ve: VariantExpr,
  mf mf: #(GleamPath, String),
  variant variant: g.Variant,
  args args: String,
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
) -> Result(#(g.Clause, List(EnsureFunc)), String) {
  ve.run(mf, variant, args, get_type, set.new(), [])
  |> result.map(fn(t) {
    let #(expr, fields, funcs) = t

    let with_spread = list.length(variant.fields) > set.size(fields)
    let arguments = fields |> set.map(g.ShorthandField) |> set.to_list

    let pattern =
      g.PatternVariant(z, None, variant.name, arguments:, with_spread:)

    #(g.Clause(patterns: [[pattern]], guard: None, body: expr), funcs)
  })
}

pub fn variant_name(
  cont cont: fn(String) -> VariantExpr,
) -> VariantExpr {
  VariantExpr(fn(mf, variant, args, get_type, fields, funcs) {
    cont(variant.name).run(mf, variant, args, get_type, fields, funcs)
  })
}

pub fn short(field: String) -> g.Field(t) {
  g.ShorthandField(field)
}

//

pub fn run_variant_expr_(
  gen: Gen(g.Expression, g.Variant),
  variant variant: g.Variant,
  args args: String,
  mf mf: #(GleamPath, String),
  get_type get_type: fn(Option(String), String) -> Result(TypeDef, Nil),
) -> Result(#(g.Clause, List(EnsureFunc)), String) {
  let #(result, GenWrite(fields:, funcs:)) =
    run_gen(gen, variant, args, mf, get_type)

  result
  |> result.map(fn(expr) {
    let with_spread = list.length(variant.fields) > set.size(fields)
    let arguments = fields |> set.map(g.ShorthandField) |> set.to_list

    let pattern =
      g.PatternVariant(z, None, variant.name, arguments:, with_spread:)

    #(g.Clause(patterns: [[pattern]], guard: None, body: expr), funcs)
  })
}

pub fn success(val: t) -> Gen(t, expr) {
  pure(val)
}

pub fn failure(msg: String) -> Gen(t, expr) {
  fail(msg)
}

pub fn variant(
) -> Gen(g.Variant, g.Variant) {
  use g.Variant(..) as var <- read(lens_expr)
  pure(var)
}

pub fn type_params_(
  type_ type_: g.Type,
) -> Gen(List(#(g.Type, Result(TypeDef, Nil))), expr) {
  use params <- bind(for_named_type(type_, fn(nt) { nt.parameters }))

  params
  |> map_m(fn(t) {
    get_type_def_for(t)
    |> map(pair.new(t, _))
  })
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

fn get_named_param_(
  name name: String,
) -> Gen(#(g.Type, Result(TypeDef, Nil)), g.Variant) {
  use var <- bind(variant())

  use t <- bind(get_named_param(var, name) |> fn(result) {
    case result {
      Ok(x) -> pure(x)
      Error(_err) -> fail("no parameter named " <> string.inspect(name) <> " on: " <> string.inspect(var))
    }
  })

  use td <- bind(get_type_def_for(t))

  pure(#(t, td))
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
