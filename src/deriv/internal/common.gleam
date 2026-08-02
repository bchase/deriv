import gleam/pair
import gleam/bool
import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import gleam/result.{try}
import gleam/regexp.{type Regexp}
import glance.{type Module, type Definition, type Function, Module, Definition, Function, type CustomType, type Variant, type Constant}
import glance_printer
import deriv/internal/types.{type DerivFieldOpts, type DerivFieldOpt, DerivField, type ModuleReader, type ModuleReaderErr, type Newtype, Newtype}
import deriv/internal/glance as dg
import deriv/internal/common/casing
import gleam/io
import shellout
import simplifile
import tom
import deriv/util

pub const snake_case_to_label = util.snake_case_to_label
pub const snake_case = casing.snake_case
pub const hyphen_case = casing.hyphen_case
pub const pascal_case = casing.pascal_case

pub const gleam_reserved_words = [
  "case", "const", "external", "fn", "import", "let", "opaque", "pub", "type", "use",
  "auto", "delegate", "derive", "else", "implement", "macro", "test", "echo",
]

pub fn gleam_toml() -> Dict(String, tom.Toml) {
  let assert Ok(output) = shellout.command(in: ".", run: "cat", with: ["gleam.toml"], opt: [])
  let assert Ok(config) = tom.parse(output)
  config
}

pub fn gleam_toml_dep_path(
  name name: String,
) -> Result(String, Nil) {
  let gt = gleam_toml()

  use deps <- try(tom.get_table(gt, ["dependencies"]) |> result.replace_error(Nil))
  use dep <- try(dict.get(deps, name))

  use cfg <- try(case dep {
    tom.InlineTable(cfg) -> Ok(cfg)
    _ -> Error(Nil)
  })

  case dict.get(cfg, "path") {
    Ok(tom.String(path)) -> Ok(path)
    _ -> Error(Nil)
  }
}

pub fn indent(str: String, level level: Int) {
  let pad =
    "  "
    |> list.repeat(level)
    |> string.join("")

  pad <> str
}

pub fn replace_function(
  full_src: String,
  func_name func_name: String,
  func_src func_src: String,
) -> String {
  let re = func_start_re(func_name)

  let assert [before, after] =
    regexp.split(re, full_src)
    |> list.filter(fn(str) { str != "pub " && str != "" })

  let new_before = string.trim_end(before)

  let new_after = drop_lines_up_to_and_including_lone_closing_brace(after)

  [ new_before, func_src, new_after ]
  |> string.join("\n\n")
}

pub fn replace_const(
  full_src full_src: String,
  const_name const_name: String,
  const_src const_src: glance.Constant,
) -> String {
  let assert Ok(module) = glance.module(full_src)

  let const_src =
    const_src
    |> glance.Definition([], _)
    |> list.wrap
    |> glance.Module([],[],[],_,[])
    |> glance_printer.print

  {
    use const_def <- try(list.find(module.constants, fn(c) { c.definition.name == const_name }))
    let const_def = const_def.definition

    let #(before, after) = dg.splice_out_span(str: full_src, span: const_def.location)

    Ok(before <> const_src <> after)
  }
  |> result.lazy_unwrap(fn() {
    full_src
    |> string.append("\n" <> const_src)
  })
}

pub fn replace_type(
  full_src: String,
  type_name type_name: String,
  type_src type_src: String,
) -> String {
  let re = type_start_re(type_name)

  let assert [before, after] =
    regexp.split(re, full_src)
    |> list.filter(fn(str) { str != "pub " && str != "" })

  let new_before = string.trim_end(before)

  let new_after = drop_lines_up_to_and_including_lone_closing_brace(after)

  [ new_before, type_src, new_after ]
  |> string.join("\n\n")
}

fn drop_lines_up_to_and_including_lone_closing_brace(str) {
  str
  |> string.split("\n")
  |> list.drop_while(fn(line) { line != "}" })
  |> fn(lines) {

    lines
    |> list.first
    |> fn(line) {
      case line {
        Ok("}") -> lines |> list.drop(1)
        _ -> lines
      }
    }
  }
  |> string.join("\n")
  |> string.trim_start
}

fn func_start_re(func_name: String) -> Regexp {
  let assert Ok(re) =
    { "^(pub )?fn " <> func_name <> "[(].*" }
    |> regexp.compile(regexp.Options(case_insensitive: False, multi_line: True))

  re
}

pub fn update_funcs(init_src: String, funcs: List(#(String, String))) -> String {
  list.fold(funcs, init_src, fn(src, func) {
    let #(func_name, func_src) = func

    let re = func_start_re(func_name)

    case regexp.check(re, src) {
      True ->  replace_function(src, func_name:, func_src:)
      False -> {
        let newlines =
          case string.ends_with(src, "\n") {
            True -> "\n"
            False -> "\n\n"
          }

        src <> newlines <> func_src
      }
    }
  })
}

pub fn update_consts(init_src: String, consts: List(#(String, glance.Constant))) -> String {
  list.fold(consts, init_src, fn(src, const_) {
    let #(const_name, const_src) = const_

    replace_const(full_src: src, const_name:, const_src:)
  })
}

fn type_start_re(type_name: String) -> Regexp {
  let assert Ok(re) =
    { "^(pub\\s*(opaque\\s*)?)?type " <> type_name <> "\\s*{\\s*$" }
    |> regexp.compile(regexp.Options(case_insensitive: False, multi_line: True))

  re
}

pub fn update_types(init_src: String, types: List(#(String, String))) -> String {
  list.fold(types, init_src, fn(src, type_) {
    let #(type_name, type_src) = type_

    let re = type_start_re(type_name)

    case regexp.check(re, src) {
      True -> replace_type(src, type_name:, type_src:)
      False -> {
        let newlines =
          case string.ends_with(src, "\n") {
            True -> "\n"
            False -> "\n\n"
          }

        src <> newlines <> type_src
      }
    }
  })
}

pub fn then(
  result: Result(a, err1),
  fun: fn(a) -> Result(b, err2),
) -> Result(b, Nil) {
  result
  |> result.map_error(log_and_discard_error)
  |> result.try(fn(x) {
    fun(x)
    |> result.map_error(log_and_discard_error)
  })
}
fn log_and_discard_error(err: err) -> Nil {
  debug(err)
  Nil
}

pub fn debug(x: t) -> t {
  io.println(string.inspect(x))

  x
}

pub fn func_name(func: Definition(Function)) -> String {
  let Definition(_, Function(name:, ..)) = func
  name
}

pub fn func_str(func: Definition(Function)) -> String {
  Module([], [], [], [], [func])
  |> glance_printer.print
  |> gleam_format
}

pub fn type_str(type_: Definition(CustomType)) -> String {
  Module([], [type_], [], [], [])
  |> glance_printer.print
  |> gleam_format
}

pub fn const_str(const_: Definition(Constant)) -> String {
  Module([], [], [], [const_], [])
  |> glance_printer.print
  |> gleam_format
}

pub fn gleam_format(src: String) -> String {
  let escaped_src =
    src
    |> string.replace(each: "\"", with: "\\\"")
    |> string.replace(each: "'", with: "\\'")

  let cmd = "echo \"" <> escaped_src <> "\" | gleam format --stdin"
  let assert Ok(formatted_enc) = shellout.command(run: "sh", with: ["-c", cmd], in: ".", opt: [])

  formatted_enc
  |> string.trim
}

pub fn get_field_opts(
  opts opts: DerivFieldOpts,
  type_ type_: CustomType,
  variant variant: Variant,
  field field: String,
) -> List(DerivFieldOpt) {
  let key =
    DerivField(
      type_: type_.name,
      variant: variant.name,
      field:,
    )

  opts
  |> dict.get(key)
  |> result.unwrap([])
}

pub fn get_field_opts_(
  opts opts: DerivFieldOpts,
  type_ type_: String,
  variant variant: String,
  field field: String,
) -> List(DerivFieldOpt) {
  let key =
    DerivField(type_:, variant:, field:)

  opts
  |> dict.get(key)
  |> result.unwrap([])
}

pub fn get_field_opt(
  opts opts: DerivFieldOpts,
  type_ type_: String,
  variant variant: String,
  field field: String,
  err_msg err: String,
  matching matching: fn(DerivFieldOpt) -> Result(t, Nil),
) -> Result(t, Nil) {
  get_field_opts_(opts:, type_:, variant:, field:)
  |> list.filter_map(matching)
  |> fn(xs) {
    case xs {
      [] -> Error(Nil)
      [x] -> Ok(x)
      _ -> panic as err
    }
  }
}

pub type BirlTimeKind {
  BirlTimeISO8601
  BirlTimeNaive
  BirlTimeHTTP
  BirlTimeUnix
  BirlTimeUnixMilli
  BirlTimeUnixMicro
}

pub fn birl_time_kind(
  type_: CustomType,
  variant: Variant,
  field: String,
  all_field_opts: DerivFieldOpts,
) -> BirlTimeKind {
  case get_field_opts(all_field_opts, type_, variant, field) {
    [] -> BirlTimeISO8601 // TODO warning to specify
    opts ->
      opts
      |> list.find_map(fn(opt) {
        case opt.strs {
          ["json", "birl", val] -> Ok(val)
          _ -> Error(Nil)
        }
      })
      |> result.unwrap("iso8601")
      |> fn(val) {
        case val {
          "iso8601" -> BirlTimeISO8601
          "naive" -> BirlTimeNaive
          "http" -> BirlTimeHTTP
          "unix" -> BirlTimeUnix
          "unix_milli" -> BirlTimeUnixMilli
          "unix_micro" -> BirlTimeUnixMicro
          _ -> panic as { "Not a supported `json(birl(VALUE))`: " <> val }
        }
      }
  }
}

pub fn fetch_module(path: String) -> Result(Module, ModuleReaderErr) {
  fetch_module_(path_prefix: "src/", path:)
  |> result.lazy_or(fn() {
    fetch_module_in_dependencies(path:)
  })
}

fn fetch_module_in_dependencies(
  path path: String,
) -> Result(Module, ModuleReaderErr) {
  let assert Ok(package) = path |> string.split("/") |> list.first

  case gleam_toml_dep_path(name: package) {
    Error(_) -> fetch_module_in_build_packages(path:, package:)
    Ok(dir_path) -> fetch_module_(path_prefix: dir_path <> "/src/", path:)
  }
}

fn fetch_module_in_build_packages(
  path path: String,
  package package: String
) -> Result(Module, ModuleReaderErr) {
  let path_prefix = "build/packages/" <> package <> "/src"

  fetch_module_(path_prefix:, path:)
}

pub fn fetch_module_(
  path path: String,
  path_prefix path_prefix: String,
) -> Result(Module, ModuleReaderErr) {
  fetch_module_in_project(path:, path_prefix:)
}

fn fetch_module_in_project(
  path path: String,
  path_prefix prefix: String,
) -> Result(Module, ModuleReaderErr) {
  let filepath = prefix <> path <> ".gleam"
  use src <- result.try(simplifile.read(filepath) |> result.map_error(types.FileErr))
  use module <- result.try(glance.module(src) |> result.map_error(types.GlanceErr))

  Ok(module)
}

pub fn fetch_custom_type(
  ident ident: String,
  read_module read_module: ModuleReader,
) -> Result(#(String, glance.Definition(glance.CustomType)), ModuleReaderErr) {
  fetch_custom_type_in_project(ident:, read_module:)
  |> result.lazy_or(fn() {
    fetch_custom_type_in_dependencies(ident:, read_module:)
  })
}

pub fn fetch_newtype(
  ident ident: String,
  read_module read_module: ModuleReader,
) -> Result(Newtype, ModuleReaderErr) {
  fetch_custom_type(ident:, read_module:)
  |> result.map(fn(type_) {
    case type_ |> pair.map_second(fn(def) { def.definition }) {
      #(module, glance.CustomType(variants: [variant], ..) as type_)->
        case variant.fields {
          [field] ->
            case field {
              glance.LabelledVariantField(label: field_access, item: wrapping) ->
                Newtype(module:, type_:, wrapping:, constr: variant.name, field_access:)

              _ -> panic as {
                "`newtype` currently only supports `LabelledVariantField` -- " <> string.inspect(variant)
              }
            }

          _ -> panic as {
            "`newtype` indicated, but variant has more than one field: " <> string.inspect(variant)
          }
        }
      _ -> panic as {
        "`newtype` indicated, but custom type is not monovariant: " <> string.inspect(type_)
      }
    }
  })
  // |> result.map
}

fn fetch_custom_type_in_project(
  ident ident: String,
  read_module read_module: ModuleReader,
) -> Result(#(String, glance.Definition(glance.CustomType)), ModuleReaderErr) {
  use #(module_name, ref) <- result.try(parse_ident(ident))
  use module <- result.try(read_module(module_name))
  use type_ <- result.try(find_custom_type(ref, module))

  Ok(#(module_name, type_))
}

fn fetch_custom_type_in_dependencies(
  ident ident: String,
  read_module read_module: ModuleReader,
) -> Result(#(String, glance.Definition(glance.CustomType)), ModuleReaderErr) {
  use #(module_name, ref) <- result.try(parse_ident(ident))
  use module <- result.try(read_module(module_name))
  use type_ <- result.try(find_custom_type(ref, module))

  Ok(#(module_name, type_))
}

pub fn fetch_function(
  ident: String,
  read_module: ModuleReader,
) -> Result(#(String, glance.Definition(glance.Function)), ModuleReaderErr) {
  use #(module_name, ref) <- result.try(parse_ident(ident))
  use module <- result.try(read_module(module_name))
  use type_ <- result.try(find_function(ref, module))

  Ok(#(module_name, type_))
}

fn parse_ident(ident: String) -> Result(#(String, String), ModuleReaderErr) {
  case string.split(ident, ".") {
    [a, b] -> Ok(#(a, b))
    _ -> Error(types.BadIdent(ident))
  }
}

pub fn find_import(
  module_name module_name: String,
  module module: glance.Module,
) -> Result(glance.Definition(glance.Import), ModuleReaderErr) {
  module.imports
  |> list.find(fn(i) { i.definition.module == module_name })
  |> result.replace_error(types.NotFoundErr(module_name))
}

fn find_custom_type(
  ref: String,
  module: glance.Module,
) -> Result(glance.Definition(glance.CustomType), ModuleReaderErr) {
  module.custom_types
  |> list.find(fn(type_) { type_.definition.name == ref })
  |> result.replace_error(types.NotFoundErr(ref))
}

fn find_function(
  ref: String,
  module: glance.Module,
) -> Result(glance.Definition(glance.Function), ModuleReaderErr) {
  module.functions
  |> list.find(fn(func) { func.definition.name == ref })
  |> result.replace_error(types.NotFoundErr(ref))
}

pub fn diff(
  str1: String,
  str2: String,
) -> String {
  let assert Ok(file_prefix) = shellout.command(
    run: "date",
    with: ["+%s"],
    in: ".",
    opt: []
  )

  let assert Ok(file_prefix) = file_prefix |> string.split("\n") |> list.first
  let file_prefix = "/tmp/" <>  file_prefix
  let file_name1 = file_prefix <> "1"
  let file_name2 = file_prefix <> "2"

  let assert Ok(_) = simplifile.write(to: file_name1, contents: str1)
  let assert Ok(_) = simplifile.write(to: file_name2, contents: str2)

  let diff =
    case shellout.command(run: "diff", with: [file_name1, file_name2], in: ".", opt: []) {
      Ok(diff) -> diff
      Error(#(_, diff)) -> diff
    }

  let assert Ok(_) = simplifile.delete(file_name1)
  let assert Ok(_) = simplifile.delete(file_name2)

  diff
}

pub fn are_any_fields_options(
  type_: CustomType,
) -> Bool {
  type_.variants
  |> list.any(fn(variant) {
    variant.fields
    |> list.any(fn(field) {
      case field.item {
        glance.NamedType(name: "Option", parameters:[_], ..) -> True
        _ -> False
      }
    })
  })
}

pub fn are_any_fields_sets(
  type_: CustomType,
) -> Bool {
  type_.variants
  |> list.any(fn(variant) {
    variant.fields
    |> list.any(fn(field) {
      case field.item {
        glance.NamedType(name: "Set", parameters:[_], ..) -> True
        _ -> False
      }
    })
  })
}

pub fn any_raw_field_options_match(
  opts opts: DerivFieldOpts,
  re re: regexp.Regexp,
) -> Bool {
  opts
  |> dict.values
  |> list.flatten
  |> list.any(fn(opt) { opt.raw |> regexp.check(re, _) })
}

pub fn are_any_fields_non_string_basic_type_dict_keys(
  type_: CustomType,
) -> Bool {
  type_.variants
  |> list.any(fn(variant) {
    variant.fields
    |> list.any(fn(field) {
      has_non_string_basic_type_dict_keys(field.item)
    })
  })
}

pub fn import_(
  module module: String,
) -> glance.Import {
  import__(module:, as_: None, types: [], values: [])
}

pub fn import__(
  module module: String,
  as_ alias: Option(String),
  types types: List(String),
  values values: List(String),
) -> glance.Import {
  let alias = alias |> option.map(glance.Named)

  glance.Import(
    location: dummy_location(),
    module:,
    alias:,
    unqualified_values: values |> list.map(unq_import),
    unqualified_types: types |> list.map(unq_import),
  )
}

fn unq_import(
  str str: String,
) -> glance.UnqualifiedImport {
  glance.UnqualifiedImport(name: str, alias: None)
}

pub fn util_import() -> glance.Import {
  import_(module: "deriv/util")
}

pub fn dict_import_with_class() -> glance.Import {
  import__(module: "gleam/dict", as_: None, types: ["Dict"], values: [])
}

pub fn none_constr_import() -> glance.Import {
  glance.Import(
    location: dummy_location(),
    module: "gleam/option",
    alias: None,
    unqualified_values: [
      glance.UnqualifiedImport(
        name: "None",
        alias: None,
      ),
    ],
    unqualified_types: [],
  )
}

pub fn dummy_location() -> glance.Span {
  x
}

//

pub fn mono_variant_or_panic(
  type_ type_: types.Type,
  deriv_name deriv_name: String,
) -> #(glance.CustomType, glance.Variant) {
  case type_ {
    types.Type(type_: glance.CustomType(variants: [variant], ..) as type_) -> {
     #(type_, variant)
    }

    types.TypeAlias(..) -> {
      panic as { "`" <> deriv_name <> "` doesn't know how to handle type aliases, namely: " <> string.inspect(type_) }
    }

    types.Type(type_: glance.CustomType(variants: [], ..)) -> {
      panic as { "`" <> deriv_name <> "` doesn't know how to handle types without any variants, namely: " <> string.inspect(type_) }
    }

    types.Type(..) -> {
      panic as { "`" <> deriv_name <> "` doesn't know how to handle multi-variant types, namely: " <> string.inspect(type_) }
    }
  }
}

pub fn custom_type_or_panic(
  type_ type_: types.Type,
  deriv_name deriv_name: String,
) -> glance.CustomType {
  case type_ {
    types.Type(type_:) -> {
      type_
    }

    types.TypeAlias(..) -> {
      panic as { "`" <> deriv_name <> "` doesn't know how to handle type aliases, namely: " <> string.inspect(type_) }
    }
  }
}

pub fn is_multi_variant(
  type_ type_: CustomType,
) -> Bool {
  { type_.variants |> list.length } >= 2
}

pub fn has_non_string_basic_type_dict_keys(
  type_ type_: glance.Type,
) -> Bool {
  case type_ {
    glance.NamedType(
      name: "Dict",
      parameters: [
        glance.NamedType(name: "String", parameters: [], ..),
        _,
      ],
    ..) -> False

    glance.NamedType(name: "Dict", parameters: [key, _val,], ..) ->
      case key {
        glance.NamedType(name:, parameters: params, ..) ->
          case name, params {
            "Int", [] | "Float", [] | "Bool", [] -> True
            _, _ -> False
          }
        _ -> False
      }

    _ -> False
  }
}

const x = glance.Span(start: -1, end: -1)

pub fn gtype_(
  module module: Option(String),
  name name: String,
  params parameters: List(glance.Type),
) -> glance.Type {
  glance.NamedType(x, name:, module:, parameters:)
}

pub fn gtype(
  name name: String,
  params params: List(glance.Type),
) -> glance.Type {
  gtype_(name:, params:, module: None)
}


pub type ImportedType {
  InScope(name: String, module: String, curr_module: Bool)
  Qualified(name: String, module: String)
}

pub fn build_imported_type(
  module_name module_name: String,
  type_ type_: CustomType,
  file file: types.File,
) -> ImportedType {
  let assert Ok(m) = glance.module(file.src)

  let assert Ok(module) =
    module_name
    |> string.split("/")
    |> list.last

  let is_curr_module = module_name == file.module
  use <- bool.guard(is_curr_module, InScope(name: type_.name, module:, curr_module: True))

  case find_import(module_name:, module: m) {
    Error(_) -> panic as {
      // TODO auto import
      file.module <> "\n" <>
      "missing import for: " <> module_name <> "." <> type_.name
    }

    Ok(glance.Definition(_, import_)) -> {
      let is_in_scope =
        import_.unqualified_types
        |> list.any(fn(t) { t.name == type_.name })

      use <- bool.guard(is_in_scope, InScope(name: type_.name, module:, curr_module: False))

      let module =
        case import_.alias {
          Some(glance.Discarded(..)) -> panic as {
            file.module <> "\n" <>
            "neither exposing type nor providing module for: " <> module_name <> "." <> type_.name
          }
          None -> module
          Some(glance.Named(module)) -> module
        }

      Qualified(module:, name: type_.name)
    }
  }
}

pub fn starts_with_uppercase(str: String) -> Bool {
  str
  |> string.first
  |> result.map(fn(ch) { ch == string.uppercase(ch) })
  |> result.unwrap(False)
}

pub fn consolidate_imports_for(src: String, add add_imports: List(glance.Import)) -> String {
  let assert Ok(module) =
    case glance.module(src) {
      Error(err) -> {
        debug(err)
        io.println(src)
        panic as "Failed to parse the above source with `glance.module`"
      }
      ok -> ok
    }

  let curr_imports =
    module.imports
    |> list.map(fn(d) { d.definition })

  let new_imports =
    [ curr_imports, add_imports ]
    |> list.flatten
    |> consolidate_imports
    |> list.map(import_src)
    |> string.join("\n")
    |> string.trim

  let src_without_imports =
    src
    |> string.split("\n")
    |> list.reverse
    |> list.take_while(fn(str) { !string.starts_with(str, "import") })
    |> list.reverse
    |> string.join("\n")
    |> string.trim

  [
    new_imports,
    src_without_imports,
  ]
  |> string.join("\n\n")
}

pub fn consolidate_imports(all_imports: List(glance.Import)) -> List(glance.Import) {
  all_imports
  |> list.group(fn(i) { i.module })
  |> dict.to_list
  |> list.map(fn(x) {
    let #(module, imports) = x

    let alias =
      imports
      |> list.map(fn(i) { i.alias })
      |> option.values
      |> fn(aliases) {
        case list.unique(aliases) {
          [] -> None
          [alias] -> Some(alias)
          _ -> panic as {
            debug(aliases)
            "0 or 1 aliases allowed, but for module `" <> module <> "` multiple aliases found (see above)"
          }
        }
      }

    let unqualified_types =
      imports
      |> list.flat_map(fn(i) { i.unqualified_types })
      |> list.unique

    let unqualified_values =
      imports
      |> list.flat_map(fn(i) { i.unqualified_values })
      |> list.unique

    glance.Import(
      location: dummy_location(),
      module:,
      alias:,
      unqualified_types:,
      unqualified_values:,
    )
  })
  |> list.sort(fn(a,b) { string.compare(a.module, b.module) })
}

fn import_src(i: glance.Import) -> String {
  let alias =
    case i.alias {
      Some(glance.Named(name)) -> "as " <> name
      Some(glance.Discarded(name)) -> "as _" <> name
      None -> ""
    }

  let types =
    i.unqualified_types
    |> list.sort(fn(a,b) { string.compare(a.name, b.name) })
    |> list.map(unqualified_import_str(_, is_type: True))

  let funcs =
    i.unqualified_values
    |> list.sort(fn(a,b) { string.compare(a.name, b.name) })
    |> list.map(unqualified_import_str(_, is_type: False))

  let types_and_constructors =
    case list.append(types, funcs) {
      [] -> ""
      xs -> {
        let str = string.join(xs, ", ")

        ".{" <> str <> "}"
      }
    }

  let module_with_types_and_constructors =
    i.module <> types_and_constructors

  [
    "import",
    module_with_types_and_constructors,
    alias,
  ]
  |> list.filter(fn(str) { str != "" })
  |> string.join(" ")
}

fn unqualified_import_str(uqi: glance.UnqualifiedImport,  is_type is_type: Bool) -> String {
  let type_ =
    case is_type {
      True -> "type "
      False -> ""
    }

  let alias =
    case uqi.alias {
      Some(alias) -> " as " <> alias
      None -> ""
    }

  type_ <> uqi.name <> alias
}
