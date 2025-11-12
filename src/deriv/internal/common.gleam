import gleam/option.{type Option, None}
import gleam/dict
import gleam/list
import gleam/string
import gleam/result
import gleam/regexp.{type Regexp}
import glance.{type Module, type Definition, type Function, Module, Definition, Function, type CustomType, type Variant}
import glance_printer
import deriv/internal/types.{type DerivFieldOpts, type DerivFieldOpt, DerivField, type ModuleReader, type ModuleReaderErr}
import gleam/io
import shellout
import simplifile

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

pub fn gleam_format(src: String) -> String {
  let escaped_src =
    src
    |> string.replace(each: "\"", with: "\\\"")
    |> string.replace(each: "'", with: "\\'")

  let cmd = "echo \"" <> escaped_src <> "\" | gleam format --stdin"
  let assert Ok(formatted_enc) = shellout.command(run: "sh", with: ["-c", cmd], in: ".", opt: [])

  formatted_enc
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
  matching matching: fn(List(String)) -> Result(t, Nil),
) -> Result(t, Nil) {
  get_field_opts_(opts:, type_:, variant:, field:)
  |> list.map(fn(opt) { opt.strs })
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
}

pub fn fetch_module_(
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
  glance.Span(start: -1, end: -1)
}

// FORM

pub fn snake_case_to_label(
  str str: String,
) -> String {
  str
  |> string.split("_")
  |> list.map(string.capitalise)
  |> string.join(" ")
}

// // // CASE HELPERS // // //

pub fn pascal_case(str: String) -> String {
  str
  |> string.split("_")
  |> list.map(string.capitalise)
  |> string.join("")
}

pub fn snake_case(str: String) -> String {
  let assert Ok(capital_re) = regexp.from_string("[A-Z]")
  let assert Ok(initial_underscore_re) = regexp.from_string("^[_]")

  str
  |> regexp.match_map(each: capital_re, in: _, with: fn(match) {
    match.content
    |> string.lowercase
    |> string.append(to: "_", suffix: _)
  })
  |> regexp.replace(each: initial_underscore_re, in: _, with: "")
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
