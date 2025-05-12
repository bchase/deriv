import gleam/option.{type Option, Some, None}
import gleam/dynamic
import gleam/dict
import gleam/list
import gleam/string
import gleam/json.{type Json}
import gleam/result.{try}
import gleam/regexp.{type Regexp}
import decode.{type Decoder}
import youid/uuid.{type Uuid}
import glance.{type Module, type Definition, type Function, Module, Definition, Function, type CustomType, type Variant, CustomType}
import glance_printer
import shellout
import simplifile
import birl.{type Time}
import deriv/types.{type DerivFieldOpts, type DerivFieldOpt, DerivField, type ModuleReader, type ModuleReaderErr}
import gleam/io

pub fn decode_type_field(
  variant variant: String,
  json_field json_field: String,
  pass decoder: Decoder(t),
) -> Decoder(t) {
  decode.into({
    use type_field <- decode.parameter
    type_field
  })
  |> decode.field(json_field, decode.string)
  |> decode.then(fn(type_field) {
    case type_field == variant {
      True -> decoder
      False ->
        { "`" <> variant <> "` failed to match `" <> json_field <> "`" }
        |> decode.fail
    }
  })
}

pub fn decoder_uuid() -> Decoder(Uuid) {
  use str <- decode.then(decode.string)
  case uuid.from_string(str) {
    Ok(uuid) -> decode.into(uuid)
    Error(Nil) -> decode.fail("Failed to parse UUID")
  }
}

pub fn encode_uuid(uuid: Uuid) -> Json {
  uuid
  |> uuid.to_string
  |> string.lowercase
  |> json.string
}

pub fn indent(str: String, level level: Int) {
  let pad =
    "  "
    |> list.repeat(level)
    |> string.join("")

  pad <> str
}

pub fn snake_case(str: String) -> String {
  let assert Ok(re) = regexp.from_string("[A-Z]")

  let step_snake_case = fn(state, char) { step_snake_case(re, state, char) }

  str
  |> string.split("")
  |> list.reverse
  |> list.fold(SC(acc: [], curr: [], next_is_capital: True), step_snake_case)
  |> fn(sc) {
    case sc.curr {
      [] -> sc
      _ -> SC(..sc, acc: list.append(sc.acc, [sc.curr]))
    }
    |> fn(sc) {
      case sc.acc {
        [[last, second_to_last, ..rest], ..rest_chunks] -> {
          let last_is_uppercase = last == string.uppercase(last)
          let second_to_last_is_lowercase = second_to_last == string.lowercase(second_to_last)

          case last_is_uppercase && second_to_last_is_lowercase {
            False -> sc.acc
            True -> {
              let second_to_last_chunk = [second_to_last, ..rest]
              let last_chunk = [last]

              [last_chunk, second_to_last_chunk, ..rest_chunks]
            }
          }
        }

        _ -> sc.acc
      }
    }
    |> list.map(fn(group) {
      group
      |> list.reverse
      |> string.join("")
      |> string.lowercase
    })
    |> list.reverse
    |> string.join("_")
  }
}

type SC {
  SC(
    acc: List(List(String)),
    curr: List(String),
    next_is_capital: Bool,
  )
}

fn step_snake_case(is_capital: Regexp, state: SC, char: String) -> SC {
  let SC(acc:, curr:, next_is_capital:) = state

  let char_is_capital = regexp.check(is_capital, char)

  case char_is_capital, next_is_capital {
    True, False -> {
      SC(
        acc: list.append(acc, [list.append(curr, [char])]),
        curr: [],
        next_is_capital: char_is_capital,
      )
    }

    True, True -> {
      SC(
        acc: acc,
        curr: list.append(curr, [char]),
        next_is_capital: !char_is_capital,
      )
    }

    _, _ -> {
      SC(
        acc: acc,
        curr: list.append(curr, [char]),
        next_is_capital: char_is_capital,
      )
    }
  }
}

pub fn pascal_case(str: String) -> String {
  str
  |> string.split("_")
  |> list.map(string.capitalise)
  |> string.join("")
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
  |> result.then(fn(x) {
    fun(x)
    |> result.map_error(log_and_discard_error)
  })
}
fn log_and_discard_error(err: err) -> Nil {
  io.debug(err)
  Nil
}

// pub fn rewrite_functions_to_file(filepath: String, funcs: List(#(String, String))) -> Result(Nil, Nil) {
//   use old_src <- then(simplifile.read(filepath))

//   let assert Ok(module) = glance.module(old_src)

//   list.each(funcs, fn(x) {
//     let #(func_name, func_src) = x

//     let new_src =
//     case string.contains(old_src, "fn " <> func_name) {
//       True -> replace_function(module, old_src, func_name:, func_src:)
//       False ->
//         [
//           old_src,
//           func_src,
//         ]
//         |> string.join("\n\n")
//     }

//     simplifile.write(filepath, new_src)
//   })

//   Ok(Nil)
// }

pub fn func_name(func: Definition(Function)) -> String {
  let Definition(_, Function(name:, ..)) = func
  name
}

pub fn func_str(func: Definition(Function)) -> String {
  Module([], [], [], [], [func])
  |> glance_printer.print
  |> gleam_format
}

pub fn type_name(type_: Definition(CustomType)) -> String {
  let Definition(_, CustomType(name:, ..)) = type_
  name
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

pub fn is(
  value: String,
) -> Decoder(Nil) {
  decode.string
  |> decode.then(fn(str) {
    case str == value {
      True -> decode.into(Nil)
      False -> decode.fail("failed to match for value: " <> value)
    }
  })
}

pub fn decoder_birl_parse() -> Decoder(Time) {
  decoder_birl_string_to_result(
    func_name: "parse",
    func: birl.parse,
  )
}

pub fn decoder_birl_from_naive() -> Decoder(Time) {
  decoder_birl_string_to_result(
    func_name: "from_naive",
    func: birl.from_naive,
  )
}

pub fn decoder_birl_from_http() -> Decoder(Time) {
  decoder_birl_string_to_result(
    func_name: "from_http",
    func: birl.from_http,
  )
}

pub fn decoder_birl_from_unix() -> Decoder(Time) {
  decoder_birl_int_to_time(birl.from_unix)
}

pub fn decoder_birl_from_unix_milli() -> Decoder(Time) {
  decoder_birl_int_to_time(birl.from_unix_milli)
}

pub fn decoder_birl_from_unix_micro() -> Decoder(Time) {
  decoder_birl_int_to_time(birl.from_unix_micro)
}

pub fn encode_birl_to_iso8601(time: Time) -> Json {
  time
  |> birl.to_iso8601
  |> json.string
}

pub fn encode_birl_to_naive(time: Time) -> Json {
  time
  |> birl.to_naive
  |> json.string
}

pub fn encode_birl_to_http(time: Time) -> Json {
  time
  |> birl.to_naive
  |> json.string
}

pub fn encode_birl_to_unix(time: Time) -> Json {
  time
  |> birl.to_unix
  |> json.int
}

pub fn encode_birl_to_unix_milli(time: Time) -> Json {
  time
  |> birl.to_unix_milli
  |> json.int
}

pub fn encode_birl_to_unix_micro(time: Time) -> Json {
  time
  |> birl.to_unix_micro
  |> json.int
}

fn decoder_birl_string_to_result(
  func func: fn(String) -> Result(Time, Nil),
  func_name func_name : String,
) -> Decoder(Time) {
  decode.string
  |> decode.then(fn(str) {
    case func(str) {
      Ok(time) -> decode.into(time)
      Error(_) -> decode.fail("Failed to `" <> func_name <> "`: " <> str)
    }
  })
}

fn decoder_birl_int_to_time(
  func: fn(Int) -> Time,
) -> Decoder(Time) {
  decode.int
  |> decode.then(fn(int) {
    int
    |> func
    |> decode.into
  })
}

// TODO more `internal` than `util`...
pub fn get_field_opts(
  all_field_opts: DerivFieldOpts,
  type_: CustomType,
  variant: Variant,
  field: String,
) -> List(DerivFieldOpt) {
  let key =
    DerivField(
      type_: type_.name,
      variant: variant.name,
      field:,
    )

  all_field_opts
  |> dict.get(key)
  |> result.unwrap([])
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
  let filepath = "src/" <> path <> ".gleam"
  use src <- result.try(simplifile.read(filepath) |> result.map_error(types.FileErr))
  use module <- result.try(glance.module(src) |> result.map_error(types.GlanceErr))

  Ok(module)
}

pub fn look_up_type(
  type_: glance.Type,
  module: glance.Module,
  module_reader: ModuleReader,
) -> Result(glance.CustomType, ModuleReaderErr) {
  look_up_type_from_imports(type_, module, module_reader)
  |> result.lazy_or(fn() { look_up_type_in_module(type_, module) })
}

fn look_up_type_in_module(
  type_: glance.Type,
  module: glance.Module,
) -> Result(glance.CustomType, ModuleReaderErr) {
  case type_ {
    glance.NamedType(name:, module: None, ..) ->
      module.custom_types
      |> list.find(fn(def) { def.definition.name == name })
      |> result.map(fn(def) { def.definition })
      |> result.replace_error(types.NotFoundErr(
        func: "look_up_type_in_module",
        data: dynamic.from(type_),
      ))

    _ ->
      Error(types.ModuleReaderErr(
        msg: "`look_up_type_in_module` doesn't understand non-`NamedType`s",
        data: dynamic.from(type_),
      ))
  }
}

fn look_up_type_from_imports(
  type_: glance.Type,
  module: glance.Module,
  module_reader: ModuleReader,
) -> Result(glance.CustomType, ModuleReaderErr) {
  case type_ {
    glance.NamedType(name:, module: Some(ref), ..) -> {
      let imports = module.imports |> list.map(fn(def) { def.definition })

      use mod <- try(
        module_name_for(ref, imports)
        |> result.replace_error(types.BadIdent(ident: name))
      )

      let ident = mod <> "." <> name

      use #(_, ct) <- try(
        fetch_custom_type(ident, module_reader)
        |> result.replace_error(types.NotFoundErr(
          func: "look_up_custom_type",
          data: dynamic.from(#(ident, type_)),
        ))
      )

      Ok(ct.definition)
    }

    glance.NamedType(name:, module: None, ..) ->
      module.imports
      |> list.find_map(fn(import_) {
        use _matched <- result.try(
          import_.definition.unqualified_types
          |> list.find(fn(t) {
            t.alias == Some(name) || t.name == name
          })
          |> result.replace_error(types.ModuleReaderErr(
            msg: "`look_up_type_from_imports` import miss",
            data: dynamic.from(#(type_, import_)),
          ))
        )

        let ident = import_.definition.module <> "." <> name

        use module <- result.try(module_reader(ident))

        module.custom_types
        |> list.find(fn(ct) { ct.definition.name == name })
        |> result.map(fn(ct) { ct.definition })
        |> result.replace_error(types.NotFoundErr(
          func: "look_up_type_from_imports",
          data: dynamic.from(#(type_, module)),
        ))
      })
      |> result.replace_error(types.NotFoundErr(
        func: "look_up_type_from_imports",
        data: dynamic.from(type_),
      ))

    _ ->
      Error(types.ModuleReaderErr(
        msg: "`look_up_type_from_imports` doesn't understand non-`NamedType`s",
        data: dynamic.from(type_),
      ))
  }
}

pub fn look_up_custom_type(
  type_: glance.Type,
  module: glance.Module,
  module_reader: ModuleReader,
) -> Result(glance.Definition(glance.CustomType), ModuleReaderErr) {
  // if qualified
  //   look up qualified module
  //   look in other module
  // else // not qualified
  //   look imported unqualifed || defined locally in module

  let imports = module.imports |> list.map(fn(def) { def.definition })

  // pub type UnqualifiedImport {
  //   UnqualifiedImport(name: String, alias: Option(String))
  // }

  case type_ {
    glance.NamedType(name:, module: Some(ref), ..) -> {
      use mod <- try(
        module_name_for(ref, imports)
        |> result.replace_error(types.BadIdent(ident: name))
      )

      let ident = mod <> "." <> name

      use #(_, ct) <- try(
        fetch_custom_type(ident, module_reader)
        |> result.replace_error(types.NotFoundErr(
          func: "look_up_custom_type",
          data: dynamic.from(#(ident, type_)),
        ))
      )

      Ok(ct)
    }

    glance.NamedType(name:, module: None, ..) ->
      todo

    _ ->
      Error(types.ModuleReaderErr(
        msg: "`look_up_custom_type` couldn't figure out non-`NamedType`",
        data: dynamic.from(type_),
      ))
  }
}

pub fn fetch_custom_type(
  ident: String,
  read_module: ModuleReader,
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
  |> result.replace_error(types.NotFoundErr(func: "find_custom_type", data: dynamic.from(ref)))
}

fn find_function(
  ref: String,
  module: glance.Module,
) -> Result(glance.Definition(glance.Function), ModuleReaderErr) {
  module.functions
  |> list.find(fn(func) { func.definition.name == ref })
  |> result.replace_error(types.NotFoundErr(func: "find_function", data: dynamic.from(ref)))
}

// pub foo(
// ) -> Import

pub fn module_name_for(
  str: String,
  imports: List(glance.Import),
) -> Result(String, Nil) {
  imports
  |> list.find(fn(import_) { local_name_for(import_) == str })
  |> result.map(fn(import_) { import_.module })
}
// pub fn module_name_for(
//   str: String,
//   module: glance.Module,
// ) -> Result(String, Nil) {
//   module.imports
//   |> list.map(fn(def) { def.definition })
//   |> list.find(fn(import_) { local_name_for(import_) == str })
//   |> result.map(fn(import_) { import_.module })
// }

fn local_name_for(
  import_: glance.Import,
) -> String {
  case import_.alias {
    Some(glance.Named(alias)) ->
      alias

    _ ->
      final_segment_in_module_name(import_.module)
  }
}

fn final_segment_in_module_name(
  module: String,
) -> String {
  case module |> string.split("/") |> list.last {
    Ok(segment) -> segment
    Error(_) -> panic as "impossible match"
  }
}
