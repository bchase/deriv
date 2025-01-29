import gleam/list
import gleam/string
import gleam/json.{type Json}
import gleam/result
import gleam/regexp.{type Regexp}
import decode.{type Decoder}
import youid/uuid.{type Uuid}
import glance.{type Definition, type Function, Module, Definition, Function}
import glance_printer
import shellout
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
    let result =
      case sc.curr {
        [] -> sc.acc
        _ -> list.append(sc.acc, [sc.curr])
      }

    result
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

pub fn replace_function(full_src: String, func_name func_name: String, func_src func_src: String) -> String {
  let re_str = "^(pub )?fn " <> func_name <> "[(].*"
  let assert Ok(re) = regexp.compile(re_str, regexp.Options(case_insensitive: False, multi_line: True))

  let assert [before, after] =
    regexp.split(re, full_src)
    |> list.filter(fn(str) { str != "pub " && str != "" })

  let new_before = string.trim_end(before)

  let new_after = drop_lines_up_to_and_including_lone_closing_brace(after)

  [ new_before, func_src, new_after ]
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

pub fn update_funcs(init_src: String, funcs: List(#(String, String))) -> String {
  list.fold(funcs, init_src, fn(src, func) {
    let #(func_name, func_src) = func

    case string.contains(src, "fn " <> func_name) {
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

pub fn gleam_format(src: String) -> String {
  let escaped_src =
    src
    |> string.replace(each: "\"", with: "\\\"")
    |> string.replace(each: "'", with: "\\'")

  let cmd = "echo \"" <> escaped_src <> "\" | gleam format --stdin"
  let assert Ok(formatted_enc) = shellout.command(run: "sh", with: ["-c", cmd], in: ".", opt: [])

  formatted_enc
}
