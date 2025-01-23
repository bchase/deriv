import gleam/list
import gleam/string
import gleam/json.{type Json}
import gleam/result
import gleam/regexp.{type Regexp}
import decode.{type Decoder}
import youid/uuid.{type Uuid}
import gleam/io
import glance


import simplifile

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

pub fn replace_function(module: glance.Module, full_src: String, func_name func_name: String, func_src func_src: String) -> String {
  let assert Ok(span) =
    module.functions
    |> list.find_map(fn(f) {
      case f.definition.name == func_name {
        False -> Error(Nil)
        True -> Ok(f.definition.location)
      }
    })

  let eof = string.byte_size(full_src) - 1

  let before = string.slice(full_src, 0, span.start - 1)
  let after = string.slice(full_src, span.end + 1, eof)

  [
    before,
    func_src,
    after,
  ]
  |> string.join("\n")
}

pub fn update_funcs(init_src: String, funcs: List(#(String, String))) -> String {
  let assert Ok(module) = glance.module(init_src)

  list.fold(funcs, init_src, fn(src, func) {
    let #(func_name, func_src) = func

    case string.contains(src, "fn " <> func_name) {
      True -> replace_function(module, src, func_name:, func_src:)
      False -> src <> "\n\n" <> func_src
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
