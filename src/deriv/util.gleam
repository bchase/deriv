import gleam/list
import gleam/string
import gleam/json.{type Json}
import gleam/regexp.{type Regexp}
import decode.{type Decoder}
import youid/uuid.{type Uuid}

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
