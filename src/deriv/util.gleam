import gleam/list
import gleam/string
import gleam/regexp.{type Regexp}
import decode/zero
import youid/uuid.{type Uuid}

pub fn decoder_uuid() {
  use bit_array <- zero.then(zero.bit_array)
  case uuid.from_bit_array(bit_array) {
    Ok(uuid) -> zero.success(uuid)
    Error(_) -> zero.failure(uuid.v7(), "uuid")
  }
}

pub fn encode_uuid(uuid: Uuid) -> String {
  uuid.to_string(uuid)
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
