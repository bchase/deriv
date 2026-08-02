import gleam/regexp
import gleam/list
import gleam/string

// // // CASE HELPERS // // //

pub fn pascal_case(str: String) -> String {
  str
  |> string.split("_")
  |> list.map(string.capitalise)
  |> string.join("")
}

pub fn snake_case(str: String) -> String {
  intersperse_capitalized(str:, with: "_")
}

pub fn hyphen_case(str: String) -> String {
  intersperse_capitalized(str:, with: "-")
}

fn intersperse_capitalized(
  str str: String,
  with separator: String,
) -> String {
  let assert Ok(capital_re) = regexp.from_string("[A-Z]")
  let assert Ok(initial_underscore_re) =
    regexp.from_string("^[" <> separator <> "](.)")

  str
  |> regexp.match_map(each: capital_re, in: _, with: fn(match) {
    match.content
    |> string.lowercase
    |> string.append(to: separator, suffix: _)
  })
  |> regexp.replace(each: initial_underscore_re, in: _, with: "\\1")
}
