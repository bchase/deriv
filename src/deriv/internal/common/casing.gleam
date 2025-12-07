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

// // // FORM // // //

pub fn snake_case_to_label(
  str str: String,
) -> String {
  str
  |> string.split("_")
  |> list.map(string.capitalise)
  |> string.join(" ")
}
