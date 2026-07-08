import gleam/option.{type Option, None}
import gleam/list
import gleam/string
import glance as g

// HELPERS

pub const x = g.Span(-1, -1)
pub const z = g.Span(-1, -1)

pub fn string(
  str str: String,
) -> g.Expression {
  g.String(x, str)
}

pub fn list(
  xs: List(g.Expression)
) -> g.Expression {
  g.List(x, xs, None)
}

pub fn term(
  str: String,
) -> g.Expression {
  g.Variable(x, str)
}

pub fn call(
  function f: g.Expression,
  arguments args: List(g.Expression),
) -> g.Expression {
  f |> call_(args |> list.map(g.UnlabelledField))
}

pub fn call_(
  function f: g.Expression,
  arguments args: List(g.Field(g.Expression)),
) -> g.Expression {
  g.Call(x, f, args)
}

pub fn dot(
  a: String,
  b: String,
) -> g.Expression {
  a |> term |> dot_(b)
}

pub fn dot_(
  a: g.Expression,
  b: String,
) -> g.Expression {
  g.FieldAccess(x, a, b)
}

pub fn tuple(
  elements: List(g.Expression),
) -> g.Expression {
  g.Tuple(x, elements)
}

pub fn fn_(
  params params: List(String),
  body body: List(g.Statement)
) -> g.Expression {
  let arguments =
    params
    |> list.map(fn(param) {
      g.FnParameter(name: g.Named(param), type_: None)
    })

  g.Fn(x,
    arguments:,
    return_annotation: None,
    body:,
  )
}

pub fn pipe(
  left left: g.Expression,
  right right: g.Expression,
) -> g.Expression {
  g.BinaryOperator(x, name: g.Pipe, left:, right:)
}

//

pub fn named_type(
  name name: String,
  params params: List(g.Type),
) -> g.Type {
  named_type_(module: None, name:, params:)
}

pub fn named_type_(
  module module: Option(String),
  name name: String,
  params parameters: List(g.Type),
) -> g.Type {
  g.NamedType(x,
    name:,
    module:,
    parameters:,
  )
}

//

pub fn identity_func(
  param_name param_name: String,
) -> g.Expression {
  g.Fn(x,
    arguments: [
      g.FnParameter(
        name: g.Named(param_name),
        type_: None,
      )
    ],
    body: [
      g.Expression(
        param_name |> term,
      ),
    ],
    return_annotation: None,
  )
}

pub fn splice_out_span(
  str str: String,
  span span: g.Span,
) -> #(String, String) {
  let g.Span(start:, end:) = span
  let len = end - start + 1

  let ignore_end = string.length(str) - start
  let ignore_start = start + len

  let before = str |> string.drop_end(ignore_end)
  let after = str |> string.drop_start(ignore_start)

  #(before, after)
}
