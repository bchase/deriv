import gleam/list
import gleam/result
import simplifile
import glance as g

pub type Foo {
  Foo(
    bar: String,
    baz: Int,
  )
}

pub fn foo(
  foo: Foo,
) -> Foo {
  Foo(..foo, bar: "")
}

pub fn main() {
  {
    use src <- result.try(simplifile.read("src/deriv/internal/debug.gleam") |> result.replace_error(Nil))
    use module <- result.try(g.module(src) |> result.replace_error(Nil))

    echo module.functions |> list.find(fn(f) { f.definition.name == "foo" })

    Ok(Nil)
  }
  |> result.unwrap(Nil)

  Nil
}

