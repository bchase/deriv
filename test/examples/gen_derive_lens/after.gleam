import bchase/lens.{type Lens, Lens}

pub type Foo {
  Foo(
    bar: String,
    //$ derive.lens
    baz: Int,
  )
}

// DERIVED

fn bar_get(o: Foo) -> String {
  o.bar
}

fn bar_set(o: Foo, i: String) -> Foo {
  Foo(..o, bar: i)
}

pub const bar: Lens(Foo, String) = Lens(bar_get, bar_set)
