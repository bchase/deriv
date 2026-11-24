import bchase/lens.{type Lens, Lens}

pub type Foo {
  Foo(
    bar: String,
    //$ derive.lens
    baz: Int,
  )
}

// DERIVED

pub const bar: Lens(Foo, String) = Lens(lens_foo_bar_get, lens_foo_bar_set)
fn lens_foo_bar_get(o: Foo) -> String { o.bar }
fn lens_foo_bar_set(o: Foo, i: String) -> Foo { Foo(..o, bar: i) }
