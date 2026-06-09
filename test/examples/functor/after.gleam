import gleam/list

pub type Simple(t) {
  //$ derive functor
  Simple(
    foo: String,
    bar: t,
  )
}

pub type Implicit(x, y) {
  //$ derive functor
  Implicit(
    foo: y,
    bar: x,
  )
}

pub type Explicit(x, y, z) {
  //$ derive functor baz
  Explicit(
    foo: z,
    bar: x,
    baz: y,
  )
}

pub type MultiVar(x, y, z) {
  //$ derive functor
  MultiVar1(
    foo: z,
    bar: x,
  )
  MultiVar2(
    bar: x,
    baz: y,
  )
}

pub type Inner(t) {
  //$ derive functor inner
  Inner(
    other: String,
    inner: List(t),
  )
}

// DERIVED

pub fn map_simple(simple simple: Simple(a), apply f: fn(a) -> b) -> Simple(b) {
  Simple(..simple, bar: f(simple.bar))
}

pub fn map_implicit(
  implicit implicit: Implicit(a, t1),
  apply f: fn(a) -> b,
) -> Implicit(b, t1) {
  Implicit(..implicit, bar: f(implicit.bar))
}

pub fn map_explicit_baz(
  explicit explicit: Explicit(t1, a, t2),
  apply f: fn(a) -> b,
) -> Explicit(t1, b, t2) {
  Explicit(..explicit, baz: f(explicit.baz))
}

pub fn map_multi_var(
  multi_var multi_var: MultiVar(a, t1, t2),
  apply f: fn(a) -> b,
) -> MultiVar(b, t1, t2) {
  case multi_var {
    MultiVar1(..) -> MultiVar1(..multi_var, bar: f(multi_var.bar))
    MultiVar2(..) -> MultiVar2(..multi_var, bar: f(multi_var.bar))
  }
}

pub fn map_inner_inner(inner inner: Inner(a), apply f: fn(a) -> b) -> Inner(b) {
  Inner(..inner, inner: list.map(inner.inner, f))
}
