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
