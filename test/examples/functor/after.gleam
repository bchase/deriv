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
