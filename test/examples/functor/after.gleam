pub type Simple(t) {
  //$ derive functor
  Simple(
    foo: String,
    bar: t,
  )
}

// DERIVED

pub fn map_simple(simple simple: Simple(a), apply f: fn(a) -> b) -> Simple(b) {
  Simple(..simple, bar: f(simple.bar))
}
