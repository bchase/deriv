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
