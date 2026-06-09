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

pub type Newtype(t) {
  //$ derive functor
  Newtype(
    wrapped: t,
  )
}

// DERIVED
