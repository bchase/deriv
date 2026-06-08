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
