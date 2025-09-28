pub type A {
  //$ derive json decode encode
  A(
    b: B,
  )
}

pub type B {
  //$ derive json decode encode
  B(
    x: String,
  )
}
