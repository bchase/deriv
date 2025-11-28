pub type T {
  //$ derive json decode encode
  //$ json foo
  X(foo: String)
  Y(bar: Int)
}

pub type TT {
  //$ derive json decode encode
  //$ json variant key Some("_foo")
  A(foo: String)
  B(bar: Int)
}

pub type TTT {
  //$ derive json decode encode
  //$ json variant key None
  M(foo: String)
  N(bar: Int)
}
