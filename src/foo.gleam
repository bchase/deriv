pub type Foo {
  Foo(
    id: Int,
    name: String,
    active: Bool,
  )
} //$ derive json(decode,encode)

pub type Bar {
  Bar(
    baz: Bool,
  )
} //$ derive json(decode)
