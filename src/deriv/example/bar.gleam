pub type Baz {
  Baz(
    id: Int,
    name: String,
    active: Bool,
  )
} //$ derive json(decode,encode)
