pub type Person {
  //$ derive get_set
  Person(
    name: Name,
    //$ get_set
    age: Int,
  )
}

pub type Name {
  Name(
    first: String,
    last: String,
  )
}

// DERIVED
