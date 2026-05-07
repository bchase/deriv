import deriv/util as deriv

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

pub const pet = deriv.GetSet(get_name, set_name)

pub fn get_name(person person: Person) -> Name {
  person.name
}

pub fn set_name(person person: Person, name name: Name) -> Person {
  Person(..person, name:)
}
