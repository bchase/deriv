pub type Pet {
  Pet(
    id: Int,
    name: String,
    age: PetAge,
  )
}

pub type PetType {
  Dog
}

pub type PetAge {
  PetAge(
    type_: PetType,
    years: Int,
  )
}

pub fn to_human_years(
  _pet_age: PetAge,
) -> Int {
  todo
}

// age: Int,
//$ from examples/from/friend/pet.Pet.age using     to_human_age //     to_human_age(pet.age)
//$ from examples/from/friend/pet.Pet.age using pet.to_human_age // pet.to_human_age(pet.age)
//$ from examples/from/friend/pet.Pet     using     to_human_age //     to_human_age(pet)
//$ from examples/from/friend/pet.Pet     using pet.to_human_age // pet.to_human_age(pet)
