import gleam/int

pub type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
  //$ derive into NamedNamed
  //$ derive into NamedNamedNamed
  Named(
    name: String,
    //$ into Titled.title
  )
}

pub type Using {
  //$ derive into Local
  Using(
    num: Int,
    //$ into Local.name using int.to_string
  )
}

pub type Local {
  Local(
    name: String,
  )
}

pub type Titled {
  Titled(
    title: String,
  )
}

pub type Missing {
  Missing(
    name: String,
    missing: Int,
  )
}

pub type Extra {
  //$ derive into Named
  Extra(
    name: String,
    extra: Float,
  )
}

pub type NamedNamed {
  NamedNamed(
    name: String,
    named: String,
  )
}

pub type NamedNamedNamed {
  NamedNamedNamed(
    name: String,
    named: String,
    named_: String,
  )
}

pub type Use {
  //$ derive into Named
  Use(
    name: String,
  )
}

pub type FieldAccess {
  //$ derive into Named
  FieldAccess(
    depth0: Nested,
    //$ into Named.name using .depth1.depth2_string
  )
}

pub type FieldAccessConvA {
  //$ derive into Named
  FieldAccessConvA(
    depth0: Nested,
    //$ into Named.name using .depth1.depth2_int int.to_string
  )
}

pub type FieldAccessConvB {
  //$ derive into Named
  FieldAccessConvB(
    depth0: Nested,
    //$ into Named.name using int.to_string .depth1.depth2_int
  )
}

pub type Nested {
  Nested(
    depth1: NestedNested,
  )
}

pub type NestedNested {
  NestedNested(
    depth2_int: Int,
    depth2_string: String,
  )
}

pub const to_string = int.to_string
