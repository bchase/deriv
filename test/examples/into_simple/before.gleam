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

pub const to_string = int.to_string
