import gleam/int

pub type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
  //$ derive into Valued
  //$ derive into ValuedDeriv
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

pub type Valued {
  Valued(
    name: String,
    value: String,
  )
}

pub type ValuedDeriv {
  ValuedDeriv(
    name: String,
    value: String,
    value_deriv: String,
  )
}

pub const to_string = int.to_string
