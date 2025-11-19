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

pub fn into_local_from_named(value: Named) -> Local {
  Local(name: value.name)
}

pub fn into_titled_from_named(value: Named) -> Titled {
  Titled(title: value.name)
}

pub fn into_missing_from_named(value: Named, missing missing: Int) -> Missing {
  Missing(name: value.name, missing:)
}

pub fn into_valued_from_named(value_deriv: Named, value value: String) -> Valued {
  Valued(name: value_deriv.name, value:)
}

pub fn into_valued_deriv_from_named(
  value_deriv_deriv: Named,
  value value: String,
  value_deriv value_deriv: String,
) -> ValuedDeriv {
  ValuedDeriv(name: value_deriv_deriv.name, value:, value_deriv:)
}

pub fn into_local_from_using(value: Using) -> Local {
  Local(name: value.num |> int.to_string)
}

pub fn into_named_from_extra(value: Extra) -> Named {
  Named(name: value.name)
}
