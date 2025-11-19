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
    named_deriv: String,
  )
}

pub const to_string = int.to_string

pub fn into_local_from_named(named named: Named) -> Local {
  Local(name: named.name)
}

pub fn into_titled_from_named(named named: Named) -> Titled {
  Titled(title: named.name)
}

pub fn into_missing_from_named(
  named named: Named,
  missing missing: Int,
) -> Missing {
  Missing(name: named.name, missing:)
}

pub fn into_named_named_from_named(
  named_deriv named_deriv: Named,
  named named: String,
) -> NamedNamed {
  NamedNamed(name: named_deriv.name, named:)
}

pub fn into_named_named_named_from_named(
  named_deriv_deriv named_deriv_deriv: Named,
  named named: String,
  named_deriv named_deriv: String,
) -> NamedNamedNamed {
  NamedNamedNamed(name: named_deriv_deriv.name, named:, named_deriv:)
}

pub fn into_local_from_using(using using: Using) -> Local {
  Local(name: using.num |> int.to_string)
}

pub fn into_named_from_extra(extra extra: Extra) -> Named {
  Named(name: extra.name)
}
