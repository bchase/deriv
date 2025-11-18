import gleam/int

pub type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
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

pub fn into_local_from_using(value: Using) -> Local {
  Local(name: value.num |> int.to_string)
}
