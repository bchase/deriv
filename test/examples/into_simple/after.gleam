import gleam/int

pub const to_string = int.to_string

type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
  Named(
    name: String,
    //$ into Titled.title
  )
}

type Using {
  //$ derive into Named
  Using(
    num: Int,
    //$ into Local.name using int.to_string
  )
}

type Local {
  Local(
    name: String,
  )
}

type Titled {
  Titled(
    title: String,
  )
}

type Missing {
  Missing(
    name: String,
    missing: Int,
  )
}

fn into_local_from_named(value: Named) -> Local {
  Local(name: value.name)
}

fn into_titled_from_named(value: Named) -> Titled {
  Titled(title: value.name)
}

fn into_missing_from_named(value: Named, missing missing: Int) -> Missing {
  Missing(name: value.name, missing:)
}

fn into_named_from_using(value: Using) -> Named {
  Named(name: value.num |> int.to_string)
}
