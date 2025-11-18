type Named {
  //$ derive into Local
  //$ derive into Titled
  Named(
    name: String,
    //$ into Titled.title
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

fn into_local_from_named(value: Named) -> Local {
  Local(name: value.name)
}

fn into_titled_from_named(value: Named) -> Titled {
  Titled(title: value.name)
}
