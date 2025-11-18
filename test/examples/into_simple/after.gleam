type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
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
