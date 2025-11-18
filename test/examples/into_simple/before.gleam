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
