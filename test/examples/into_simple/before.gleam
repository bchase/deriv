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
