pub type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
  Named(
    name: String,
    //$ into Titled.title
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
