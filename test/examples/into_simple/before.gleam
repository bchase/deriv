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
