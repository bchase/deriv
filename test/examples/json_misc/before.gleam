pub type Task {
  //$ derive json decode encode
  Task(
    id: String,
    name: String,
    project: NamedResource(Project),
  )
}

pub type Project {
  Project(
    name: String,
  )
}

pub type NamedResource(resource) {
  //$ derive json decode encode
  NamedResource(
    id: Int,
    name: String,
  )
}
