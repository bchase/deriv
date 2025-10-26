pub type Empty {
  //$ derive json decode encode
  Empty(
    list: List(String),
    //$ json decode default empty
    //$ json encode skip
  )
}
