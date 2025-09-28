pub type Unnested {
  //$ derive json decode encode
  Unnested(
    unnested: Int,
    //$ json named foo.bar.baz
  )
}
