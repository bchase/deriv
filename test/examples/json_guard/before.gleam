pub type Field {
  //$ derive json decode encode
  //$ json variant key None
  Text(
    //$ json guard type "text"
    text: String,
  )

  Number(
    //$ json guard type "number"
    number: Float,
  )
}
