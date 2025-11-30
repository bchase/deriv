pub type Field {
  //$ derive json decode encode
  //$ json variant key None
  Text(
    //$ json guard type "text"
    //$ json encode static type "text"
    text: String,
  )

  Number(
    //$ json guard type "number"
    //$ json encode static type "number"
    number: Float,
  )
}
