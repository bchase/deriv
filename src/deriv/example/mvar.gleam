import decode.{type Decoder}

pub type T {
  Var1(var1: String)
  Var2(var2: Int)
} //$ derive json(decode)


pub fn decoder_t() -> Decoder(T) {
  decode.one_of([
    decoder_t_var1(),
    decoder_t_var2(),
  ])
}

fn decoder_t_var1() -> Decoder(T) {
  decode.into({
    use var1 <- decode.parameter

    Var1(var1:)
  })
  |> decode.field("var1", decode.string)
}

fn decoder_t_var2() -> Decoder(T) {
  decode.into({
    use var2 <- decode.parameter

    Var2(var2:)
  })
  |> decode.field("var2", decode.int)
}