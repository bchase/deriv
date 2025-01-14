import decode/zero.{type Decoder} as decode
import deriv/util

pub type T {
  Var1(
    var1: String,
  )
  Var2(
    var2: Int,
  )
} //$ derive json(decode)

// TODO gen for multi variant
pub fn decoder_t() -> Decoder(T) {
  decode.failure(dummy_t(), "No `Decoder(T)` succeeded")
  |> decode.one_of([
    decoder_t_var1(),
    decoder_t_var2(),
  ])
}

// TODO gen with `decode_type_field` if multi var
fn decoder_t_var1() -> Decoder(T) {
  util.decode_type_field(variant: "Var1", json_field: "type_", fail_dummy: dummy_t(), pass: {
    use var1 <- decode.field("var1", decode.string)

    decode.success(Var1(var1:))
  })
}
fn decoder_t_var2() -> Decoder(T) {
  util.decode_type_field(variant: "Var2", json_field: "type_", fail_dummy: dummy_t(), pass: {
    use var2 <- decode.field("var2", decode.int)

    decode.success(Var2(var2:))
  })
}

// TODO gen for multi var decode
fn dummy_t() -> T { Var1(util.dummy_string()) }
