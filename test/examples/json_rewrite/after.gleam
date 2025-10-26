import gleam/dynamic/decode.{type Decoder}

pub type Foo {
  //$ derive json decode
  Foo(
    int: Int,
    string: String,
    bool: Bool,
    float: Float,
  )
}

pub fn decoder_foo() -> Decoder(Foo) {
  decode.one_of(decoder_foo_foo(), [])
}

pub fn decoder_foo_foo() -> Decoder(Foo) {
  use int <- decode.field("int", decode.int)
  use string <- decode.field("string", decode.string)
  use bool <- decode.field("bool", decode.bool)
  use float <- decode.field("float", decode.float)
  decode.success(Foo(int:, string:, bool:, float:))
}
