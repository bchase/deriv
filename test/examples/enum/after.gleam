import gleam/string

pub type T {
  //$ derive enum
  Foo
  //$ enum display Something else other than "Foo"
  Bar
  Baz
}

pub fn parse_enum_t(str: String) -> Result(T, Nil) {
  case str {
    "Foo" -> Ok(Foo)
    "Bar" -> Ok(Bar)
    "Baz" -> Ok(Baz)
    _ -> Error(Nil)
  }
}

pub fn enum_t_str(x: T) -> String {
  string.inspect(x)
}

pub fn display_enum_t(x: T) -> String {
  case x {
    Foo -> "Something else other than \"Foo\""
    Bar -> "Bar"
    Baz -> "Baz"
  }
}
