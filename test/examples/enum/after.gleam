import gleam/string

pub type T {
  //$ derive enum
  Foo
  //$ enum display Something else other than "Foo"
  Bar
  Baz
}

pub type ABC {
  //$ derive enum
  A
  B
  C
  Other(str: String)
  //$ enum fail
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
  case x {
    Foo -> "Foo"
    Bar -> "Bar"
    Baz -> "Baz"
  }
}

pub fn display_enum_t(x: T) -> String {
  case x {
    Foo -> "Something else other than \"Foo\""
    Bar -> "Bar"
    Baz -> "Baz"
  }
}

pub fn parse_enum_a_b_c(str: String) -> ABC {
  case str {
    "A" -> A
    "B" -> B
    "C" -> C
    _ -> Other(str)
  }
}

pub fn enum_a_b_c_str(x: ABC) -> String {
  case x {
    A -> "A"
    B -> "B"
    C -> "C"
    Other(str) -> str
  }
}

pub fn display_enum_a_b_c(x: ABC) -> String {
  case x {
    A -> "A"
    B -> "B"
    C -> "C"
    Other(str) -> str
  }
}
