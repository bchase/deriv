import deriv/example/baz.{type Baz, type Other} //$ derive json(encode,decode)

pub type SuppressWarningsBaz = Baz
pub type SuppressWarningsOther = Other