import gleam/option.{type Option}

pub type File {
  File(
    module: String,
    src: String,
    idx: Int,
  )
}

pub type Output {
  Output(
    module: String,
    deriv: String,
  )
}

pub type Gen {
  Gen(
    file: File,
    deriv: Derivation,
    src: String,
  )
}

pub type Derivation {
  Derivation(
    name: String,
    opts: List(String),
  )
}

pub type DerivFieldOpt {
  DerivFieldOpt(
    deriv: String,
    opt: Option(String),
    key: String,
    val: String,
  )
}

