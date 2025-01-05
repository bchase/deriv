import gleam/option.{type Option}
import glance.{type CustomType}

pub type GenFunc = fn(CustomType, List(String), File) -> String

pub type Imports = List(#(#(String, String), String)) // ((deriv, opt), import)

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

