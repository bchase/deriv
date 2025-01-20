import gleam/option.{type Option}
import gleam/dict.{type Dict}
import glance.{type CustomType}

pub type GenFunc = fn(CustomType, Derivation, Dict(String, List(DerivFieldOpt)), File) -> Gen

pub type Import {
  Import(
    module: String,
    types: List(String),
    constructors: List(String),
    alias: Option(String),
  )
}

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

pub type Write {
  Write(
    filepath: String,
    src: String,
    output: Output,
  )
}

pub type Function {
  Function(
    name: String,
    src: String,
  )
}

pub type Gen {
  Gen(
    file: File,
    deriv: Derivation,
    imports: List(Import),
    funcs: List(Function),
    src: String,
    meta: Dict(String, String),
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

