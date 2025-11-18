import gleam/option.{type Option}
import gleam/dict.{type Dict}
import glance.{type Module, type Import, type Definition}
import simplifile

pub type Context {
  Context(
    deriv: Derivation,
    opts: DerivFieldOpts,
    file: File,
    module_reader: ModuleReader,
  )
}

pub type Type {
  Type(type_: glance.CustomType)
  TypeAlias(type_alias: glance.TypeAlias)
}

pub type GenFunc = fn(Type, Context) -> Gen

pub type ModuleReaderErr {
  FileErr(simplifile.FileError)
  GlanceErr(glance.Error)
  BadIdent(ident: String)
  NotFoundErr(ident: String)
}

pub type ModuleReader = fn(String) -> Result(Module, ModuleReaderErr)

pub type File {
  File(
    module: String,
    src: String,
    idx: Option(Int),
  )
}

pub type Output {
  Output(
    module: String,
    deriv: String,
  )
  OutputInline(
    module: String,
    filepath: String,
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
    ast: glance.Definition(glance.Function),
  )
}

pub type Gen {
  Gen(
    file: File,
    deriv: Derivation,
    imports: List(Import),
    types: List(Definition(glance.CustomType)),
    funcs: List(Definition(glance.Function)),
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

pub type DerivField {
  DerivField(
    type_: String,
    variant: String,
    field: String,
  )
}

pub type DerivFieldOpt {
  DerivFieldOpt(
    strs: List(String),
    raw: String,
  )
}

pub type DerivFieldOpts = Dict(DerivField, List(DerivFieldOpt))
