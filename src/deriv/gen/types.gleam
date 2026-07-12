import gleam/list
import gleam/dict.{type Dict}
import glance as g

pub type TypeDef {
  TypeDef(
    path: GleamPath,
    def: g.Definition(g.CustomType),
  )
}

// pub type ExprGenRef {
//   ExprGenRef(
//     module: String,
//     func: String,
//   )
// }

// pub fn expr_gen_ref(
//   path path: GleamPath,
//   func func: String,
// ) -> ExprGenRef {
//   ExprGenRef(
//     module: path.full |> string.join("/"),
//     func:,
//   )
// }

pub type GleamPath {
  GleamPath(
    full: List(String),
    package: String,
    module: String,
  )
}

pub type GleamFile {
  GleamFile(
    path: GleamPath,
    filepath: String,
    src: String,
    ast: AST,
  )
}

pub type AST {
  AST(
    imports: Imports,
    custom_types: Dict(String, g.Definition(g.CustomType)),
    type_aliases: Dict(String, g.Definition(g.TypeAlias)),
    constants: Dict(String, g.Definition(g.Constant)),
    functions: Dict(String, g.Definition(g.Function)),
  )
}

pub type Imports {
  Imports(
    named: Dict(String, g.Definition(g.Import)),
    discarded: List(g.Definition(g.Import)),
  )
}

pub fn all_imports(
  imports imports: Imports,
) -> List(g.Definition(g.Import)) {
  [
    imports.discarded,
    imports.named |> dict.values,
  ]
  |> list.flatten
}
