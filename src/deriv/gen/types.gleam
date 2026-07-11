import glance as g

pub type TypeDef {
  TypeDef(
    path: GleamPath,
    def: g.Definition(g.CustomType),
  )
}

pub type GleamPath {
  GleamPath(
    full: List(String),
    package: String,
    module: String,
  )
}
