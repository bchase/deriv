import deriv/internal/dummy/lookup_other.{type OtherImport}
import deriv/internal/dummy/lookup_other_other as oo
import glance
import bchase/id
import gleam/option

pub type Local {
  Local
}

pub type Foo {
}

pub type LocalAlias = Local

pub fn gen() {
  let foo = todo

  // some other comment

  { //$ gen target
    case foo {
      _ -> foo
    }
  }
}
