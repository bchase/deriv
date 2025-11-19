import gleam/int
import gleam/option.{type Option}

pub type Named {
  //$ derive into Local
  //$ derive into Titled
  //$ derive into Missing
  //$ derive into NamedNamed
  //$ derive into NamedNamedNamed
  Named(
    name: String,
    //$ into Titled.title
  )
}

pub type Using {
  //$ derive into Local
  Using(
    num: Int,
    //$ into Local.name using int.to_string
  )
}

pub type Local {
  Local(
    name: String,
  )
}

pub type Titled {
  Titled(
    title: String,
  )
}

pub type Missing {
  Missing(
    name: String,
    missing: Int,
  )
}

pub type Extra {
  //$ derive into Named
  Extra(
    name: String,
    extra: Float,
  )
}

pub type NamedNamed {
  NamedNamed(
    name: String,
    named: String,
  )
}

pub type NamedNamedNamed {
  NamedNamedNamed(
    name: String,
    named: String,
    named_: String,
  )
}

pub type Use {
  //$ derive into Named
  Use(
    name: String,
  )
}

pub type FieldAccess {
  //$ derive into Named
  FieldAccess(
    depth0: Nested,
    //$ into Named.name using .depth1.depth2_string
  )
}

pub type FieldAccessConvA {
  //$ derive into Named
  FieldAccessConvA(
    depth0: Nested,
    //$ into Named.name using .depth1.depth2_int int.to_string
  )
}

pub type FieldAccessConvB {
  //$ derive into Named
  FieldAccessConvB(
    depth0: Nested,
    //$ into Named.name using int.to_string .depth1.depth2_int
  )
}

pub type FieldAccessInner {
  //$ derive into OptionNamed
  FieldAccessInner(
    name: Option(Named),
    //$ into OptionNamed using inner .name
  )
}

pub type OptionNamed {
  OptionNamed(
    name: Option(String),
  )
}

pub type Nested {
  Nested(
    depth1: NestedNested,
  )
}

pub type NestedNested {
  NestedNested(
    depth2_int: Int,
    depth2_string: String,
  )
}

pub const to_string = int.to_string

pub fn into_local_from_named(named named: Named) -> Local {
  Local(name: named.name)
}

pub fn into_titled_from_named(named named: Named) -> Titled {
  Titled(title: named.name)
}

pub fn into_missing_from_named(
  named named: Named,
  missing missing: Int,
) -> Missing {
  Missing(name: named.name, missing:)
}

pub fn into_named_named_from_named(
  named_ named_: Named,
  named named: String,
) -> NamedNamed {
  NamedNamed(name: named_.name, named:)
}

pub fn into_named_named_named_from_named(
  named__ named__: Named,
  named named: String,
  named_ named_: String,
) -> NamedNamedNamed {
  NamedNamedNamed(name: named__.name, named:, named_:)
}

pub fn into_local_from_using(using using: Using) -> Local {
  Local(name: using.num |> int.to_string)
}

pub fn into_named_from_extra(extra extra: Extra) -> Named {
  Named(name: extra.name)
}

pub fn into_named_from_use(use_ use_: Use) -> Named {
  Named(name: use_.name)
}

pub fn into_named_from_field_access(
  field_access field_access: FieldAccess,
) -> Named {
  Named(name: field_access.depth0.depth1.depth2_string)
}

pub fn into_named_from_field_access_conv_a(
  field_access_conv_a field_access_conv_a: FieldAccessConvA,
) -> Named {
  Named(name: field_access_conv_a.depth0.depth1.depth2_int |> int.to_string)
}

pub fn into_named_from_field_access_conv_b(
  field_access_conv_b field_access_conv_b: FieldAccessConvB,
) -> Named {
  Named(name: field_access_conv_b.depth0.depth1.depth2_int |> int.to_string)
}

pub fn into_option_named_from_field_access_inner(
  field_access_inner field_access_inner: FieldAccessInner,
) -> OptionNamed {
  OptionNamed(name: field_access_inner.name |> option.map(fn(x) { x.name }))
}
