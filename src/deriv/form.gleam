import gleam/dynamic
import gleam/dict
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{Module, type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess, Span, CustomType}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivFieldOpt}
import deriv/util
import form

pub type GenFunc = fn(CustomType, Derivation, DerivFieldOpts, File) -> Gen

fn build_fields(
  type_: CustomType,
  variant: Variant,
  file: File,
  module_reader: ModuleReader,
) -> List(Field) {
  let assert Ok(module) = glance.module(file.src)
  let module_name = file.module

  gen_form_fields(
    variant,
    [],
    prefix: type_.name,
    module_name:,
    module:,
    module_reader:,
    wrap: fn(x) { x },
  )
}

fn gen_form_field_type(
  fields: List(Field),
  type_: CustomType,
  variant: Variant,
) -> String {
  let fields =
    fields
    |> list.map(fn(f) {
      to_id(variant, f)
    })

  [
    ["pub type " <> type_.name <> "Field {"],
    fields |> list.map(indent(_, 1)),
    ["}"],
  ]
  |> list.flatten
  |> string.join("\n")
}

fn indent(
  str: String,
  count: Int,
) -> String {
  "  "
  |> list.repeat(count)
  |> string.join("")
  |> string.append(str)
}

fn is_enum_type(
  type_: CustomType,
) -> Bool {
  type_.variants
  |> list.all(fn(var) {
    list.is_empty(var.fields)
  })
}

fn gleam_type_for(
  type_: glance.Type,
) -> Result(form.GleamType, Nil) {
  case type_ {
    NamedType(name:, parameters: [], ..) ->
      case name {
        "Int" ->
          Ok(form.Int)

        "Float" ->
          Ok(form.Float)

        "Bool" ->
          Ok(form.Bool)

        "String" ->
          Ok(form.String)

        "Uuid" ->
          Ok(form.Uuid)

        _ ->
          Error(Nil)
      }

    NamedType(name: "Option", parameters: [param], ..) -> {
      use t <- result.try(gleam_type_for(param))

      Ok(form.Option(t))
    }

    NamedType(name: "List", parameters: [param], ..) -> {
      use t <- result.try(gleam_type_for(param))

      Ok(form.List(t))
    }

    _ ->
      Error(Nil)
  }
}

type Field {
  Field(
    segments: List(String),
    gleam_type: form.GleamType,
  )
}

fn to_id(
  variant: Variant,
  field: Field,
) -> String {
  field.segments
  |> list.map(util.pascal_case)
  |> string.join("")
  |> fn(str) {
    variant.name <> str
  }
}

fn to_name(
  field: Field,
) -> String {
  field.segments
  |> list.map(fn(segment) {
    "[" <> segment <> "]"
  })
  |> string.join("")
}

fn to_label(
  field: Field,
) -> String {
  field.segments
  |> list.map(string.capitalise)
  |> string.join(" ")
}

fn to_gleam_type_string(
  type_: form.GleamType,
) -> String {
  case type_ {
    form.String -> "String"
    form.Int -> "Int"
    form.Float -> "Float"
    form.Bool -> "Bool"
    form.Uuid -> "Uuid"
    form.Enum(ident:) -> "Enum(ident: \"" <> ident <> "\")"
    form.Option(t) -> "Option(" <> to_gleam_type_string(t) <> ")"
    form.List(t) -> "List(" <> to_gleam_type_string(t) <> ")"
  }
}

fn is_required(
  field: Field,
) -> Bool {
  case field.gleam_type {
    form.Bool ->
      False

    form.Option(_) ->
      False

    form.List(_) ->
      False

    _ ->
      True
  }
}

fn gen_form_fields(
  variant: Variant,
  fields: List(String),
  prefix prefix: String,
  module_name module_name: String,
  module module: glance.Module,
  module_reader module_reader: ModuleReader,
  wrap wrap: fn(form.GleamType) -> form.GleamType,
) -> List(Field) {
  let recurse_on = fn(field_type, name, field, wrap) {
    case util.look_up_type(field_type, module, module_reader) {
      Ok(t) ->
        case is_enum_type(t), t {
          True, _ -> {
            let gleam_type = form.Enum(ident: "?." <> t.name)

            Field(
              gleam_type: gleam_type |> wrap,
              segments: fields |> list.append([name])
            )
            |> list.wrap
          }

          False, CustomType(variants: [variant], ..) -> {
            let prefix = prefix <> { name |> util.pascal_case }

            let fields = fields |> list.append([name])

            gen_form_fields(variant, fields, prefix:, module_name:, module:, module_reader:, wrap:)
          }

          _, _ ->
            panic as "`derive form` (`gen_form_fields`, depth 2) only supports invariant types"
        }

      Error(_) -> {
        io.debug(field)
        // !!! NOTE: if here for some weird nesting, see `TODO recurse` above !!!
        panic as { "`derive form` couldn't find field type" }
        // !!! NOTE: if here for some weird nesting, see `TODO recurse` above !!!
      }
    }
  }

  variant.fields
  |> list.flat_map(fn(field) {
    case field {
      LabelledVariantField(label: name, item: field_type) -> {
        case field_type, gleam_type_for(field_type) {
          _, Ok(gleam_type) ->
            Field(
              gleam_type: gleam_type |> wrap,
              segments: fields |> list.append([name]),
            )
            |> list.wrap

          NamedType(name: "Option", parameters: [param_type], ..), _->
            recurse_on(param_type, name, field, fn(t) { wrap(form.Option(t)) })

          NamedType(name: "List", parameters: [param_type], ..), _->
            recurse_on(param_type, name, field, fn(t) { wrap(form.List(t)) })

          _not_option_or_list, _not_simple_gleam_type -> {
            recurse_on(field_type, name, field, wrap)
          }
        }
      }

      _ -> {
        io.debug(variant)
        io.debug(field)
        panic as "`derive form` only supports labelled fields"
      }
    }
  })
}

pub fn gen(
  type_: CustomType,
  deriv: Derivation,
  _field_opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  let variant =
    case type_.variants {
      [variant] ->
        variant

      _ -> {
        io.debug(type_)
        panic as "`derive form` only supports invariant types"
      }
    }

  // let overrides = build_field_overrides(field_opts, module_reader)

  let imports = []

  let form =
    case deriv.opts {
      [name] -> name
      _ -> {
        io.debug(deriv)
        panic as { "`deriv/form.gen` cannot figure out form name (invalid opts)" }
      }
    }

  let fields = build_fields(type_, variant, file, module_reader)
  let gen = gen_form_field_type(fields, type_, variant)

  let assert Ok(Module(custom_types: [field_type], ..)) = glance.module(gen)

  let funcs =
    gen_lookups(fields, type_, variant)

  let types =
    [field_type]

  let types_src =
    types
    |> list.map(util.type_str)
    |> string.join("\n\n")

  let funcs_src =
    funcs
    |> list.map(util.func_str)
    |> string.join("\n\n")

  let src =
    [
      types_src,
      funcs_src,
    ]
    |> string.join("\n\n")
    |> util.gleam_format

  Gen(file:, deriv:, imports:, types:, funcs:, src:, meta: dict.new())
}

fn field_to_id_clauses(
  fields: List(Field),
  variant: Variant,
  indent count: Int,
) -> String {
  fields
  |> list.map(fn(field) {
    let id = to_id(variant, field)

    id <> " -> " <> "\"" <> id <> "\""
  })
  |> list.map(indent(_, count))
  |> string.join("\n")
  |> string.trim
}

fn field_to_name_clauses(
  fields: List(Field),
  variant: Variant,
  indent count: Int,
) -> String {
  fields
  |> list.map(fn(field) {
    let id = to_id(variant, field)
    let name = to_name(field)

    id <> " -> form <> \"" <> name <> "\""
  })
  |> list.map(indent(_, count))
  |> string.join("\n")
  |> string.trim
}

fn field_to_label_clauses(
  fields: List(Field),
  variant: Variant,
  indent count: Int,
) -> String {
  fields
  |> list.map(fn(field) {
    let id = to_id(variant, field)
    let label = to_label(field)

    id <> " -> " <> "\"" <> label <> "\""
  })
  |> list.map(indent(_, count))
  |> string.join("\n")
  |> string.trim
}

fn field_to_type_clauses(
  fields: List(Field),
  variant: Variant,
  indent count: Int,
) -> String {
  fields
  |> list.map(fn(field) {
    let id = to_id(variant, field)
    let type_ = to_gleam_type_string(field.gleam_type)

    id <> " -> " <> type_
  })
  |> list.map(indent(_, count))
  |> string.join("\n")
  |> string.trim
}

fn field_is_required_clauses(
  fields: List(Field),
  variant: Variant,
  indent count: Int,
) -> String {
  fields
  |> list.map(fn(field) {
    let id = to_id(variant, field)
    let required =
      case is_required(field) {
        True -> "True"
        False -> "False"
      }

    id <> " -> " <> required
  })
  |> list.map(indent(_, count))
  |> string.join("\n")
  |> string.trim
}

fn name_to_field_tuples(
  fields: List(Field),
  variant: Variant,
  indent count: Int,
) -> String {
  fields
  |> list.map(fn(field) {
    let id = to_id(variant, field)
    let name = to_name(field)

    "#(form <> \"" <> name <> "\", " <> id <> "),"
  })
  |> list.map(indent(_, count))
  |> string.join("\n")
  |> string.trim
}

fn gen_lookups(
  fields: List(Field),
  type_: CustomType,
  variant: Variant,
) -> List(Definition(Function)) {
  let field_type_name = type_.name <> "Field"
  let func_name =
    field_type_name
    |> util.snake_case
    |> string.append("_lookups")

  "
pub fn FUNC_NAME(form: String) -> Lookups(FIELD_TYPE_NAME) {
  let field_to_id =
    fn(field) {
      case field {
        FIELD_TO_ID
      }
      |> fn(str) { \"field-\" <> str }
    }

  let field_to_name =
    fn(field) {
      case field {
        FIELD_TO_NAME
      }
    }

  let field_to_label =
    fn(field) {
      case field {
        FIELD_TO_LABEL
      }
    }

  let field_to_type =
    fn(field) {
      case field {
        FIELD_TO_TYPE
      }
    }

  let field_is_required =
    fn(field) {
      case field {
        FIELD_IS_REQUIRED
      }
    }

  let dict =
    [
      NAME_TO_FIELD
    ]
    |> dict.from_list

  let name_to_field = fn(name) { dict.get(dict, name) }

  form.Lookups(
    field_to_id:,
    field_to_name:,
    field_to_label:,
    field_to_type:,
    field_is_required:,
    name_to_field:,
  )
}
  "
  |> string.trim
  |> string.replace("FIELD_TYPE_NAME", field_type_name)
  |> string.replace("FUNC_NAME", func_name)
  |> string.replace("FIELD_TO_ID", field_to_id_clauses(fields, variant, indent: 4))
  |> string.replace("FIELD_TO_NAME", field_to_name_clauses(fields, variant, indent: 4))
  |> string.replace("FIELD_TO_LABEL", field_to_label_clauses(fields, variant, indent: 4))
  |> string.replace("FIELD_TO_TYPE", field_to_type_clauses(fields, variant, indent: 4))
  |> string.replace("FIELD_IS_REQUIRED", field_is_required_clauses(fields, variant, indent: 4))
  |> string.replace("NAME_TO_FIELD", name_to_field_tuples(fields, variant, indent: 3))
  |> fn(src) {
    case glance.module(src) {
      Error(_) -> {
        io.println(src)
        panic as { "`gen_lookups` generated invalid Gleam" }
      }

      Ok(module) ->
        module.functions
    }
  }
}

// fn base_type(
//   type_: glance.Type
// ) -> glance.Type {
//   case type_ {
//     NamedType(parameters: [], ..) ->
//       type_

//     NamedType(parameters: [param_type], ..) ->
//       base_type(param_type)

//     NamedType(name: _multi_parameterized_type,  ..) -> {
//       io.debug(type_)
//       panic as { "`derive form` doesn't know what to do with multi parameter types" }
//     }

//     _ -> {
//       io.debug(type_)
//       panic as { "`derive form` only supports fields of `NamedType`" }
//     }
//   }
// }
