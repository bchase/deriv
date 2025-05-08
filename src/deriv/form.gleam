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

fn base_type_(
  type_: glance.Type
) -> glance.Type {
  case type_ {
    NamedType(parameters: [], ..) ->
      type_

    NamedType(parameters: [param_type], ..) ->
      base_type_(param_type)

    NamedType(name: _multi_parameterized_type,  ..) -> {
      io.debug(type_)
      panic as { "`derive form` doesn't know what to do with multi parameter types" }
    }

    _ -> {
      io.debug(type_)
      panic as { "`derive form` only supports fields of `NamedType`" }
    }
  }
}

// type OuterType {
//   List
//   Option
// }

// fn base_type__(
//   type_: glance.Type,
//   outer: List(OuterType),
// ) -> #(glance.Type, List(OuterType)) {
//   case type_ {
//     NamedType(parameters: [], ..) ->
//       #(type_, outer)

//     NamedType(name: "Option", parameters: [param_type], ..) ->
//       base_type__(param_type, outer |> list.append([Option]))

//     NamedType(name: "List", parameters: [param_type], ..) ->
//       base_type__(param_type, outer |> list.append([List]))

//     NamedType(name:, parameters: [_param_type], ..) -> {
//       io.debug(type_)
//       panic as { "`derive form` doesn't know what to do with wrapper type: " <> name }
//     }

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

fn gen_form_field_type(
  type_: CustomType,
  variant: Variant,
  file: File,
  module_reader: ModuleReader,
) -> String {
  let assert Ok(module) = glance.module(file.src)
  let module_name = file.module

  let fields =
    gen_form_fields(
      variant,
      [],
      prefix: type_.name,
      module_name:,
      module:,
      module_reader:,
      wrap: fn(x) { x },
    )

  // fields
  // |> list.map(fn(f) {
  //   to_id(variant, f)
  //   |> io.debug
  //   is_required(f)
  //   |> io.debug
  // })

  // fields
  // |> list.map(fn(f) {
  //   to_label(f)
  // })
  // |> io.debug

  // fields
  // |> list.map(fn(f) {
  //   to_name("foo", f)
  // })
  // |> io.debug

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
    // id: String, // e.g. `"SomeFormField"`
    segments: List(String), //
    // name: String, // e.g. `"[field]"`
    // label: String, // e.g. `"Field"`
    gleam_type: form.GleamType, // e.g. `form.String`
    // required: Bool,
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
  form: String,
  field: Field,
) -> String {
  field.segments
  |> list.map(fn(segment) {
    "[" <> segment <> "]"
  })
  |> string.join("")
  |> fn(str) {
    form <> str
  }
}

fn to_label(
  field: Field,
) -> String {
  field.segments
  |> list.map(string.capitalise)
  |> string.join(" ")
}

fn is_required(
  field: Field,
) -> Bool {
  case field.gleam_type {
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

//   pub type [SomeForm]Field {
//     [SomeForm][FieldName]
//   }

// fn gen_to_fields_func
//
//   pub fn to_fields(form: [SomeForm]) -> Fields([SomeForm]Field) {
//     [
//       #([SomeForm][StringFieldName], [form.string_field_name]),
//       #([SomeForm][OptionalStringFieldName], [form.optional_string_field_name|> option.unwrap("")]),
//       #([SomeForm][IntFieldName], [form.int_field_name |> int.to_string]),
//     ]
//     |> dict.from_list
//   }

// fn gen_decode_form_func
//
//   pub fn decode_person_form(
//     fields: Fields(PersonFormField),
//   ) -> Result(PersonForm, form.Err(PersonFormField)) {
//     use name <- result.try(scalar(PersonFormName, decoder_string(), fields))
//     use email <- result.try(maybe(scalar(PersonFormEmail, decoder_string(), fields)))
//     use interval <- result.try(scalar(PersonFormInterval, decoder_int(), fields))
//     use favorite <- result.try(bool(PersonFormFavorite, fields))
//     use revenue <- result.try(maybe({
//       use currency <- result.try(scalar(PersonFormRevenueCurrency, decoder_currency(), fields))
//       use amount <- result.try(scalar(PersonFormRevenueAmount, decoder_int(), fields))
//
//       Ok(RevenueForm(currency:, amount:))
//     }))
//
//     Ok(PersonForm(name:, email:, interval:, favorite:, revenue:))
//   }

// fn gen_form_field_lookups_func
//
//   pub fn person_form_field_lookups(form: String) -> Lookups(PersonFormField) {
//     let field_to_id =
//       fn(field) {
//         case field {
//           PersonFormName  -> "PersonFormName"
//           // ...
//         }
//         |> fn(str) { "field-" <> str }
//       }
//
//     let field_to_name =
//       fn(field) {
//         case field {
//           PersonFormName -> form <> "[name]"
//           // ...
//         }
//       }
//
//     let field_to_label =
//       fn(field) {
//         case field {
//           PersonFormName -> "Name"
//           // ...
//         }
//       }
//
//     let field_to_type =
//       fn(field) {
//         case field {
//           PersonFormName -> String
//           // ...
//         }
//       }
//
//     let field_is_required =
//       fn(field) {
//         case field {
//           PersonFormName -> True
//           // ...
//         }
//       }
//
//     let dict = {
//       [
//         #(form <> "[name]", PersonFormName),
//         // ...
//       ]
//       |> dict.from_list
//     }
//
//     let name_to_field = fn(name) { dict.get(dict, name) }
//
//     form.Lookups(
//       field_to_id:,
//       field_to_name:,
//       field_to_label:,
//       field_to_type:,
//       field_is_required:,
//       name_to_field:,
//     )
//   }

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

  let funcs =
    []
    // into_(
    //   type_,
    //   deriv.opts,
    //   overrides,
    //   module_reader,
    // )
    // |> list.map(into_func)

  let gen = gen_form_field_type(type_, variant, file, module_reader)
  let assert Ok(Module(custom_types: [field_type], ..)) = glance.module(gen)
  // io.debug(field_type)

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

  Gen(file:, deriv:, imports:, types:, funcs:, src:, meta: dict.new())
}
