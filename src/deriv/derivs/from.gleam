import gleam/dict
import gleam/io
import gleam/option.{Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess, Span}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivFieldOpt} as deriv
import deriv/common

pub type GenFunc = fn(CustomType, Derivation, DerivFieldOpts, File) -> Gen

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/from` "

    deriv.Type(type_:) -> {
      let overrides = build_field_overrides(field_opts, module_reader)

      let idents = deriv.opts

      let imports = []

      let funcs =
        from(
          type_,
          idents,
          overrides,
          module_reader,
        )
        |> list.map(from_func)

      let src =
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file:, deriv:, imports:, funcs:, types: [], src:, meta: dict.new())
    }
  }
}

fn build_field_overrides(
  field_opts: DerivFieldOpts,
  module_reader: ModuleReader,
) -> FromFieldOverrides {
  field_opts
  |> dict.map_values(fn(field, opts) {
    opts
    |> list.map(build_field_override(_, field, module_reader))
  })
  |> dict.to_list
}

type FromFunc {
  FromFunc(
    func_name: String,
    param_type: String,
    return_type: String,
    return_contr: String,
    fields: List(Field),
  )
}
// func_name: String, // "authe_type_a"
// param_type: String, // "AutheTypeA"
// return_type: String, // "AutheTokens"
// return_contr: String, // "Authe"
// fields: List(#(String, String)),
// // [
// //   #("id", "authe_id"),
// //   #("encrypted_access_token", "encrypted_access_token"),
// //   #("encrypted_refresh_token", "encrypted_refresh_token"),
// // ]

fn get_param_types_and_variants(
  idents: List(String),
  module_reader: ModuleReader,
) -> List(#(Definition(CustomType), Variant, String)) {
  idents
  |> list.map(fn(ident) {
    common.fetch_custom_type(ident, module_reader)
  })
  |> result.all
  |> fn(x) {
    case x {
      Error(err) -> {
        common.debug(idents)
        common.debug(err)
        panic as "`from` issue with the above `idents`"
      }

      Ok(list) ->
        list
    }
  }
  |> list.map(fn(x) {
    let #(module_name, type_) = x

    case type_.definition.variants {
      [variant] ->
        #(type_, variant, module_name)

      _, -> {
        common.debug(type_)
        panic as "`from` derivation currently only supports invariant types"
      }
    }
  })
}

fn from(
  return_type: CustomType,
  idents: List(String),
  overrides: FromFieldOverrides,
  module_reader: ModuleReader,
) -> List(FromFunc) {
  case return_type.variants {
    [return_variant] ->
      idents
      |> get_param_types_and_variants(module_reader)
      |> list.map(fn(x) {
        let #(param_type_def, param_variant, param_type_module) = x
        let param_type = param_type_def.definition

        from_variant(
          param_type_module,
          param_type,
          param_variant,
          return_type,
          return_variant,
          overrides,
        )
      })

    _, -> {
      common.debug(return_type)
      panic as "`from` derivation currently only supports invariant types"
    }
  }
}

fn from_variant(
  param_type_module: String,
  param_type: CustomType,
  param_variant: Variant,
  return_type: CustomType,
  return_variant: Variant,
  overrides: FromFieldOverrides,
) -> FromFunc {
  let fields = from_func_fields(
    param_type_module,
    param_type,
    param_variant,
    return_type,
    return_variant,
    overrides,
  )

  let func_name = common.snake_case(param_type.name)
  let param_type = param_type.name
  let return_type = return_type.name
  let return_contr = return_variant.name

  FromFunc(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  )
}

// fn from_func_fields(
//   param_variant: Variant,
//   return_variant: Variant,
//   overrides: FromFieldOverrides,
// ) -> List(Field) {
//   return_variant.fields
//   |> list.map(fn(f1) {
//     // #(f1.item, list.find(param_variant.field, fn(f2) { f1.name == f2.name}))
//   })
// }

type Field {
  Field(
    // type_: String,
    // param_field_override: Option(String),
    param_field: String,
    return_field: String,
  )
}

type FromFieldOverrides = List(#(DerivField, List(FromFieldOverride)))

type FromFieldOverride {
  FromFieldOverride(
    ident: String,
    field: String,
    override: String,
    module_name: String,
    type_: CustomType,
  )
}

fn build_field_override(
  field_opt: DerivFieldOpt,
  deriv_field: DerivField,
  module_reader: ModuleReader,
) -> FromFieldOverride {
  case field_opt {
    DerivFieldOpt(strs: ["from", "field", ident, override]) -> {
      let #(module_name, type_) =
        case common.fetch_custom_type(ident, module_reader) {
          Error(err) -> {
            common.debug(err)
            panic
          }

          Ok(#(m, td)) -> #(m, td.definition)
        }

      let field = deriv_field.field

      FromFieldOverride(
        ident:,
        field:,
        override:,
        module_name:,
        type_:,
      )
    }

    _ -> {
      common.debug(field_opt)
      panic as "Invalid `from` `DerivFieldOpt` (printed above)"
    }
  }
}

// fn field_overrides(
//   ident: String,
//   field_name: String,
//   overrides: FromFieldOverrides,
// ) -> Option(String) {
//   overrides
//   |> list.find_map(fn(x) {
//     let #(f, opts) = x
//     case f.field == field_name {
//       False ->
//         Error(Nil)

//       True -> {
//         list.find_map(opts, fn(opt) {
//           case opt {
//             FromFieldOverride(override:, ..) as uf -> {
//               case uf.ident == ident {
//                 False -> Error(Nil)
//                 True -> Ok(override)
//               }
//             }
//           }
//         })
//       }
//     }
//   })
//   |> option.from_result
// }

fn build_ident(
  module_name: String,
  type_: CustomType,
) -> String {
  module_name <> "." <> type_.name
}

fn from_func_fields(
  param_type_module: String,
  param_type: CustomType,
  param_variant: Variant,
  return_type: CustomType,
  return_variant: Variant,
  overrides: FromFieldOverrides,
) -> List(Field) {
  let param_fields = fields(param_variant)
  let return_fields = fields(return_variant)

  return_fields
  |> list.map(fn(r_field) {
    let #(return_field, result_field_type) = r_field

    let overrides =
      overrides
      |> list.filter_map(fn(x) {
        let #(df, os) = x

        case df.type_ == return_type.name && df.field == return_field {
          False -> Error(Nil)
          True -> Ok(#(df.field, os))
        }
      })

    let override =
      overrides
      |> list.find_map(fn(x) {
        let #(param_field, os) = x

        list.find_map(os, fn(o) {
          let ident = build_ident(param_type_module, param_type)
          case o.ident == ident && o.field == return_field {
            False -> Error(Nil)
            True -> Ok(#(param_field, o))
          }
        })
      })

    let #(param_field, _p_type) =
      case override {
        Error(_) -> #(return_field, Nil)
        Ok(#(_param_field, o)) -> #(o.override, Nil)
      }

    let param_field_type =
      list.find_map(param_fields, fn(x) {
        let #(name, type_) = x
        case param_field == name {
          False -> Error(Nil)
          True -> Ok(type_)
        }
      })

    case param_field_type {
      Error(_) -> {
        common.debug(param_type)
        common.debug(param_variant)
        common.debug(param_field)
        panic as "`from` param field doesn't exist"
      }

      Ok(param_field_type) if param_field_type == result_field_type ->
        param_field_type

      _ -> {
        io.println("PARAM TYPE")
        common.debug(param_type)
        common.debug(param_variant)
        common.debug(param_field)
        io.println("RETURN TYPE")
        common.debug(return_type)
        common.debug(return_variant)
        common.debug(return_field)
        panic as "`from` param & return field types don't match"
      }
    }

    Field(
      // param_field_override:,
      param_field:,
      return_field:,
    )
  })
}

fn fields(variant: Variant) -> List(#(String, String)) {
  variant.fields
  |> list.map(fn(field) {
    case field {
      LabelledVariantField(item: NamedType(name:, ..), label:) ->
        #(label, name)

      _ -> {
        common.debug(variant)
        common.debug(field)
        panic as "Only the following field type is supported: `LabelledVariantField(item: NamedType(name:, ..), label:)`"
      }
    }
  })
}

fn from_func(
  uf: FromFunc
) -> Definition(Function) {
  let FromFunc(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  ) = uf

  Definition([], Function(common.dummy_location(), func_name, Public,
    [FunctionParameter(None, Named("value"), Some(NamedType(common.dummy_location(), param_type, None, [])))],
    Some(NamedType(common.dummy_location(), return_type, None, [])),
    [Expression(Call(common.dummy_location(), Variable(common.dummy_location(), return_contr), list.map(fields, fn(field) {
      LabelledField(field.return_field, FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "value"), field.param_field))
    })))])
  )
}
