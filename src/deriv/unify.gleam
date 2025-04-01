import gleam/dict.{type Dict}
import gleam/io
import gleam/option.{Some, None}
import gleam/list
import gleam/result
import gleam/string
import gleam/regexp
import glance.{type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess, Span}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivFieldOpt}
import deriv/util

pub type GenFunc = fn(CustomType, Derivation, DerivFieldOpts, File) -> Gen

pub fn gen(
  type_: CustomType,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  let import_refs = build_import_refs(file.src)

  let overrides = build_field_overrides(field_opts, module_reader)

  let idents = deriv.opts

  let imports = []

  let funcs =
    unify(
      type_,
      idents,
      overrides,
      module_reader,
      import_refs,
    )
    |> list.map(unify_func)

  let src =
    funcs
    |> list.map(util.func_str)
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
}

fn build_field_overrides(
  field_opts: DerivFieldOpts,
  module_reader: ModuleReader,
) -> UnifyFieldOverrides {
  field_opts
  |> dict.map_values(fn(field, opts) {
    opts
    |> list.map(build_field_override(_, field, module_reader))
  })
  |> dict.to_list
}

type UnifyFunc {
  UnifyFunc(
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
    util.fetch_custom_type(ident, module_reader)
  })
  |> result.all
  |> fn(x) {
    case x {
      Error(err) -> {
        io.debug(idents)
        io.debug(err)
        panic as "`unify` issue with the above `idents`"
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
        io.debug(type_)
        panic as "`unify` derivation currently only supports invariant types"
      }
    }
  })
}

fn unify(
  return_type: CustomType,
  idents: List(String),
  overrides: UnifyFieldOverrides,
  module_reader: ModuleReader,
  import_refs: Dict(String, String),
) -> List(UnifyFunc) {
  case return_type.variants {
    [return_variant] ->
      idents
      |> get_param_types_and_variants(module_reader)
      |> list.map(fn(x) {
        let #(param_type_def, param_variant, param_type_module) = x
        let param_type = param_type_def.definition

        unify_variant(
          param_type_module,
          param_type,
          param_variant,
          return_type,
          return_variant,
          overrides,
        )
      })

    _, -> {
      io.debug(return_type)
      panic as "`unify` derivation currently only supports invariant types"
    }
  }
}

fn unify_variant(
  param_type_module: String,
  param_type: CustomType,
  param_variant: Variant,
  return_type: CustomType,
  return_variant: Variant,
  overrides: UnifyFieldOverrides,
) -> UnifyFunc {
  let fields = unify_func_fields(
    param_type_module,
    param_type,
    param_variant,
    return_type,
    return_variant,
    overrides,
  )

  let func_name = util.snake_case(param_type.name)
  let param_type = param_type.name
  let return_type = return_type.name
  let return_contr = return_variant.name

  UnifyFunc(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  )
}

// fn unify_func_fields(
//   param_variant: Variant,
//   return_variant: Variant,
//   overrides: UnifyFieldOverrides,
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

type UnifyFieldOverrides = List(#(DerivField, List(UnifyFieldOverride)))

type UnifyFieldOverride {
  UnifyFieldOverride(
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
) -> UnifyFieldOverride {
  case field_opt {
    DerivFieldOpt(strs: ["unify", "field", ident, override]) -> {
      let #(module_name, type_) =
        case util.fetch_custom_type(ident, module_reader) {
          Error(err) -> {
            io.debug(err)
            panic
          }

          Ok(#(m, td)) -> #(m, td.definition)
        }

      let field = deriv_field.field

      UnifyFieldOverride(
        ident:,
        field:,
        override:,
        module_name:,
        type_:,
      )
    }

    _ -> {
      io.debug(field_opt)
      panic as "Invalid `unify` `DerivFieldOpt` (printed above)"
    }
  }
}

// fn field_overrides(
//   ident: String,
//   field_name: String,
//   overrides: UnifyFieldOverrides,
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
//             UnifyFieldOverride(override:, ..) as uf -> {
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

fn unify_func_fields(
  param_type_module: String,
  param_type: CustomType,
  param_variant: Variant,
  return_type: CustomType,
  return_variant: Variant,
  overrides: UnifyFieldOverrides,
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
        io.debug(param_type)
        io.debug(param_variant)
        io.debug(param_field)
        panic as "`unify` param field doesn't exist"
      }

      Ok(param_field_type) if param_field_type == result_field_type ->
        param_field_type

      _ -> {
        io.println("PARAM TYPE")
        io.debug(param_type)
        io.debug(param_variant)
        io.debug(param_field)
        io.println("RETURN TYPE")
        io.debug(return_type)
        io.debug(return_variant)
        io.debug(return_field)
        panic as "`unify` param & return field types don't match"
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
        io.debug(variant)
        io.debug(field)
        panic as "Only the following field type is supported: `LabelledVariantField(item: NamedType(name:, ..), label:)`"
      }
    }
  })
}

fn unify_func(
  uf: UnifyFunc
) -> Definition(Function) {
  let UnifyFunc(
    func_name:,
    param_type:,
    return_type:,
    return_contr:,
    fields:,
  ) = uf

  io.debug(param_type)

  let dummy_span = Span(-1, -1)

  Definition([], Function(func_name, Public,
    [FunctionParameter(None, Named("value"), Some(NamedType(param_type, None, [])))],
    Some(NamedType(return_type, None, [])),
    [Expression(Call(Variable(return_contr), list.map(fields, fn(field) {
      LabelledField(field.return_field, FieldAccess(Variable("value"), field.param_field))
    })))], dummy_span)
  )
}

// TODO mv util
fn build_import_refs(
  src src: String,
) -> Dict(String, String) {
  let assert Ok(re) =
    "^import\\s+([/A-Za-z0-9]+)([.][{].+?[}])?(\\s+as\\s+(\\w+))?"
    //          1              2              3          4
    |> regexp.compile(regexp.Options(case_insensitive: False, multi_line: True))

  regexp.scan(re, src)
  |> list.filter_map(fn(match) {
    case match {
      regexp.Match(submatches: [Some(m), _, _, Some(qualified), ..], ..) ->
        Ok(#(m, qualified))

      regexp.Match(submatches: [Some(m), ..], ..) ->
        Ok(#(m, m))

      _ ->
        Error(Nil)
    }
  })
  |> dict.from_list
}
