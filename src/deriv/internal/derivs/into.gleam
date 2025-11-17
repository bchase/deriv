import gleam/pair
import gleam/bool
import gleam/dict
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess, Span}
import deriv/internal/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivFieldOpt} as deriv
import deriv/internal/common.{type ImportedType, InScope, Qualified, gtype, gtype_}

const x = common.x

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
      panic as "`deriv.TypeAlias` unimplemented for `deriv/into` "

    deriv.Type(type_:) -> {
      let overrides = build_field_overrides(field_opts, module_reader)

      let imports = []

      let funcs =
        into_(
          type_,
          deriv.opts,
          overrides,
          file.module,
          module_reader,
          file,
        )
        |> list.map(into_func)

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
) -> IntoFieldOverrides {
  field_opts
  |> dict.map_values(fn(field, opts) {
    opts
    |> list.map(build_field_override(_, field, module_reader))
  })
  |> dict.to_list
}

type IntoFunc {
  IntoFunc(
    func_name: String,
    param_type: String,
    param_alias: Option(String),
    return_type: ImportedType,
    return_constr: String,
    return_alias: Option(String),
    fields: List(Field),
    missing: List(Missing),
  )
}
// func_name: String, // "authe_type_a"
// param_type: String, // "AutheTypeA"
// return_type: String, // "AutheTokens"
// return_constr: String, // "Authe"
// fields: List(#(String, String)),
// // [
// //   #("id", "authe_id"),
// //   #("encrypted_access_token", "encrypted_access_token"),
// //   #("encrypted_refresh_token", "encrypted_refresh_token"),
// // ]

fn starts_with_uppercase(
  str str: String,
) {
  str
  |> string.first
  |> result.map(fn(ch) {
    string.uppercase(ch) == ch
  })
  |> result.unwrap(False)
}

fn get_return_types_and_variants(
  idents: List(String),
  module: String,
  module_reader: ModuleReader,
) -> List(#(Definition(CustomType), Variant, String)) {
  idents
  |> list.map(fn(ident) {
    use <- bool.guard(!{ ident |> starts_with_uppercase }, ident)
    module <> "." <> ident
  })
  |> list.map(fn(ident) {
    common.fetch_custom_type(ident, module_reader)
  })
  |> result.all
  |> fn(x) {
    case x {
      Error(err) -> {
        common.debug(idents)
        common.debug(err)
        panic as "`into` issue with the above `idents`"
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
        panic as "`into` derivation currently only supports invariant types"
      }
    }
  })
}

// fn into(
//   param_type: CustomType,
//   opts: List(String),
//   overrides: IntoFieldOverrides,
//   module_reader: ModuleReader,
// ) -> List(IntoFunc) {
//   // param_type param_type: CustomType,
//   // param_variant param_variant: Variant,
//   // return_type return_type: CustomType,
//   // return_variant return_variant: Variant,
//   // return_type_module return_type_module: String,
//   // overrides overrides: IntoFieldOverrides,
//   case param_type.variants {
//     [param_variant] -> {
//       let #(ident, _alias) =
//         case opts {
//           [ident] ->
//             #(ident, None)

//           [ident, "as", alias] ->
//             #(ident, Some(alias))

//           _ -> {
//             common.debug(opts)
//             panic as "invalid `into` opts"
//           }
//         }

//       [ident]
//       |> get_return_types_and_variants(module_reader)
//       |> list.map(fn(x) {
//         let #(return_type_def, return_variant, return_type_module) = x
//         let return_type = return_type_def.definition

//         into_variant(
//           param_type:,
//           param_variant:,
//           return_type:,
//           return_variant:,
//           return_type_module:,
//           overrides:,
//         )
//       })
//     }
//     _, -> {
//       common.debug(param_type)
//       panic as "`into` derivation currently only supports invariant types"
//     }
//   }
// }

// fn into_variant(
//   param_type param_type: CustomType,
//   param_variant param_variant: Variant,
//   return_type return_type: CustomType,
//   return_variant return_variant: Variant,
//   return_type_module return_type_module: String,
//   overrides overrides: IntoFieldOverrides,
// ) -> IntoFunc {
//   let fields = into_func_fields(
//     return_type_module:,
//     param_type:,
//     param_variant:,
//     return_type:,
//     return_variant:,
//     overrides:,
//   )

//   let func_name = common.snake_case(param_type.name)
//   let param_type = param_type.name
//   let return_type = return_type.name
//   let return_constr = return_variant.name

//   IntoFunc(
//     func_name:,
//     param_type:,
//     param_alias:,
//     return_type:,
//     return_constr:,
//     return_alias:,
//     fields:,
//   )
// }

// fn into_func_fields(
//   param_variant: Variant,
//   return_variant: Variant,
//   overrides: IntoFieldOverrides,
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

type Missing {
  Missing(
    field: String,
    type_: glance.Type,
  )
}

type IntoFieldOverrides = List(#(DerivField, List(IntoFieldOverride)))

type IntoFieldOverride {
  IntoFieldOverride(
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
) -> IntoFieldOverride {
  case field_opt {
    DerivFieldOpt(strs: ["into", "field", ident, field]) -> {
      let #(module_name, type_) =
        case common.fetch_custom_type(ident, module_reader) {
          Error(err) -> {
            common.debug(ident)
            common.debug(err)
            panic
          }

          Ok(#(m, td)) -> #(m, td.definition)
        }

      let override = deriv_field.field

      IntoFieldOverride(
        ident:,
        field:,
        override:,
        module_name:,
        type_:,
      )
    }

    _ -> {
      common.debug(field_opt)
      panic as "Invalid `into` `DerivFieldOpt` (printed above)"
    }
  }
}

// fn field_overrides(
//   ident: String,
//   field_name: String,
//   overrides: IntoFieldOverrides,
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
//             IntoFieldOverride(override:, ..) as uf -> {
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

// fn into_func_fields(
//   param_type param_type: CustomType,
//   param_variant param_variant: Variant,
//   return_type return_type: CustomType,
//   return_variant return_variant: Variant,
//   return_type_module return_type_module: String,
//   overrides overrides: IntoFieldOverrides,
// ) -> List(Field) {
//   let param_fields = fields(param_variant)
//   let return_fields = fields(return_variant)

//   param_fields
//   |> list.map(fn(r_field) {
//     let #(return_field, result_field_type) = r_field

//     common.debug(param_type.name)
//     common.debug(return_field)
//     let overrides =
//       overrides
//       |> list.filter_map(fn(x) {
//         let #(df, os) = x

//         common.debug(df.type_)
//         common.debug(df.field)

//         case df.type_ == param_type.name && df.field == return_field {
//           False -> Error(Nil)
//           True -> Ok(#(df.field, os))
//         }
//       })

//     let override =
//       overrides
//       |> list.find_map(fn(x) {
//         let #(param_field, os) = x

//         list.find_map(os, fn(o) {
//           let ident = build_ident(return_type_module, return_type)
//           case o.ident == ident && o.field == return_field {
//             False -> Error(Nil)
//             True -> Ok(#(param_field, o))
//           }
//         })
//       })

//     let #(param_field, _p_type) =
//       case override {
//         Error(_) -> #(return_field, Nil)
//         Ok(#(_param_field, o)) -> #(o.override, Nil)
//       }

//     let param_field_type =
//       list.find_map(return_fields, fn(x) {
//         let #(name, type_) = x
//         case param_field == name {
//           False -> Error(Nil)
//           True -> Ok(type_)
//         }
//       })

//     case param_field_type {
//       Error(_) -> {
//         common.debug(param_type)
//         common.debug(param_variant)
//         common.debug(param_field)
//         panic as "`into` param field doesn't exist"
//       }

//       Ok(param_field_type) if param_field_type == result_field_type ->
//         param_field_type

//       _ -> {
//         common.debug("PARAM TYPE")
//         common.debug(param_type)
//         common.debug(param_variant)
//         common.debug(param_field)
//         common.debug("RETURN TYPE")
//         common.debug(return_type)
//         common.debug(return_variant)
//         common.debug(return_field)
//         panic as "`into` param & return field types don't match"
//       }
//     }

//     Field(
//       // param_field_override:,
//       param_field:,
//       return_field:,
//     )
//   })
// }

fn fields(variant: Variant) -> List(#(String, String)) {
  variant
  |> fields_with_type
  |> list.map(fn(t) {
    let #(field_name, type_name, _type) = t

    #(field_name, type_name)
  })
}

fn fields_with_type(variant: Variant) -> List(#(String, String, glance.Type)) {
  variant.fields
  |> list.map(fn(field) {
    case field {
      LabelledVariantField(item: NamedType(name:, ..) as type_, label:) ->
        #(label, name, type_)

      _ -> {
        common.debug(variant)
        common.debug(field)
        panic as "Only the following field type is supported: `LabelledVariantField(item: NamedType(name:, ..), label:)`"
      }
    }
  })
}

fn into_func(
  uf: IntoFunc
) -> Definition(Function) {
  let IntoFunc(
    func_name:,
    param_type:,
    param_alias:,
    return_type:,
    return_constr:,
    return_alias:,
    fields:,
    missing:,
  ) = uf

  let return_constr =
    // case return_alias {
    //   None -> Variable(common.dummy_location(), return_constr)
    //   Some(module) -> FieldAccess(common.dummy_location(), Variable(common.dummy_location(), module), return_constr)
    // }
    case return_type {
      InScope(curr_module: True, ..) ->
        Variable(x, return_constr)

      InScope(module:, curr_module: False, ..) |
      Qualified(module:, ..) ->
        FieldAccess(x, Variable(x, module), return_constr)
    }

  let #(missing_params, missing_fields) =
    missing
    |> list.map(fn(missing) {
      let param =
        FunctionParameter(
          label: Some(missing.field),
          name: glance.Named(missing.field),
          type_: Some(missing.type_),
        )

      let field =
        glance.ShorthandField(label: missing.field)

      #(param, field)
    })
    |> list.unzip

  let fields =
    list.map(fields, fn(field) {
      LabelledField(field.return_field, FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "value"), field.param_field))
    })

  let fields =
    fields
    |> list.append(missing_fields)

  let return_type =
    case return_type {
      InScope(name:, ..) ->
        gtype(name, [])

      Qualified(module:, name:) ->
        gtype_(
          module: Some(module),
          name:,
          params:[]
        )
    }

  Definition([], Function(common.dummy_location(), func_name, Public,
    [FunctionParameter(None, Named("value"), Some(NamedType(common.dummy_location(), param_type, param_alias, []))), ..missing_params],
    Some(return_type),
    [Expression(Call(common.dummy_location(), return_constr, fields))])
  )
}


// // // // //

type Direction {
  LocalToRemote
  RemoteToLocal
}

type Mapping {
  Mapping(
    direction: Direction,
    local_type: CustomType,
    local_variant: Variant,
    remote_type: CustomType,
    remote_variant: Variant,
    remote_alias: Option(String),
    remote_type_module: String,
    overrides: IntoFieldOverrides,
    return_type: ImportedType,
  )
}

// type IntoFunc1 {
//   IntoFunc1(
//     func_name: String,
//     param_type: String,
//     return_type: String,
//     return_constr: String,
//     fields: List(Field),
//   )
// }

// type Field1 {
//   Field1(
//     // type_: String,
//     // param_field_override: Option(String),
//     remote_field: String,
//     local_field: String,
//   )
// }

fn into_variant_(
  m: Mapping,
  // prefix: String,
) -> IntoFunc {
  // let Mapping(
  //   direction:,
  //   local_type:,
  //   local_variant:,
  //   remote_type:,
  //   remote_variant:,
  //   remote_type_module:,
  //   overrides:,
  // ) = m

  // let param_type = param_type.name
  // let return_type = return_type.name
  // let return_constr = return_variant.name

  let into = common.snake_case(m.remote_type.name)
  let from = common.snake_case(m.local_type.name)

  let func_name = "into_" <> into <> "_from_" <> from

  let #(fields, missing) = build_fields(m)

  case m.direction {
    LocalToRemote -> {
      IntoFunc(
        func_name:,
        param_type: m.local_type.name,
        param_alias: None,
        return_type: m.return_type,
        return_constr: m.remote_variant.name,
        return_alias: m.remote_alias,
        fields:,
        missing:,
      )
    }

    RemoteToLocal ->
      todo
      // IntoFunc(
      //   func_name: prefix <> common.snake_case(m.local_type.name),
      //   param_type: m.remote_type.name,
      //   param_alias: m.remote_alias,
      //   return_type: m.local_type.name,
      //   return_constr: m.local_variant.name,
      //   return_alias: None,
      //   fields:,
      // )
  }
}

type IntoFieldOverride1 {
  IntoFieldOverride1(
    local_type: String,
    local_variant: String,
    local_field: String,
    remote_ident: String,
    remote_field: String,
    remote_override: String,
    remote_module_name: String,
    remote_type: CustomType,
  )
}

fn conv(
  xs: List(#(DerivField, List(IntoFieldOverride))),
) -> List(IntoFieldOverride1) {
  list.flat_map(xs, fn(x) {
    let #(df, os) = x

    list.map(os, fn(o) {
      IntoFieldOverride1(
        local_type: df.type_,
        local_variant: df.variant,
        local_field: df.field,
        remote_ident: o.ident,
        remote_field: o.field,
        remote_override: o.override,
        remote_module_name: o.module_name,
        remote_type: o.type_,
      )
    })
  })
}

fn get_return_field_override(
  field: String,
  m: Mapping,
) -> Result(String, Nil) {
  case m.direction {
    RemoteToLocal ->
      Ok(field)

    LocalToRemote -> {
      let os = conv(m.overrides)

      todo
    }
  }
}

type FieldRole {
  Param
  Return
}

// fn fields(variant: Variant) -> List(#(String NAME, String TYPE)) {
fn fields_(
  variant: Variant,
  role: FieldRole,
  m: Mapping,
) -> List(#(String, String)) {
  let os = conv(m.overrides)
  let fs = fields(variant)

  // RemoteToLocal (FROM) // overrides refer to remote field as return
  // LocalToRemote (INTO)  // overrides refer to remote field as param

  case m.direction, role {
    LocalToRemote, Return ->
      fs

    RemoteToLocal, Param ->
      fs

    LocalToRemote, Param ->
      todo // override (off remote key, `IntoFieldOverride`)

    RemoteToLocal, Return ->
      todo // override (off local key, `DerivField`)
  }
}

type F {
  F(field: Field)
  NoF(
    field: String,
    type_: glance.Type,
  )
}

fn build_fields(
  m: Mapping,
) -> #(List(Field), List(Missing)) {
  let Mapping(
    local_type:,
    local_variant:,
    remote_type:,
    remote_variant:,
    remote_type_module:,
    overrides:,
    ..
  ) = m

  case m.direction {
    LocalToRemote -> {
      let #(
        param_type,
        param_variant,
        param_module,
        return_type,
        return_variant,
        return_module,
      ) =
        #(
          local_type, // param_type
          local_variant, // param_variant
          None, // param_module
          remote_type, // return_type
          remote_variant, // return_variant
          Some(remote_type_module), // return_module
        )

      let param_fields = fields(param_variant)
      let return_fields = fields_with_type(return_variant)

      return_fields
      |> list.map(fn(r_field) {
        let #(return_field, result_field_type, return_type) = r_field

        overrides
        |> list.find_map(fn(x) {
          let #(df, os) = x

          os
          |> list.reverse // prefer override defined later // TODO panic dupe
          |> list.find_map(fn(o) {
            case o.field == return_field && o.type_ == remote_type && o.module_name == remote_type_module {
              True -> Ok(#(df.field, df.type_))
              False -> Error(Nil)
            }
          })
        })
        |> fn(r) {
          case r {
            Ok(#(param_field, _return_field)) -> F(field: Field(param_field:, return_field:))
            Error(Nil) -> {
              let field =
                param_fields
                |> list.find(fn(field) {
                  let #(name, _) = field

                  name == return_field
                })

              case field {
                Ok(#(param_field, _)) -> F(field: Field(param_field:, return_field:))
                Error(Nil) -> NoF(field: return_field, type_: return_type)
              }
            }
          }
        }
      })
    }
    |> list.partition(fn(f) {
      case f {
        F(..) -> True
        NoF(..) -> False
      }
    })
    |> pair.map_first(list.filter_map(_, fn(field) {
      case field {
        F(field:) -> Ok(field)
        NoF(..) -> Error(Nil)
      }
    }))
    |> pair.map_second(list.filter_map(_, fn(field) {
      case field {
        NoF(field:, type_:) -> Ok(Missing(field:, type_:))
        F(..) -> Error(Nil)
      }
    }))

    RemoteToLocal -> {
      todo as "rework to match `LocalToRemote`"
      // let param_type_module = remote_type_module

      // let #(
      //   param_type,
      //   param_variant,
      //   param_module,
      //   return_type,
      //   return_variant,
      //   return_module,
      // ) =
      //   #(
      //     remote_type, // param_type
      //     remote_variant, // param_variant
      //     Some(remote_type_module), // param_module
      //     local_type, // return_type
      //     local_variant, // return_variant
      //     None, // return_module
      //   )

      // let param_fields = fields(param_variant)
      // let return_fields = fields(return_variant)

      // return_fields
      // |> list.map(fn(r_field) {
      //   let #(return_field, result_field_type) = r_field

      //   let overrides =
      //     overrides
      //     |> list.filter_map(fn(x) {
      //       let #(df, os) = x

      //       case df.type_ == return_type.name && df.field == return_field {
      //         False -> Error(Nil)
      //         True -> Ok(#(df.field, os))
      //       }
      //     })

      //   let override =
      //     overrides
      //     |> list.find_map(fn(x) {
      //       let #(param_field, os) = x

      //       list.find_map(os, fn(o) {
      //         let ident = build_ident(param_type_module, param_type)
      //         case o.ident == ident && o.field == return_field {
      //           False -> Error(Nil)
      //           True -> Ok(#(param_field, o.override))
      //         }
      //       })
      //     })

      //   let #(param_field, _p_type) =
      //     case override {
      //       Error(_) ->  #(return_field, Nil)
      //       Ok(#(_param_field, override)) -> #(override, Nil)
      //     }

      //   let param_field_type =
      //     list.find_map(param_fields, fn(x) {
      //       let #(name, type_) = x
      //       case param_field == name {
      //         False -> Error(Nil)
      //         True -> Ok(type_)
      //       }
      //     })

      //   case param_field_type {
      //     Error(_) -> {
      //       common.debug(param_type)
      //       common.debug(param_variant)
      //       common.debug(param_field)
      //       panic as "`into` `RemoteToLocal` -- param field doesn't exist"
      //     }

      //     Ok(param_field_type) if param_field_type == result_field_type ->
      //       Nil

      //     _ -> {
      //       io.println("PARAM TYPE")
      //       common.debug(param_type)
      //       common.debug(param_variant)
      //       common.debug(param_field)
      //       io.println("RETURN TYPE")
      //       common.debug(return_type)
      //       common.debug(return_variant)
      //       common.debug(return_field)
      //       panic as "`into` `RemoteToLocal` -- param & return field types don't match"
      //     }
      //   }

      //   Field(
      //     param_field:,
      //     return_field:,
      //   )
      // })
    }
  }
}

fn into_(
  local_type: CustomType,
  opts: List(String),
  overrides: IntoFieldOverrides,
  module: String,
  module_reader: ModuleReader,
  file: File,
) -> List(IntoFunc) {
  case local_type.variants {
    [local_variant] -> {
      let #(ident, remote_alias) =
        case opts {
          [ident] ->
            #(ident, None)

          [ident, "as", alias] ->
            #(ident, Some(alias))

          _ -> {
            common.debug(opts)
            panic as "invalid `into` opts"
          }
        }

      [ident]
      |> get_return_types_and_variants(module, module_reader)
      |> list.map(fn(x) {
        let #(remote_type_def, remote_variant, remote_type_module) = x
        let remote_type = remote_type_def.definition

        let return_type =
          common.build_imported_type(
            module_name: remote_type_module,
            type_: remote_type,
            file:,
          )

        Mapping(
          direction: LocalToRemote,
          local_type:,
          local_variant:,
          remote_type:,
          remote_variant:,
          remote_alias:,
          remote_type_module:,
          overrides:,
          return_type:,
        )
        // |> into_variant_("into_")
        |> into_variant_
      })
    }
    _, -> {
      common.debug(local_type)
      panic as "`into` derivation currently only supports invariant types"
    }
  }
}
