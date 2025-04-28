import gleam/dict
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
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
  let overrides = build_field_overrides(field_opts, module_reader)

  let imports = []

  let funcs =
    into_(
      type_,
      deriv.opts,
      overrides,
      module_reader,
    )
    |> list.map(into_func)

  let src =
    funcs
    |> list.map(util.func_str)
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
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
    return_type: String,
    return_constr: String,
    return_alias: Option(String),
    fields: List(Field),
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

fn get_return_types_and_variants(
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
        io.debug(type_)
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
//             io.debug(opts)
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
//       io.debug(param_type)
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

//   let func_name = util.snake_case(param_type.name)
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
        case util.fetch_custom_type(ident, module_reader) {
          Error(err) -> {
            io.debug(err)
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
      io.debug(field_opt)
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

//     io.debug(param_type.name)
//     io.debug(return_field)
//     let overrides =
//       overrides
//       |> list.filter_map(fn(x) {
//         let #(df, os) = x

//         io.debug(df.type_)
//         io.debug(df.field)

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
//         io.debug(param_type)
//         io.debug(param_variant)
//         io.debug(param_field)
//         panic as "`into` param field doesn't exist"
//       }

//       Ok(param_field_type) if param_field_type == result_field_type ->
//         param_field_type

//       _ -> {
//         io.debug("PARAM TYPE")
//         io.debug(param_type)
//         io.debug(param_variant)
//         io.debug(param_field)
//         io.debug("RETURN TYPE")
//         io.debug(return_type)
//         io.debug(return_variant)
//         io.debug(return_field)
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
  ) = uf

  let dummy_span = Span(-1, -1)

  let return_constr =
    case return_alias {
      None -> Variable(return_constr)
      Some(module) -> FieldAccess(Variable(module), return_constr)
    }

  Definition([], Function(func_name, Public,
    [FunctionParameter(None, Named("value"), Some(NamedType(param_type, param_alias, [])))],
    Some(NamedType(return_type, return_alias, [])),
    [Expression(Call(return_constr, list.map(fields, fn(field) {
      LabelledField(field.return_field, FieldAccess(Variable("value"), field.param_field))
    })))], dummy_span)
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
  prefix: String,
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

  let fields = build_fields(m)

  case m.direction {
    LocalToRemote -> {
      IntoFunc(
        func_name: prefix <> util.snake_case(m.remote_type.name),
        param_type: m.local_type.name,
        param_alias: None,
        return_type: m.remote_type.name,
        return_constr: m.remote_variant.name,
        return_alias: m.remote_alias,
        fields:,
      )
    }

    RemoteToLocal ->
      IntoFunc(
        func_name: prefix <> util.snake_case(m.local_type.name),
        param_type: m.remote_type.name,
        param_alias: m.remote_alias,
        return_type: m.local_type.name,
        return_constr: m.local_variant.name,
        return_alias: None,
        fields:,
      )
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

  // RemoteToLocal (UNIFY) // overrides refer to remote field as return
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

fn build_fields(
  m: Mapping,
) -> List(Field) {
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
      let return_fields = fields(return_variant)

      // io.debug(param_fields)
      // io.debug(return_fields)

      return_fields
      |> list.map(fn(r_field) {
        io.debug(r_field)
        io.debug(r_field)
        io.debug(r_field)
        io.debug(r_field)
        let #(return_field, result_field_type) = r_field

        let #(param_field, param_field_type) =
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
              Ok(field) -> field
              Error(Nil) -> {
                let field =
                  param_fields
                  |> list.find(fn(field) {
                    let #(name, _type) = field

                    name == return_field
                  })

                case field {
                  Ok(field) -> field
                  Error(Nil) ->
                    panic as { "unable to find `param_field` for: " <> return_field }
                }
              }
            }
          }

        Field(
          param_field:,
          return_field:,
        )
      })
    }

    RemoteToLocal -> {
      let param_type_module = remote_type_module

      let #(
        param_type,
        param_variant,
        param_module,
        return_type,
        return_variant,
        return_module,
      ) =
        #(
          remote_type, // param_type
          remote_variant, // param_variant
          Some(remote_type_module), // param_module
          local_type, // return_type
          local_variant, // return_variant
          None, // return_module
        )

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
                True -> Ok(#(param_field, o.override))
              }
            })
          })

        let #(param_field, _p_type) =
          case override {
            Error(_) ->  #(return_field, Nil)
            Ok(#(_param_field, override)) -> #(override, Nil)
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
            Nil

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
          param_field:,
          return_field:,
        )
      })
    }
  }
}

fn into_(
  local_type: CustomType,
  opts: List(String),
  overrides: IntoFieldOverrides,
  module_reader: ModuleReader,
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
            io.debug(opts)
            panic as "invalid `into` opts"
          }
        }

      [ident]
      |> get_return_types_and_variants(module_reader)
      |> list.map(fn(x) {
        let #(remote_type_def, remote_variant, remote_type_module) = x
        let remote_type = remote_type_def.definition

        Mapping(
          direction: LocalToRemote,
          local_type:,
          local_variant:,
          remote_type:,
          remote_variant:,
          remote_alias:,
          remote_type_module:,
          overrides:,
        )
        |> into_variant_("into_")
      })
    }
    _, -> {
      io.debug(local_type)
      panic as "`into` derivation currently only supports invariant types"
    }
  }
}
