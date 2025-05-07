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
      init_acc(),
      prefix: type_.name,
      module_name:,
      module:,
      module_reader:,
      wrap: None,
    )

  let fields = fields |> list.map(fn(f) { f.id })

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
  case type_ |> io.debug {
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
    id: String, // e.g. `"SomeFormField"`
    segments: List(String), //
    // name: String, // e.g. `"[field]"`
    // label: String, // e.g. `"Field"`
    gleam_type: form.GleamType, // e.g. `form.String`
    // required: Bool,
  )
}

fn to_field(
  id id: String,
  gleam_type gleam_type: form.GleamType,
  acc acc: Acc,
) -> Field {
  Field(
    id:,
    segments: acc.fields,
    // name: id,
    // label: id,
    gleam_type:,
    // required: False,
  )
}

type Acc {
  Acc(
    fields: List(String),
    // build_type: fn(form.GleamType) -> form.GleamType,
    // constructors: List(String),
    // outer: List(OuterType),
  )
}

fn init_acc() -> Acc {
  Acc(
    fields: [],
    // build_type: fn(x) { x },
    // constructors: [],
    // outer: [],
  )
}

fn gen_form_fields(
  variant: Variant,
  acc: Acc,
  prefix prefix: String,
  module_name module_name: String,
  module module: glance.Module,
  module_reader module_reader: ModuleReader,
  wrap wrap: Option(fn(form.GleamType) -> form.GleamType),
) -> List(Field) {
  variant.fields
  |> list.flat_map(fn(field) {
    case field {
      LabelledVariantField(label: name, item: field_type) -> {
        case gleam_type_for(field_type) {
          Ok(gleam_type) ->
            { prefix <> { name |> util.pascal_case } }
            |> to_field(id: _, gleam_type:, acc: {
              Acc(..acc, fields: acc.fields |> list.append([name]))
            })
            |> list.wrap

          _not_simple_gleam_type -> {
            let field_type = base_type_(field_type) // TODO recurse properly

            case util.look_up_type(field_type, module, module_reader) {
              Ok(t) ->
                case is_enum_type(t), t {
                  True, _ -> {
                    { prefix <> { name |> util.pascal_case } }
                    |> to_field(id: _, gleam_type: form.Enum(ident: "?." <> t.name), acc: {
                      Acc(..acc, fields: acc.fields |> list.append([name]))
                    })
                    |> list.wrap
                  }

                  False, CustomType(variants: [variant], ..) -> {
                    let prefix = prefix <> { name |> util.pascal_case }

                    let acc =
                      Acc(
                        fields: acc.fields |> list.append([name]),
                        // build_type: todo,
                        // constructors: acc.constructors |> list.append([variant.name]),
                      )

                    gen_form_fields(variant, acc, prefix:, module_name:, module:, module_reader:, wrap:)
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
        }
      }

      _ -> {
        io.debug(variant)
        io.debug(field)
        panic as "`derive form` only supports labelled fields"
      }
    }
    |> io.debug
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

  let src =
    ""
    // funcs
    // |> list.map(util.func_str)
    // |> string.join("\n\n")

  Gen(file:, deriv:, imports:, types:, funcs:, src:, meta: dict.new())
}

// fn build_field_overrides(
//   field_opts: DerivFieldOpts,
//   module_reader: ModuleReader,
// ) -> IntoFieldOverrides {
//   field_opts
//   |> dict.map_values(fn(field, opts) {
//     opts
//     |> list.map(build_field_override(_, field, module_reader))
//   })
//   |> dict.to_list
// }

// type IntoFunc {
//   IntoFunc(
//     func_name: String,
//     param_type: String,
//     param_alias: Option(String),
//     return_type: String,
//     return_constr: String,
//     return_alias: Option(String),
//     fields: List(Field),
//   )
// }
// // func_name: String, // "authe_type_a"
// // param_type: String, // "AutheTypeA"
// // return_type: String, // "AutheTokens"
// // return_constr: String, // "Authe"
// // fields: List(#(String, String)),
// // // [
// // //   #("id", "authe_id"),
// // //   #("encrypted_access_token", "encrypted_access_token"),
// // //   #("encrypted_refresh_token", "encrypted_refresh_token"),
// // // ]

// fn get_return_types_and_variants(
//   idents: List(String),
//   module_reader: ModuleReader,
// ) -> List(#(Definition(CustomType), Variant, String)) {
//   idents
//   |> list.map(fn(ident) {
//     util.fetch_custom_type(ident, module_reader)
//   })
//   |> result.all
//   |> fn(x) {
//     case x {
//       Error(err) -> {
//         io.debug(idents)
//         io.debug(err)
//         panic as "`into` issue with the above `idents`"
//       }

//       Ok(list) ->
//         list
//     }
//   }
//   |> list.map(fn(x) {
//     let #(module_name, type_) = x

//     case type_.definition.variants {
//       [variant] ->
//         #(type_, variant, module_name)

//       _, -> {
//         io.debug(type_)
//         panic as "`into` derivation currently only supports invariant types"
//       }
//     }
//   })
// }

// // fn into(
// //   param_type: CustomType,
// //   opts: List(String),
// //   overrides: IntoFieldOverrides,
// //   module_reader: ModuleReader,
// // ) -> List(IntoFunc) {
// //   // param_type param_type: CustomType,
// //   // param_variant param_variant: Variant,
// //   // return_type return_type: CustomType,
// //   // return_variant return_variant: Variant,
// //   // return_type_module return_type_module: String,
// //   // overrides overrides: IntoFieldOverrides,
// //   case param_type.variants {
// //     [param_variant] -> {
// //       let #(ident, _alias) =
// //         case opts {
// //           [ident] ->
// //             #(ident, None)

// //           [ident, "as", alias] ->
// //             #(ident, Some(alias))

// //           _ -> {
// //             io.debug(opts)
// //             panic as "invalid `into` opts"
// //           }
// //         }

// //       [ident]
// //       |> get_return_types_and_variants(module_reader)
// //       |> list.map(fn(x) {
// //         let #(return_type_def, return_variant, return_type_module) = x
// //         let return_type = return_type_def.definition

// //         into_variant(
// //           param_type:,
// //           param_variant:,
// //           return_type:,
// //           return_variant:,
// //           return_type_module:,
// //           overrides:,
// //         )
// //       })
// //     }
// //     _, -> {
// //       io.debug(param_type)
// //       panic as "`into` derivation currently only supports invariant types"
// //     }
// //   }
// // }

// // fn into_variant(
// //   param_type param_type: CustomType,
// //   param_variant param_variant: Variant,
// //   return_type return_type: CustomType,
// //   return_variant return_variant: Variant,
// //   return_type_module return_type_module: String,
// //   overrides overrides: IntoFieldOverrides,
// // ) -> IntoFunc {
// //   let fields = into_func_fields(
// //     return_type_module:,
// //     param_type:,
// //     param_variant:,
// //     return_type:,
// //     return_variant:,
// //     overrides:,
// //   )

// //   let func_name = util.snake_case(param_type.name)
// //   let param_type = param_type.name
// //   let return_type = return_type.name
// //   let return_constr = return_variant.name

// //   IntoFunc(
// //     func_name:,
// //     param_type:,
// //     param_alias:,
// //     return_type:,
// //     return_constr:,
// //     return_alias:,
// //     fields:,
// //   )
// // }

// // fn into_func_fields(
// //   param_variant: Variant,
// //   return_variant: Variant,
// //   overrides: IntoFieldOverrides,
// // ) -> List(Field) {
// //   return_variant.fields
// //   |> list.map(fn(f1) {
// //     // #(f1.item, list.find(param_variant.field, fn(f2) { f1.name == f2.name}))
// //   })
// // }

// type Field {
//   Field(
//     // type_: String,
//     // param_field_override: Option(String),
//     param_field: String,
//     return_field: String,
//   )
// }

// type IntoFieldOverrides = List(#(DerivField, List(IntoFieldOverride)))

// type IntoFieldOverride {
//   IntoFieldOverride(
//     ident: String,
//     field: String,
//     override: String,
//     module_name: String,
//     type_: CustomType,
//   )
// }

// fn build_field_override(
//   field_opt: DerivFieldOpt,
//   deriv_field: DerivField,
//   module_reader: ModuleReader,
// ) -> IntoFieldOverride {
//   case field_opt {
//     DerivFieldOpt(strs: ["into", "field", ident, field]) -> {
//       let #(module_name, type_) =
//         case util.fetch_custom_type(ident, module_reader) {
//           Error(err) -> {
//             io.debug(err)
//             panic
//           }

//           Ok(#(m, td)) -> #(m, td.definition)
//         }

//       let override = deriv_field.field

//       IntoFieldOverride(
//         ident:,
//         field:,
//         override:,
//         module_name:,
//         type_:,
//       )
//     }

//     _ -> {
//       io.debug(field_opt)
//       panic as "Invalid `into` `DerivFieldOpt` (printed above)"
//     }
//   }
// }

// // fn field_overrides(
// //   ident: String,
// //   field_name: String,
// //   overrides: IntoFieldOverrides,
// // ) -> Option(String) {
// //   overrides
// //   |> list.find_map(fn(x) {
// //     let #(f, opts) = x
// //     case f.field == field_name {
// //       False ->
// //         Error(Nil)

// //       True -> {
// //         list.find_map(opts, fn(opt) {
// //           case opt {
// //             IntoFieldOverride(override:, ..) as uf -> {
// //               case uf.ident == ident {
// //                 False -> Error(Nil)
// //                 True -> Ok(override)
// //               }
// //             }
// //           }
// //         })
// //       }
// //     }
// //   })
// //   |> option.from_result
// // }

// fn build_ident(
//   module_name: String,
//   type_: CustomType,
// ) -> String {
//   module_name <> "." <> type_.name
// }

// // fn into_func_fields(
// //   param_type param_type: CustomType,
// //   param_variant param_variant: Variant,
// //   return_type return_type: CustomType,
// //   return_variant return_variant: Variant,
// //   return_type_module return_type_module: String,
// //   overrides overrides: IntoFieldOverrides,
// // ) -> List(Field) {
// //   let param_fields = fields(param_variant)
// //   let return_fields = fields(return_variant)

// //   param_fields
// //   |> list.map(fn(r_field) {
// //     let #(return_field, result_field_type) = r_field

// //     io.debug(param_type.name)
// //     io.debug(return_field)
// //     let overrides =
// //       overrides
// //       |> list.filter_map(fn(x) {
// //         let #(df, os) = x

// //         io.debug(df.type_)
// //         io.debug(df.field)

// //         case df.type_ == param_type.name && df.field == return_field {
// //           False -> Error(Nil)
// //           True -> Ok(#(df.field, os))
// //         }
// //       })

// //     let override =
// //       overrides
// //       |> list.find_map(fn(x) {
// //         let #(param_field, os) = x

// //         list.find_map(os, fn(o) {
// //           let ident = build_ident(return_type_module, return_type)
// //           case o.ident == ident && o.field == return_field {
// //             False -> Error(Nil)
// //             True -> Ok(#(param_field, o))
// //           }
// //         })
// //       })

// //     let #(param_field, _p_type) =
// //       case override {
// //         Error(_) -> #(return_field, Nil)
// //         Ok(#(_param_field, o)) -> #(o.override, Nil)
// //       }

// //     let param_field_type =
// //       list.find_map(return_fields, fn(x) {
// //         let #(name, type_) = x
// //         case param_field == name {
// //           False -> Error(Nil)
// //           True -> Ok(type_)
// //         }
// //       })

// //     case param_field_type {
// //       Error(_) -> {
// //         io.debug(param_type)
// //         io.debug(param_variant)
// //         io.debug(param_field)
// //         panic as "`into` param field doesn't exist"
// //       }

// //       Ok(param_field_type) if param_field_type == result_field_type ->
// //         param_field_type

// //       _ -> {
// //         io.debug("PARAM TYPE")
// //         io.debug(param_type)
// //         io.debug(param_variant)
// //         io.debug(param_field)
// //         io.debug("RETURN TYPE")
// //         io.debug(return_type)
// //         io.debug(return_variant)
// //         io.debug(return_field)
// //         panic as "`into` param & return field types don't match"
// //       }
// //     }

// //     Field(
// //       // param_field_override:,
// //       param_field:,
// //       return_field:,
// //     )
// //   })
// // }

// fn fields(variant: Variant) -> List(#(String, String)) {
//   variant.fields
//   |> list.map(fn(field) {
//     case field {
//       LabelledVariantField(item: NamedType(name:, ..), label:) ->
//         #(label, name)

//       _ -> {
//         io.debug(variant)
//         io.debug(field)
//         panic as "Only the following field type is supported: `LabelledVariantField(item: NamedType(name:, ..), label:)`"
//       }
//     }
//   })
// }

// fn into_func(
//   uf: IntoFunc
// ) -> Definition(Function) {
//   let IntoFunc(
//     func_name:,
//     param_type:,
//     param_alias:,
//     return_type:,
//     return_constr:,
//     return_alias:,
//     fields:,
//   ) = uf

//   let dummy_span = Span(-1, -1)

//   let return_constr =
//     case return_alias {
//       None -> Variable(return_constr)
//       Some(module) -> FieldAccess(Variable(module), return_constr)
//     }

//   Definition([], Function(func_name, Public,
//     [FunctionParameter(None, Named("value"), Some(NamedType(param_type, param_alias, [])))],
//     Some(NamedType(return_type, return_alias, [])),
//     [Expression(Call(return_constr, list.map(fields, fn(field) {
//       LabelledField(field.return_field, FieldAccess(Variable("value"), field.param_field))
//     })))], dummy_span)
//   )
// }


// // // // // //

// type Direction {
//   LocalToRemote
//   RemoteToLocal
// }

// type Mapping {
//   Mapping(
//     direction: Direction,
//     local_type: CustomType,
//     local_variant: Variant,
//     remote_type: CustomType,
//     remote_variant: Variant,
//     remote_alias: Option(String),
//     remote_type_module: String,
//     overrides: IntoFieldOverrides,
//   )
// }

// // type IntoFunc1 {
// //   IntoFunc1(
// //     func_name: String,
// //     param_type: String,
// //     return_type: String,
// //     return_constr: String,
// //     fields: List(Field),
// //   )
// // }

// // type Field1 {
// //   Field1(
// //     // type_: String,
// //     // param_field_override: Option(String),
// //     remote_field: String,
// //     local_field: String,
// //   )
// // }

// fn into_variant_(
//   m: Mapping,
//   prefix: String,
// ) -> IntoFunc {
//   // let Mapping(
//   //   direction:,
//   //   local_type:,
//   //   local_variant:,
//   //   remote_type:,
//   //   remote_variant:,
//   //   remote_type_module:,
//   //   overrides:,
//   // ) = m

//   // let param_type = param_type.name
//   // let return_type = return_type.name
//   // let return_constr = return_variant.name

//   let fields = build_fields(m)

//   case m.direction {
//     LocalToRemote -> {
//       IntoFunc(
//         func_name: prefix <> util.snake_case(m.remote_type.name),
//         param_type: m.local_type.name,
//         param_alias: None,
//         return_type: m.remote_type.name,
//         return_constr: m.remote_variant.name,
//         return_alias: m.remote_alias,
//         fields:,
//       )
//     }

//     RemoteToLocal ->
//       IntoFunc(
//         func_name: prefix <> util.snake_case(m.local_type.name),
//         param_type: m.remote_type.name,
//         param_alias: m.remote_alias,
//         return_type: m.local_type.name,
//         return_constr: m.local_variant.name,
//         return_alias: None,
//         fields:,
//       )
//   }
// }

// type IntoFieldOverride1 {
//   IntoFieldOverride1(
//     local_type: String,
//     local_variant: String,
//     local_field: String,
//     remote_ident: String,
//     remote_field: String,
//     remote_override: String,
//     remote_module_name: String,
//     remote_type: CustomType,
//   )
// }

// fn conv(
//   xs: List(#(DerivField, List(IntoFieldOverride))),
// ) -> List(IntoFieldOverride1) {
//   list.flat_map(xs, fn(x) {
//     let #(df, os) = x

//     list.map(os, fn(o) {
//       IntoFieldOverride1(
//         local_type: df.type_,
//         local_variant: df.variant,
//         local_field: df.field,
//         remote_ident: o.ident,
//         remote_field: o.field,
//         remote_override: o.override,
//         remote_module_name: o.module_name,
//         remote_type: o.type_,
//       )
//     })
//   })
// }

// fn get_return_field_override(
//   field: String,
//   m: Mapping,
// ) -> Result(String, Nil) {
//   case m.direction {
//     RemoteToLocal ->
//       Ok(field)

//     LocalToRemote -> {
//       let os = conv(m.overrides)

//       todo
//     }
//   }
// }

// type FieldRole {
//   Param
//   Return
// }

// // fn fields(variant: Variant) -> List(#(String NAME, String TYPE)) {
// fn fields_(
//   variant: Variant,
//   role: FieldRole,
//   m: Mapping,
// ) -> List(#(String, String)) {
//   let os = conv(m.overrides)
//   let fs = fields(variant)

//   // RemoteToLocal (UNIFY) // overrides refer to remote field as return
//   // LocalToRemote (INTO)  // overrides refer to remote field as param

//   case m.direction, role {
//     LocalToRemote, Return ->
//       fs

//     RemoteToLocal, Param ->
//       fs

//     LocalToRemote, Param ->
//       todo // override (off remote key, `IntoFieldOverride`)

//     RemoteToLocal, Return ->
//       todo // override (off local key, `DerivField`)
//   }
// }

// fn build_fields(
//   m: Mapping,
// ) -> List(Field) {
//   let Mapping(
//     local_type:,
//     local_variant:,
//     remote_type:,
//     remote_variant:,
//     remote_type_module:,
//     overrides:,
//     ..
//   ) = m

//   case m.direction {
//     LocalToRemote -> {
//       let #(
//         param_type,
//         param_variant,
//         param_module,
//         return_type,
//         return_variant,
//         return_module,
//       ) =
//         #(
//           local_type, // param_type
//           local_variant, // param_variant
//           None, // param_module
//           remote_type, // return_type
//           remote_variant, // return_variant
//           Some(remote_type_module), // return_module
//         )

//       let param_fields = fields(param_variant)
//       let return_fields = fields(return_variant)

//       // io.debug(param_fields)
//       // io.debug(return_fields)

//       return_fields
//       |> list.map(fn(r_field) {
//         io.debug(r_field)
//         io.debug(r_field)
//         io.debug(r_field)
//         io.debug(r_field)
//         let #(return_field, result_field_type) = r_field

//         let #(param_field, param_field_type) =
//           overrides
//           |> list.find_map(fn(x) {
//             let #(df, os) = x

//             os
//             |> list.reverse // prefer override defined later // TODO panic dupe
//             |> list.find_map(fn(o) {
//               case o.field == return_field && o.type_ == remote_type && o.module_name == remote_type_module {
//                 True -> Ok(#(df.field, df.type_))
//                 False -> Error(Nil)
//               }
//             })
//           })
//           |> fn(r) {
//             case r {
//               Ok(field) -> field
//               Error(Nil) -> {
//                 let field =
//                   param_fields
//                   |> list.find(fn(field) {
//                     let #(name, _type) = field

//                     name == return_field
//                   })

//                 case field {
//                   Ok(field) -> field
//                   Error(Nil) ->
//                     panic as { "unable to find `param_field` for: " <> return_field }
//                 }
//               }
//             }
//           }

//         Field(
//           param_field:,
//           return_field:,
//         )
//       })
//     }

//     RemoteToLocal -> {
//       let param_type_module = remote_type_module

//       let #(
//         param_type,
//         param_variant,
//         param_module,
//         return_type,
//         return_variant,
//         return_module,
//       ) =
//         #(
//           remote_type, // param_type
//           remote_variant, // param_variant
//           Some(remote_type_module), // param_module
//           local_type, // return_type
//           local_variant, // return_variant
//           None, // return_module
//         )

//       let param_fields = fields(param_variant)
//       let return_fields = fields(return_variant)

//       return_fields
//       |> list.map(fn(r_field) {
//         let #(return_field, result_field_type) = r_field

//         let overrides =
//           overrides
//           |> list.filter_map(fn(x) {
//             let #(df, os) = x

//             case df.type_ == return_type.name && df.field == return_field {
//               False -> Error(Nil)
//               True -> Ok(#(df.field, os))
//             }
//           })

//         let override =
//           overrides
//           |> list.find_map(fn(x) {
//             let #(param_field, os) = x

//             list.find_map(os, fn(o) {
//               let ident = build_ident(param_type_module, param_type)
//               case o.ident == ident && o.field == return_field {
//                 False -> Error(Nil)
//                 True -> Ok(#(param_field, o.override))
//               }
//             })
//           })

//         let #(param_field, _p_type) =
//           case override {
//             Error(_) ->  #(return_field, Nil)
//             Ok(#(_param_field, override)) -> #(override, Nil)
//           }

//         let param_field_type =
//           list.find_map(param_fields, fn(x) {
//             let #(name, type_) = x
//             case param_field == name {
//               False -> Error(Nil)
//               True -> Ok(type_)
//             }
//           })

//         case param_field_type {
//           Error(_) -> {
//             io.debug(param_type)
//             io.debug(param_variant)
//             io.debug(param_field)
//             panic as "`unify` param field doesn't exist"
//           }

//           Ok(param_field_type) if param_field_type == result_field_type ->
//             Nil

//           _ -> {
//             io.println("PARAM TYPE")
//             io.debug(param_type)
//             io.debug(param_variant)
//             io.debug(param_field)
//             io.println("RETURN TYPE")
//             io.debug(return_type)
//             io.debug(return_variant)
//             io.debug(return_field)
//             panic as "`unify` param & return field types don't match"
//           }
//         }

//         Field(
//           param_field:,
//           return_field:,
//         )
//       })
//     }
//   }
// }

// fn into_(
//   local_type: CustomType,
//   opts: List(String),
//   overrides: IntoFieldOverrides,
//   module_reader: ModuleReader,
// ) -> List(IntoFunc) {
//   case local_type.variants {
//     [local_variant] -> {
//       let #(ident, remote_alias) =
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
//         let #(remote_type_def, remote_variant, remote_type_module) = x
//         let remote_type = remote_type_def.definition

//         Mapping(
//           direction: LocalToRemote,
//           local_type:,
//           local_variant:,
//           remote_type:,
//           remote_variant:,
//           remote_alias:,
//           remote_type_module:,
//           overrides:,
//         )
//         |> into_variant_("into_")
//       })
//     }
//     _, -> {
//       io.debug(local_type)
//       panic as "`into` derivation currently only supports invariant types"
//     }
//   }
// }
