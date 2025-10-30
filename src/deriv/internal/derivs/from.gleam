import gleam/dict
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess, Span}
import glance as g
import deriv/internal/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivFieldOpt, DerivField} as deriv
import deriv/internal/common

pub type GenFunc = fn(CustomType, Derivation, DerivFieldOpts, File) -> Gen

const x = g.Span(-1, -1)

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/from` "

    deriv.Type(type_:) -> {
      let overrides = build_field_overrides(opts, module_reader)

      let idents = deriv.opts

      let imports = []

      let funcs =
        from(
          type_,
          idents,
          overrides,
          module_reader,
          opts,
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
  _module_reader: ModuleReader,
) -> FromFieldOverrides {
  field_opts
  |> dict.map_values(fn(field, opts) {
    opts
    |> list.filter_map(build_field_override(_, field))
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
  opts: DerivFieldOpts,
) -> List(FromFunc) {
  case return_type.variants {
    [return_variant] ->
      idents
      |> get_param_types_and_variants(module_reader)
      |> list.map(fn(x) {
        let #(param_type_def, param_variant, param_type_module) = x
        let param_type = param_type_def.definition

        from_variant_(
          param_type_module,
          param_type,
          param_variant,
          return_type,
          return_variant,
          overrides,
          opts,
        )
      })

    _, -> {
      common.debug(return_type)
      panic as "`from` derivation currently only supports single-variant types"
    }
  }
}

fn from_variant_(
  param_type_module: String,
  param_type: CustomType,
  param_variant: Variant,
  return_type: CustomType,
  return_variant: Variant,
  overrides: FromFieldOverrides,
  opts: DerivFieldOpts,
) -> FromFunc {
  let fields =
  return_variant
  |> fields(type_: return_type, variant: _)
  |> list.map(fn(t) {
    let #(field, _type) = t
    field
  })
  |> list.map(from_func_field(field: _, opts:))

  let from = common.snake_case(param_type.name)
  let to = common.snake_case(return_type.name)

  let func_name = "from_" <> from <> "_to_" <> to
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

  let from = common.snake_case(param_type.name)
  let to = common.snake_case(return_type.name)

  let func_name = "from_" <> from <> "_to_" <> to
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
    conv: Option(Conv),
  )
}

// type Field1 {
//   Field1(
//     param_field: Option(String),
//     return_field: String,
//     conv: Option(FromFieldOverrideConv),
//   )
// }

// fn build_field(
//   param_type_module: String,
//   param_type: CustomType,
//   _param_variant: Variant,
//   return_type: CustomType,
//   return_variant: Variant,
//   return_field: glance.VariantField,
//   overrides: FromFieldOverrides,
// ) -> Field1 {

//   todo


//   // Field1(
//   //   param_field:,
//   //   return_field:,
//   //   conv:,
//   // )
// }

// fn parse_override(
//   param_type_module: String,
//   param_type: CustomType,
//   _param_variant: Variant,
//   return_type: CustomType,
//   return_variant: Variant,
//   return_field: glance.VariantField,
//   overrides: FromFieldOverrides,
// ) -> Field1 {

//   todo


//   // Field1(
//   //   param_field:,
//   //   return_field:,
//   //   conv:,
//   // )
// }

type FromFieldOverrides = List(#(DerivField, List(FromFieldOverride)))

pub type FromFieldOverride {
  FromFieldOverride(
    ident: String,
    field: String,
    override: String,
    module_name: String,
    // type_: CustomType,
    using: Option(FromFieldOverrideConv),
  )
}

pub type FromFieldOverrideConv {
  FromFieldOverrideConv(
    module: Option(String),
    func: String,
    args: FromFieldOverrideConvArgs,
  )
}

pub type FromFieldOverrideConvArgs {
  EntireValue
  ValueDotField
}

fn parse_ident_with_field(
  ident ident: String,
) -> #(String, String, Option(String)) {
  case string.split(ident, ".") {
    [""] -> #("", "", None)
    [a, b] -> {
      case a, b, starts_with_uppercase(b) {
        module, type_, True -> #(module, module <> "." <> type_, None)
        type_, field, False -> #("", type_, Some(field))
      }
    }
    [module, type_, field] -> #(module, module <> "." <> type_, Some(field))
    [type_] -> #("", type_, None)
    // _ -> panic // TODO panic w/ error
    _ -> #("", "", None)
  }
}

type Ident {
  IdentFieldForType(
    module: Option(String),
    type_: String,
    field: String,
  )
  IdentType(
    module: Option(String),
    type_: String,
  )
}

fn starts_with_uppercase(str: String) -> Bool {
  str
  |> string.first
  |> result.map(fn(ch) { ch == string.uppercase(ch) })
  |> result.unwrap(False)
}

fn parse_ident(
  ident ident: String,
) -> Result(Ident, Nil) {
  case ident |> string.split(".") {
    [""] -> Error(Nil)
    [type_] ->
      case starts_with_uppercase(type_) {
        True -> Ok(IdentType(module: None, type_:))
        False -> Error(Nil)
      }
    [module, type_, field] ->
      case starts_with_uppercase(type_) {
        True -> Ok(IdentFieldForType(module: Some(module), type_:, field:))
        False -> Error(Nil)
      }
    [a, b] ->
      case starts_with_uppercase(a), starts_with_uppercase(b), a, b {
        True, False, type_, field -> Ok(IdentFieldForType(module: None, type_:, field:))
        False, True, module, type_ -> Ok(IdentType(module: Some(module), type_:))
        _, _, _, _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

type Conv {
  Conv(
    module: Option(String),
    func: String,
  )
}

type Override {
  SpecifyField(ident: Ident, field: String)
  ConvAllWith(conv: Conv)
  ConvTypeWith(ident: Ident, conv: Conv)
}

fn match_specific(
  field f: DerivField,
  overrides overrides: List(Override),
) -> Result(Override, Nil) {
  overrides
  |> list.filter(fn(override) {
    case override {
      SpecifyField(ident: IdentFieldForType(type_:, field:, ..), ..) |
      ConvTypeWith(ident: IdentFieldForType(type_:, field:, ..), ..) -> {
        f.type_ == type_ && f.field == field
      }

      _ -> {
        False
      }
    }
  })
  |> fn(os) {
    case os {
      [_, _, ..] -> panic as {
        [
          "`from` found multiple options specifically for the same field, namely:\n",
          f |> string.inspect,
          .. os |> list.map(string.inspect),
        ]
        |> string.join("")
      }
      [o] -> Ok(o)
      [] -> Error(Nil)
    }
  }
}

fn match_general(
  field f: DerivField,
  overrides overrides: List(Override),
) -> Result(Override, Nil) {
  overrides
  |> list.filter(fn(override) {
    case override {
      SpecifyField(ident: IdentFieldForType(type_:, field:, ..), ..) |
      ConvTypeWith(ident: IdentFieldForType(type_:, field:, ..), ..) -> {
        f.type_ == type_ && f.field == field
      }

      _ -> {
        False
      }
    }
  })
  |> fn(os) {
    case os {
      [o, _, ..] -> {
        [
          "`from` found multiple options generally matching the following field:\n",
          f |> string.inspect,
          .. os |> list.map(string.inspect),
        ]
        |> string.join("")
        |> io.println_error

        Ok(o)
      }
      [o] -> Ok(o)
      [] -> Error(Nil)
    }
  }
}

fn build_field_override_(
  field_opt: DerivFieldOpt,
) -> Result(Override, Nil) {
  case field_opt.strs {
    ["from", ..rest] -> {
      case rest {
        [ident] -> {
          result.try(parse_ident(ident:), fn(ident) {
            case ident {
              IdentType(..) ->
                Error(Nil)

              IdentFieldForType(field:, ..) ->
                Ok(SpecifyField(ident:, field:))
            }
          })
        }

        ["using", conv] -> {
          Ok(ConvAllWith(conv: conv |> parse_conv_or_panic))
        }

        [ident, "using", conv] -> {
          result.try(parse_ident(ident:), fn(ident) {
            Ok(ConvTypeWith(ident:, conv: conv |> parse_conv_or_panic))
          })
        }

        _ -> {
          panic as { "`from` invalid field option: " <> field_opt.strs |> string.join(" ") }
        }
      }
    }

    _ -> {
      Error(Nil)
    }
  }
}

fn parse_conv_or_panic(
  str str: String,
) -> Conv {
  case str |> string.split(".") {
    [""] -> panic as { "`from` must specify a convert function for `using`" }
    [func] -> Conv(module: None, func:)
    [module, func] -> Conv(module: Some(module), func:)
    _ -> panic as {
      "`from` field convert function specified by `using` is invalid. Valid syntax is `func_name` or `module.func_name`, but got: " <> str
    }
  }
}


pub fn build_field_override(
  field_opt: DerivFieldOpt,
  deriv_field: DerivField,
) -> Result(FromFieldOverride, Nil) {
  case field_opt, "" {
    // DerivFieldOpt(strs: ["from", ident]), _ ->
    //   // 1. `from mod/Type.field`
    //   // 2. `from mod/Type`
    //   // 3. `from mod`
    //   todo

    DerivFieldOpt(strs: ["from", ident_with_field]), _ as conv |
    DerivFieldOpt(strs: ["from", ident_with_field, "using", conv]), _ |
    DerivFieldOpt(strs: ["from", "using", conv]), _ as ident_with_field -> {
      let #(module_name, ident, override) = parse_ident_with_field(ident_with_field)

      let args =
        case override, ident_with_field |> string.is_empty {
          Some(_specified_field), _ -> ValueDotField
          None, True -> ValueDotField
          None, False -> EntireValue
        }

      let conv =
        case conv |> string.split(".") {
          [""] -> None
          [func] -> Some(FromFieldOverrideConv(module: None, func:, args:))
          [module, func] -> Some(FromFieldOverrideConv(module: Some(module), func:, args:))
          _ -> panic as {
            "`from` field conv func specification invalid. Valid syntax is `func_name` or `module.func_name`, but got: " <> string.inspect(field_opt.strs |> string.join(" "))
          }
        }

      // let module_name =
      //   case common.fetch_custom_type(ident, module_reader) {
      //     Ok(#(m, _td)) -> m
      //     Error(_err) -> ""
      //   }
      // let #(module_name, type_) =
      //   case common.fetch_custom_type(ident, module_reader) {
      //     Error(err) -> {
      //       common.debug(err)
      //       panic
      //     }

      //     Ok(#(m, td)) -> #(m, td.definition)
      //   }

      let field = deriv_field.field

      let override =
        case override, conv {
          Some(override), _ -> override
          _, Some(_conv), -> "" // TODO zero value, as unused in this case, but confusing
          None, _ -> panic as {
            "`from` field override must specify either a field or conversion func" // TODO better err
          }
        }

      Ok(FromFieldOverride(
        ident:,
        field:,
        override:,
        module_name:,
        // type_:,
        using: conv,
      ))
    }

    _, _ -> {
      Error(Nil)
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

fn from_func_field(
  field field: DerivField,
  opts opts: DerivFieldOpts,
) -> Field {
  let overrides =
    opts
    |> dict.get(field)
    |> result.unwrap([])
    |> list.map(build_field_override_)
    |> result.values

  let override =
    match_specific(field:, overrides:)
    |> result.lazy_or(fn() {
      match_general(field:, overrides:)
    })

  let #(param_field, conv) =
    case override {
      Error(Nil) ->
        #(field.field, None)

      Ok(SpecifyField(field:, ..)) ->
        #(field, None)

      Ok(ConvAllWith(conv:)) ->
        #(field.field, Some(conv))

      Ok(ConvTypeWith(ident: _, conv:)) ->
        #(field.field, Some(conv))
    }

  Field(
    param_field:,
    return_field: field.field,
    conv:,
  )
  |> echo
}

// fn from_func_fields(
//   param_type_module: String,
//   param_type: CustomType,
//   _param_variant: Variant,
//   return_type: CustomType,
//   return_variant: Variant,
//   overrides: FromFieldOverrides,
// ) -> List(Field) {
//   let return_fields = fields(type_: return_type, variant: return_variant)

//   return_fields
//   |> list.map(fn(r_field) {
//     let #(df, _result_field_type) = r_field
//     let return_field = df.field

//     let overrides =
//       overrides
//       |> list.filter_map(fn(x) {
//         let #(df, os) = x

//         case df.type_ == return_type.name && df.field == return_field {
//           False -> Error(Nil)
//           True -> Ok(#(df.field, os))
//         }
//       })

//     let specfic_override =
//       overrides
//       |> list.find_map(fn(x) {
//         let #(param_field, os) = x

//         list.find_map(os, fn(o) {
//           let ident = build_ident(param_type_module, param_type)
//           case o.ident == ident && o.field == return_field {
//             False -> Error(Nil)
//             True -> Ok(#(param_field, o))
//           }
//         })
//       })

//     let general_override = fn() {
//       overrides
//       |> list.find_map(fn(x) {
//         let #(param_field, os) = x

//         list.find_map(os, fn(o) {
//           case { o.ident == "" || o.ident == param_type.name } && o.field == return_field { // TODO RF-types better types, not `""`
//             False -> Error(Nil)
//             True -> Ok(#(param_field, o))
//           }
//         })
//       })
//     }

//     let override =
//       specfic_override
//       |> result.lazy_or(general_override)

//     let conv =
//       case override {
//         Ok(#(_field, FromFieldOverride(using: conv, ..))) -> conv
//         _ -> None
//       }

//     let #(param_field, _p_type) =
//       case override {
//         Error(_) ->
//           #(return_field, Nil)

//         Ok(#(_param_field, o)) if o.ident == "" || o.override == "" -> // TODO RF-types
//           #(return_field, Nil)

//         Ok(#(_param_field, o)) ->
//           #(o.override, Nil)
//       }

//     // // TODO check types match (commented out due to breaking change from impl `using`)
//     // // let param_fields = fields(param_variant)
//     // // let param_field_type =
//     // //   list.find_map(param_fields, fn(x) {
//     // //     let #(name, type_) = x
//     // //     case param_field == name {
//     // //       False -> Error(Nil)
//     // //       True -> Ok(type_)
//     // //     }
//     // //   })

//     // // case override, param_field_type {
//     // //   Ok(#(_field, FromFieldOverride(using: Some(_conv_func), ..))), _ -> {
//     // //     Nil
//     // //   }

//     // //   Error(Nil), Ok(param_field_type) if param_field_type == result_field_type ->
//     // //     Nil

//     // //   Error(Nil), Error(_) -> {
//     // //     common.debug(param_type)
//     // //     common.debug(param_variant)
//     // //     common.debug(param_field)
//     // //     panic as { "`from` param field doesn't exist: " <>
//     // //       param_type.name <> " " <>
//     // //       param_variant.name <> "." <> param_field
//     // //     }
//     // //   }

//     // //   _, _ -> {
//     // //     io.println("PARAM TYPE")
//     // //     common.debug(param_type)
//     // //     common.debug(param_variant)
//     // //     common.debug(param_field)
//     // //     io.println("RETURN TYPE")
//     // //     common.debug(return_type)
//     // //     common.debug(return_variant)
//     // //     common.debug(return_field)
//     // //     panic as {
//     // //       [
//     // //         "`from` param & return field types don't match",
//     // //         "PARAM",
//     // //         param_type |> string.inspect,
//     // //         param_variant |> string.inspect,
//     // //         param_field |> string.inspect,
//     // //         "RETURN",
//     // //         return_type |> string.inspect,
//     // //         return_variant |> string.inspect,
//     // //         return_field |> string.inspect,
//     // //       ]
//     // //       |> string.join("\n")
//     // //       |> fn(str) {
//     // //         io.println_error(str)
//     // //         str
//     // //       }
//     // //     }
//     // //   }
//     // // }

//     Field(
//       // param_field_override:,
//       param_field:,
//       return_field:,
//       conv:,
//     )
//   })
// }

fn fields(
  type_ type_: CustomType,
  variant variant: Variant,
) -> List(#(DerivField, g.Type)) {
  variant.fields
  |> list.map(fn(field) {
    case field {
      LabelledVariantField(item: NamedType(..) as t, label: field) ->
        #(DerivField(type_: type_.name, variant: variant.name, field:), t)

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
      case field.conv {
        None ->
          LabelledField(field.return_field, FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "value"), field.param_field))

        Some(conv) -> {
          let value = Variable(common.dummy_location(), "value")

          let value =
            case conv.args {
              EntireValue -> value
              ValueDotField -> FieldAccess(common.dummy_location(), value, field.param_field)
            }

          LabelledField(
            label: field.return_field,
            item: glance.BinaryOperator(common.dummy_location(),
              name: glance.Pipe,
              left: value,
              right: conv_func(conv),
            ),
          )
        }
      }
    })))])
  )
}

fn conv_func(
  conv conv: FromFieldOverrideConv,
) {
  case conv {
    FromFieldOverrideConv(module: Some(module), func:, ..) ->
      FieldAccess(common.dummy_location(),
        Variable(common.dummy_location(), module),
        func,
      )

    FromFieldOverrideConv(module: None, func:, ..) ->
      Variable(common.dummy_location(), func)
  }
}
