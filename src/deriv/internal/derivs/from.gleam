import gleam/dict
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/result
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Variant, LabelledVariantField, Definition, Function, Public, FunctionParameter, Named, NamedType, Expression, Call, Variable, LabelledField, FieldAccess} as _
import glance as g
import deriv/internal/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader, type DerivFieldOpt, type DerivField, DerivField} as deriv
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
      let ident =
        case deriv.opts {
          [ident] -> {
            ident
          }
          _ -> panic as "`from` requires specifying a single type in the form `m1/m2.T"
        }

      let imports = []

      let funcs =
        from(
          type_,
          ident,
          module_reader,
          opts,
        )
        |> from_func
        |> list.wrap

      let src =
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file:, deriv:, imports:, funcs:, types: [], src:, meta: dict.new())
    }
  }
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

fn get_param_types_and_variants(
  ident: String,
  module_reader: ModuleReader,
) -> #(Definition(CustomType), Variant, String) {
  let #(module_name, type_) =
    case common.fetch_custom_type(ident, module_reader) {
      Error(err) -> {
        common.debug(ident)
        common.debug(err)
        panic as "`from` issue with the above `ident`"
      }

      Ok(x) ->
        x
    }

  case type_.definition.variants {
    [variant] ->
      #(type_, variant, module_name)

    _, -> {
      common.debug(type_)
      panic as "`from` derivation currently only supports invariant types"
    }
  }
}

fn from(
  return_type: CustomType,
  ident: String,
  module_reader: ModuleReader,
  opts: DerivFieldOpts,
) -> FromFunc {
  case return_type.variants {
    [return_variant] -> {
      let #(param_type_def, param_variant, param_type_module) =
        get_param_types_and_variants(ident, module_reader)

      let param_type = param_type_def.definition

      from_variant_(
        param_type_module,
        param_type,
        param_variant,
        return_type,
        return_variant,
        ident,
        opts,
      )
    }

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
  ident: String,
  opts: DerivFieldOpts,
) -> FromFunc {
  let ident =
    ident
    |> parse_ident
    |> result.lazy_unwrap(fn() {
      panic as { "`from` couldn't make sense of this type identifier: " <> ident }
    })

  let fields =
    return_variant
    |> fields(type_: return_type, variant: _)
    |> list.map(fn(t) {
      let #(field, _type) = t
      field
    })
    |> list.map(from_func_field(field: _, ident:, opts:))

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

type Field {
  Field(
    param_field: String,
    return_field: String,
    conv: Option(Conv),
  )
}

pub type ConvArgs {
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
    args: ConvArgs,
  )
}

type Override {
  SpecifyField(ident: Ident, field: String)
  ConvAllWith(conv: Conv)
  ConvTypeWith(ident: Ident, conv: Conv)
}


fn generalize_field_ident(
  ident ident: Ident,
) -> #(String, String) {
  case ident {
    IdentType(module:, type_:) -> #(module |> option.unwrap(""), type_)
    IdentFieldForType(module:, type_:, ..) -> #(module |> option.unwrap(""), type_)
  }
}

fn match_specific(
  field f: DerivField,
  ident ident: Ident,
  overrides overrides: List(Override),
) -> Result(Override, Nil) {
  let module_type = #(ident.module |> option.unwrap(""), ident.type_)

  overrides
  |> list.filter(fn(override) {
    case override {
      SpecifyField(ident:, ..) |
      ConvTypeWith(ident:, ..) -> {
        generalize_field_ident(ident) == module_type
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
  ident ident: Ident,
  overrides overrides: List(Override),
) -> Result(Override, Nil) {
  let overrides_by_unqualified_type =
    overrides
    |> list.filter(fn(override)  {
      case override {
        ConvTypeWith(ident: IdentFieldForType(type_: "", module: None, ..), ..) |
        ConvTypeWith(ident: IdentFieldForType(type_: "*", module: None, ..), ..) |
        SpecifyField(ident: IdentFieldForType(type_: "", module: None, ..), ..) |
        SpecifyField(ident: IdentFieldForType(type_: "*", module: None, ..), ..) -> {
          True
        }

        ConvTypeWith(ident: IdentFieldForType(type_:, module: None, ..), ..) |
        SpecifyField(ident: IdentFieldForType(type_:, module: None, ..), ..) |
        ConvTypeWith(ident: IdentType(type_:, module: None), ..) -> {
          type_ == ident.type_
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

  let overrides_for_all = fn() {
    overrides
    |> list.find(fn(override) {
      case override {
        ConvAllWith(..) -> True
        _ -> False
      }
    })
    // |> fn(os) {
    //   case os {
    //     [o, _, ..] -> {
    //       [
    //         "`from` found multiple options generally matching the following field:\n",
    //         f |> string.inspect,
    //         .. os |> list.map(string.inspect),
    //       ]
    //       |> string.join("")
    //       |> io.println_error

    //       Ok(o)
    //     }
    //     [o] -> Ok(o)
    //     [] -> Error(Nil)
    //   }
    // }
  }

  overrides_by_unqualified_type
  |> result.lazy_or(overrides_for_all)
}

fn build_field_override(
  field_opt: DerivFieldOpt,
) -> Result(Override, Nil) {
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

  case field_opt.strs {
    ["from", ..rest] -> {
      case rest  {
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
          Ok(ConvAllWith(conv: { conv |> parse_conv_or_panic }(ValueDotField)))
        }

        [ident, "using", conv] -> {
          result.try(parse_ident(ident:), fn(ident) {
            let build_conv = conv |> parse_conv_or_panic
            // TODO clean up `*` handling
            let args =
              case ident.type_ == "*", ident.type_ |> string.contains("*") {
                True, _ -> ValueDotField
                False, True -> EntireValue
                False, False -> ValueDotField
               }
            let conv = build_conv(args)
            let type_ = ident.type_ |> string.replace("*", "")
            let ident =
              case ident {
                IdentFieldForType(..) -> IdentFieldForType(..ident, type_:)
                IdentType(..) -> IdentType(..ident, type_:)
              }
            Ok(ConvTypeWith(ident:, conv:))
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
) -> fn(ConvArgs) -> Conv {
  case str |> string.split(".") {
    [""] -> panic as { "`from` must specify a convert function for `using`" }
    [func] -> Conv(module: None, func:, args: _)
    [module, func] -> Conv(module: Some(module), func:, args: _)
    _ -> panic as {
      "`from` field convert function specified by `using` is invalid. Valid syntax is `func_name` or `module.func_name`, but got: " <> str
    }
  }
}


fn from_func_field(
  field field: DerivField,
  ident ident: Ident,
  opts opts: DerivFieldOpts,
) -> Field {
  let overrides =
    opts
    |> dict.get(field)
    |> result.unwrap([])
    |> list.map(build_field_override)
    |> result.values

  let override =
    match_specific(field:, ident:, overrides:)
    |> result.lazy_or(fn() {
      match_general(field:, ident:, overrides:)
    })

  let #(param_field, conv) =
    case override {
      Error(Nil) ->
        #(field.field, None)

      Ok(SpecifyField(field:, ..)) ->
        #(field, None)

      Ok(ConvAllWith(conv:)) ->
        #(field.field, Some(conv))

      Ok(ConvTypeWith(ident:, conv:)) ->
        case ident {
          IdentFieldForType(field:, ..) ->
            #(field, Some(conv))
          IdentType(..) ->
            #(field.field, Some(conv))
        }
    }

  Field(
    param_field:,
    return_field: field.field,
    conv:,
  )
}

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

  Definition([], Function(x, func_name, Public,
    [FunctionParameter(None, Named("value"), Some(NamedType(x, param_type, None, [])))],
    Some(NamedType(x, return_type, None, [])),
    [Expression(Call(x, Variable(x, return_contr), list.map(fields, fn(field) {
      case field.conv {
        None ->
          LabelledField(field.return_field, FieldAccess(x, Variable(x, "value"), field.param_field))

        Some(conv) -> {
          let value = Variable(x, "value")

          let value =
            case conv.args {
              EntireValue -> value
              ValueDotField -> FieldAccess(x, value, field.param_field)
            }

          let conv_func =
            case conv {
              Conv(module: Some(module), func:, ..) ->
                FieldAccess(x,
                  Variable(x, module),
                  func,
                )

              Conv(module: None, func:, ..) ->
                Variable(x, func)
            }

          LabelledField(
            label: field.return_field,
            item: g.BinaryOperator(x,
              name: g.Pipe,
              left: value,
              right: conv_func,
            ),
          )
        }
      }
    })))])
  )
}
