import gleam/result
import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, UnlabelledField, FunctionParameter, Named, Case, Variant, Clause, PatternString, PatternDiscard, PatternVariant, String}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader} as deriv
import deriv/common

fn to_string_func(
  type_ type_: CustomType,
) -> Definition(Function) {
  let x = common.dummy_location()

  let func_name =
    "enum_" <> { type_.name |> common.snake_case } <> "_str"

  Definition([], Function(x, func_name, Public,
    [FunctionParameter(None, Named("x"), Some(NamedType(x, type_.name, None, [])))],
    Some(NamedType(x, "String", None, [])),
    [
      Expression(Call(x, FieldAccess(x, Variable(x, "string"), "inspect"), [UnlabelledField(Variable(x, "x"))]))
    ]
  ))
}

fn parse_func(
  type_ type_: CustomType,
) -> Definition(Function) {
  let x = common.dummy_location()

  let func_name =
    "parse_enum_" <> { type_.name |> common.snake_case }

  Function(x, func_name, Public,
    [FunctionParameter(None, Named("str"), Some(NamedType(x, "String", None, [])))],
    Some(NamedType(x, "Result", None, [NamedType(x, type_.name, None, []), NamedType(x, "Nil", None, [])])),
    [
      Expression(Case(x, [Variable(x, "str")], {
        type_.variants
        |> list.map(fn(variant) {
          case variant {
            Variant(name:, fields: [], ..) -> {
              Clause(
                [[PatternString(x, name)]],
                None,
                Call(x, Variable(x, "Ok"), [UnlabelledField(Variable(x, name))]),
              )
            }

            Variant(fields: _, ..) -> {
              panic as { "`derive enum` doesn't support variants with fields, but got: " <> string.inspect(variant) }
            }
          }
        })
        |> list.append([
          Clause(
            [[PatternDiscard(x, "")]],
            None,
            Call(x, Variable(x, "Error"), [UnlabelledField(Variable(x, "Nil"))]),
          )
        ])
      })),
    ]
  )
  |> Definition(attributes: [], definition: _)
}

fn display_func(
  type_ type_: CustomType,
  opts opts: DerivFieldOpts,
) -> Result(Definition(Function), Nil) {
  let display_lookups =
    type_.variants
    |> list.filter_map(fn(variant) {
      case opts |> dict.get(deriv.DerivField(type_: type_.name, variant: variant.name, field: "")) {
        Ok(opts) -> {
          opts
          |> list.filter_map(fn(opt) {
            case opt.strs {
              ["enum", "display", ..display] -> {
                Ok(string.join(display, " "))
              }

              _ -> {
                Error(Nil)
              }
            }
          })
          |> fn(displays) {
            case displays {
              [] -> {
                Error(Nil)
              }

              [display] -> {
                Ok(#(variant.name, display))
              }

              _multiple -> {
                panic as { "`derive enum` found multiple `display` values for: " <> string.inspect(type_) }
              }
            }
          }
        }

        Error(Nil) -> {
          Error(Nil)
        }
      }
    })
    |> dict.from_list

  case display_lookups |> dict.size {
    0 -> {
      Error(Nil)
    }

    _ -> {
      Ok(display_func_(type_:, display_lookups:))
    }
  }
}

fn display_func_(
  type_ type_: CustomType,
  display_lookups display_lookups: Dict(String, String)
) -> Definition(Function) {
  let x = common.dummy_location()

  let func_name =
    "display_enum_" <> { type_.name |> common.snake_case }

  Function(x, func_name, Public,
    [FunctionParameter(None, Named("x"), Some(NamedType(x, type_.name, None, [])))],
    Some(NamedType(x, "String", None, [])),
    [Expression(
      Case(x,
        [Variable(x, "x")],
        list.map(type_.variants, fn(variant) {
          let backslash = "\\"
          let double_quote = "\""

          let display_str =
            dict.get(display_lookups, variant.name)
            |> result.unwrap(variant.name)
            |> string.replace(double_quote, backslash <> backslash <> backslash <> backslash <> double_quote)

          Clause([[PatternVariant(x, None, variant.name, [], False)]], None, String(x, display_str))
        }),
      )
    )]
  )
  |> Definition(attributes: [], definition: _)
}

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  opts: DerivFieldOpts,
  file: File,
  _module_reader: ModuleReader,
) -> Gen {
  let type_ = common.custom_type_or_panic(type_: t, deriv_name: "enum")

  let imports = gen_imports(type_)

  let funcs =
    [
      parse_func(type_:),
      to_string_func(type_:),
    ]
    |> list.append(optional_funcs(type_:, opts:))

  let src = ""
    funcs
    |> list.map(common.func_str)
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
}

fn optional_funcs(
  type_ type_: CustomType,
  opts opts: DerivFieldOpts,
) -> List(Definition(Function)) {
  display_func(type_:, opts:)
  |> result.map(list.wrap)
  |> result.unwrap([])
}

fn gen_imports(
  _type_: CustomType,
) -> List(Import) {
  [
    common.import_("gleam/string"),
  ]
}
