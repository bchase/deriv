import gleam/bool
import gleam/result
import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, UnlabelledField, FunctionParameter, Named, Case, Variant, Clause, PatternString, PatternDiscard, PatternVariant, String}
import deriv/internal/types.{type Context, type Gen, Gen, type DerivFieldOpts} as deriv
import deriv/internal/common

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
  ctx ctx: Context,
) -> Definition(Function) {
  let x = common.dummy_location()

  let func_name =
    "parse_enum_" <> { type_.name |> common.snake_case }

  let fail =
    ctx.opts
    |> dict.to_list
    |> list.filter_map(fn(t) {
      let #(field, opts) = t

      let has_enum_fail =
        opts
        |> list.any(fn(opt) {
          opt.strs == ["enum", "fail"] &&
            !string.is_empty(field.variant)
        })

      use <- bool.guard(!has_enum_fail, Error(Nil))
      Ok(field)
    })

  let fail =
    case fail {
      [] -> Error(Nil)
      [field] -> Ok(field)
      _ -> panic as {
        "multiple fields specified as `enum fail` for type:\n" <>
          string.inspect(type_) <> "\n" <>
          string.inspect(fail)
      }
    }

  let return_type =
    case fail {
      Error(Nil) ->
        NamedType(x, "Result", None, [NamedType(x, type_.name, None, []), NamedType(x, "Nil", None, [])])

      Ok(_) ->
        NamedType(x, type_.name, None, [])
    }

  Function(x, func_name, Public,
    [FunctionParameter(None, Named("str"), Some(NamedType(x, "String", None, [])))],
    Some(return_type),
    [
      Expression(Case(x, [Variable(x, "str")], {
        type_.variants
        |> list.map(fn(variant) {
          case variant {
            Variant(name:, fields: [], ..) -> {
              Clause(
                [[PatternString(x, name)]],
                None,
                {
                  case fail {
                    Ok(_) ->
                      Variable(x, name)

                    Error(Nil) ->
                      Call(x, Variable(x, "Ok"), [UnlabelledField(Variable(x, name))])
                  }
                },
              )
            }
            |> Ok

            Variant(fields: _, ..) ->
              Error(Nil)
          }
        })
        |> result.values
        |> list.append([
          Clause(
            [[PatternDiscard(x, "")]],
            None,
            {
              case fail {
                Ok(field) ->
                  Call(x, Variable(x, field.variant), [UnlabelledField(Variable(x, "str"))])

                Error(Nil) ->
                  Call(x, Variable(x, "Error"), [UnlabelledField(Variable(x, "Nil"))])
              }
            },
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
  ctx: Context,
) -> Gen {
  let type_ = common.custom_type_or_panic(type_: t, deriv_name: "enum")

  let imports = gen_imports(type_)

  let funcs =
    [
      parse_func(type_:, ctx:),
      to_string_func(type_:),
    ]
    |> list.append(optional_funcs(type_:, opts: ctx.opts))

  let src = ""
    funcs
    |> list.map(common.func_str)
    |> string.join("\n\n")

  Gen(file: ctx.file, deriv: ctx.deriv, imports:, funcs:, types: [], src:, meta: dict.new())
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
