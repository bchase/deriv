import gleam/option.{Some, None}
import gleam/dict
import gleam/list
import gleam/string
import glance.{type CustomType, type Definition, type Function, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, UnlabelledField, FunctionParameter, Named, Case, Variant, Clause, PatternString, PatternDiscard}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader} as deriv
import deriv/common

fn to_string_func(
  type_ type_: CustomType,
) -> Definition(Function) {
  let x = common.dummy_location()

  let func_name =
    "enum_" <> { type_.name |> common.snake_case } <> "_str"

  Definition([], Function(x, func_name, Public,
    [FunctionParameter(None, Named("x"), Some(NamedType(x, "T", None, [])))],
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
    Some(NamedType(x, "Result", None, [NamedType(x, "T", None, []), NamedType(x, "Nil", None, [])])),
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

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  _field_opts: DerivFieldOpts,
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

  let src = ""
    funcs
    |> list.map(common.func_str)
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
}

fn gen_imports(
  _type_: CustomType,
) -> List(Import) {
  [
    common.import_("gleam/string"),
  ]
}
