import gleam/option.{type Option, Some, None}
import deriv/types
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import glance.{type Expression, type CustomType, type Definition, type Function, type Variant, type Span, type VariantField, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, Span, List, UnlabelledField, String, Int, Float}
import deriv/types.{type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type ModuleReader} as deriv
import deriv/common

fn parse_form_fields(
  type_ type_: CustomType,
  opts opts: FormFieldOpts,
) -> List(FormField) {
  case type_.variants {
    [variant]-> {
      variant.fields
      |> list.map(fn(field) {
        case field {
          glance.LabelledVariantField(label: name, item: type_) -> {
            parse_form_field(name:, prefix: [], type_:, opts:)
          }

          glance.UnlabelledVariantField(..) -> {
            panic as { "`derive form` only works for variants with labelled fields, but got: " <> string.inspect(type_) }
          }
        }
      })
    }

    _ -> {
      panic as { "`derive form` only works for types with a single variant, but got: " <> string.inspect(type_) }
    }
  }
}

fn parse_form_field(
  name name: String,
  prefix prefix: List(String),
  type_ type_: glance.Type,
  opts opts: FormFieldOpts,
) -> FormField {
  let opt =
    opts
    |> dict.get(name)
    |> result.unwrap(zero_form_field_opt())

  case type_ {
    glance.NamedType(..) -> {
      FormField(
        name:,
        prefix:,
        parser: type_ |> to_parser(opt:),
        checks: [],
      )
    }

    glance.TupleType(..) |
    glance.FunctionType(..) |
    glance.VariableType(..) |
    glance.HoleType(..) -> {
      panic as { "`derive form` doesn't know what to do with non-`NamedType`: " <> string.inspect(type_) }
    }
  }
}

// fn get_field_opt(
//   opts opts: FormFieldOpts,
//   name name: List(String),
// ) -> Result(FormFieldOpt, Nil) {
// }

fn build_form_field_opts(
  from opts: DerivFieldOpts,
  type_ type_: deriv.Type,
) -> FormFieldOpts {
  case type_ {
    deriv.TypeAlias(..) -> {
      panic as { "`derive form` doesn't know how to handle type aliases, namely: " <> string.inspect(type_) }
    }

    deriv.Type(type_: glance.CustomType(variants: [glance.Variant(name: variant, ..)], ..) as type_) -> {
      opts
      |> dict.to_list()
      |> list.filter_map(fn(t) {
        let #(key, opts) = t

        case key.type_ == type_.name && key.variant == variant {
          True -> {
            Ok(#(key.field, build_form_field_opt(from: opts, name: key.field, constr: variant)))
          }

          False -> {
            Error(Nil)
          }
        }
      })
      |> dict.from_list
    }

    deriv.Type(type_: glance.CustomType(variants: [], ..)) -> {
      panic as { "`derive form` doesn't know how to handle types without any variants, namely: " <> string.inspect(type_) }
    }

    deriv.Type(..) -> {
      panic as { "`derive form` doesn't know how to handle multi-variant types, namely: " <> string.inspect(type_) }
    }
  }
}

fn build_form_field_opt(
  from opts: List(types.DerivFieldOpt),
  constr constr: String,
  name name: String,
) -> FormFieldOpt {
  opts
  |> list.fold(zero_form_field_opt(), fn(acc, opt) {
    let types.DerivFieldOpt(strs: tokens) = opt

    case tokens, acc {
      // FORM PARSER
      ["form", "parser", _func_name], FormFieldOpt(parser_override: Some(..), ..) -> {
        panic as { "`"<> constr <>  "." <> name <> "` already specifies a `parser` override" }
      }
      ["form", "parser", func_name], FormFieldOpt(parser_override: None, ..) -> {
        FormFieldOpt(..acc, parser_override: Some(ParserOverride(func_name:)))
      }
      // FORM PARSER INNER
      ["form", "parser", "inner", _func_name], FormFieldOpt(parser_override: Some(..), ..) -> {
        panic as { "`"<> constr <>  "." <> name <> "` already specifies a `parser` override" }
      }
      ["form", "parser", "inner", func_name], FormFieldOpt(parser_override: None, ..) -> {
        FormFieldOpt(..acc, parser_override: Some(ParserOverrideInner(func_name:)))
      }
      //
      _, _ -> {
        acc
      }
    }

    acc
  })
}

fn to_parser(
  type_ type_: glance.Type,
  opt opt: FormFieldOpt,
) -> Parser {
  case opt.parser_override, type_ {
    // OVERRIDES

    Some(ParserOverrideInner(func_name:)), glance.NamedType(name: "Option", parameters: [_param_type], ..) -> {
      OptionParser(CustomParser(func_name:, type_:))
    }

    Some(ParserOverrideInner(func_name:)), glance.NamedType(name: "List", parameters: [_param_type], ..) -> {
      ListParser(CustomParser(func_name:, type_:))
    }

    Some(ParserOverrideInner(..)), glance.NamedType(..) -> {
      panic as { "the `parser inner` override only makes sense in the context of `List(t)` or `Option(t)`, but got: " <> string.inspect(type_) }
    }

    Some(ParserOverride(func_name:)), glance.NamedType(..) -> {
      CustomParser(func_name:, type_:)
    }

    // STANDARD PARSERS

    None, glance.NamedType(name: "String", parameters: [], ..) -> {
      StringParser
    }

    None, glance.NamedType(name: "Int", parameters: [], ..) -> {
      IntParser
    }

    None, glance.NamedType(name: "Float", parameters: [], ..) -> {
      FloatParser
    }

    None, glance.NamedType(name: "Bool", parameters: [], ..) -> {
      BoolParser
    }

    None, glance.NamedType(name: "Option", parameters: [param_type], ..) -> {
      param_type
      |> to_parser(opt:)
      |> OptionParser
    }

    None, glance.NamedType(name: "List", parameters: [param_type], ..) -> {
      param_type
      |> to_parser(opt:)
      |> ListParser
    }

    None, glance.NamedType(name:, ..) -> {
      name
      |> common.snake_case
      |> CustomParser(func_name: _, type_:)
    }

    // PANIC FOR ANYTHING OTHER THAN `NamedType`

    Some(ParserOverride(..)), _ |
    Some(ParserOverrideInner(..)), _ |
    None, glance.TupleType(..) |
    None, glance.FunctionType(..) |
    None, glance.VariableType(..) |
    None, glance.HoleType(..) -> {
      panic as { "`form.to_parser` doesn't know what to do with non-`NamedType`: " <> string.inspect(type_) }
    }
  }
}

type Parser {
  StringParser
  IntParser
  FloatParser
  BoolParser
  OptionParser(Parser)
  ListParser(Parser)
  CustomParser(
    func_name: String,
    type_: glance.Type,
  )
}

type Check {
  Check
}

type FormField {
  FormField(
    name: String,
    prefix: List(String),
    // //
    // gleam_var: String,
    // input_name: String,
    //
    parser: Parser,
    checks: List(Check),
  )
}

type NestedForm {
  NestedForm(
    key: String,
    constr: String,
  )
}

type Form {
  Form(
    nested: List(NestedForm),
    fields: List(FormField),
  )
}

type UseVals {
  UseVals(
    gleam_var: String,
    input_name: String,
  )
}

fn to_use_vals(
  field field: FormField,
) -> UseVals {
  let FormField(name:, prefix:, ..) = field

  UseVals(
    gleam_var: gleam_var(name:, prefix:),
    input_name: input_name(name:, prefix:)
  )
}

fn gleam_var(
  name name: String,
  prefix prefix: List(String),
) -> String {
  prefix
  |> list.append([name])
  |> string.join("_")
}

fn input_name(
  name name: String,
  prefix prefix: List(String),
) -> String {
  case prefix {
    [] -> {
      name
    }

    [first, ..rest] -> {
      let rest =
        rest
        |> list.append([name])
        |> list.map(fn(str) { "[" <> str <> "]" })
        |> string.join("")

      first <> rest
    }
  }
}

//
//
//

type FormFieldOpts = Dict(String, FormFieldOpt)

type FormFieldOpt {
  FormFieldOpt(
    nil: Nil,
    parser_override: Option(ParserOverride)
  )
}

type ParserOverride {
  ParserOverride(func_name: String)
  ParserOverrideInner(func_name: String)
}

fn zero_form_field_opt() -> FormFieldOpt {
  FormFieldOpt(
    nil: Nil,
    parser_override: None,
  )
}

pub fn gen(
  t: deriv.Type,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  _module_reader: ModuleReader,
) -> Gen {
  let opts = build_form_field_opts(from: field_opts, type_: t)

  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/form` "

    deriv.Type(type_:) -> {
      let imports = gen_imports(type_)

      let funcs =
        form_func(type_)
        |> list.wrap

      let src = ""
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file:, deriv:, imports:, funcs:, src:, meta: dict.new())
    }
  }
}

fn gen_imports(
  type_: CustomType,
) -> List(Import) {
  case common.are_any_fields_options(type_) {
    True -> [common.none_constr_import()]
    False -> []
  }
  |> list.append([
    common.util_import(),
  ])
}

fn form_func(
  type_: CustomType,
) -> Definition(Function) {
  todo
}
// fn form_func(
//   type_: CustomType,
// ) -> Definition(Function) {
//   type_.variants
//   |> list.fold_until(None, fn(acc, variant) {
//     case form_func_(variant, type_) {
//       Ok(func) -> list.Stop(Some(func))
//       Error(_) -> list.Continue(acc)
//     }
//   })
//   |> option.lazy_unwrap(fn() {
//     panic as { "`CustomType` has no variants!\n\n" <> string.inspect(type_)}
//   })
// }

// fn form_func_(
//   variant: Variant,
//   type_: CustomType
// ) -> Result(Definition(Function), Nil) {
//   use field_form_vals: List(glance.Field(Expression)) <- result.try(result.all(
//     variant.fields
//     |> list.map(form_call)
//     |> list.map(result.map(_, UnlabelledField))
//   ))

//   let constr_name = variant.name
//   let func_name = "form_" <> common.snake_case(type_.name)
//   let func_return_type_name = type_.name

//   let func_return_type = Some(NamedType(common.dummy_location(), func_return_type_name, None, []))

//   let body =
//     Call(
//       location: common.dummy_location(),
//       function: Variable(common.dummy_location(), constr_name),
//       arguments: field_form_vals,
//     )
//     |> Expression

//   let func =
//     Function(common.dummy_location(), func_name, Public, [], func_return_type, [body])

//   Ok(Definition([], func))
// }

// fn form_call(
//   field: VariantField
// ) -> Result(Expression, Nil) {
//   case field.item {
//     NamedType(name: "String", ..) -> Ok(form_string())
//     NamedType(name: "Bool", ..) -> Ok(form_bool())
//     NamedType(name: "Int", ..) -> Ok(form_int())
//     NamedType(name: "Float", ..) -> Ok(form_float())
//     NamedType(name: "Option", ..) -> Ok(form_option())
//     NamedType(name: "List", ..) -> Ok(form_list())
//     NamedType(name: "Time", ..) -> Ok(form_time())
//     NamedType(name: "Uuid", ..) -> Ok(form_uuid())
//     _ -> Error(Nil)
//   }
// }

// fn form_uuid() -> Expression {
//   Call(common.dummy_location(), FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "util"), "form_uuid"), [])
// }

// fn form_time() -> Expression {
//   Call(common.dummy_location(), FieldAccess(common.dummy_location(), Variable(common.dummy_location(), "util"), "form_time"), [])
// }

// fn form_string() -> Expression {
//   String(common.dummy_location(), "")
// }

// fn form_option() -> Expression {
//   Variable(common.dummy_location(), "None")
// }

// fn form_int() -> Expression {
//   Int(common.dummy_location(), "0")
// }

// fn form_float() -> Expression {
//   Float(common.dummy_location(), "0.0")
// }

// fn form_bool() -> Expression {
//   Variable(common.dummy_location(), "False")
// }

// fn form_list() -> Expression {
//   List(common.dummy_location(), [], None)
// }
