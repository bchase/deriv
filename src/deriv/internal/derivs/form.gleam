import deriv/util
import gleam/int
import gleam/float
import gleam/option.{type Option, Some, None}
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import glance.{type Expression, type CustomType, type Definition, type Function, type Variant, type Span, type VariantField, type Import, Definition, Function, Public, NamedType, Expression, Call, Variable, FieldAccess, Span, List, UnlabelledField, String, Int, Float, ShorthandField, LabelledField, Block, CustomType, Variant, Named, FunctionType, TupleType, FunctionParameter, VariableType, Let, PatternVariable, Assignment, Fn, FnParameter, Clause, Case, PatternDiscard, PatternString, PatternVariant, BinaryOperator, Pipe, FnCapture}
import deriv/internal/types.{type Context, type File, type Derivation, type Gen, Gen, type DerivFieldOpts, type DerivFieldOpt, type ModuleReader, DerivFieldOpt} as deriv
import deriv/internal/common
import birl

// IMPROVE
//   - support nested forms (see `formal/scratch.gleam`; would need to parse nested opts)
//   - support `Dict`?

fn field_gleam_token(
  field field: FormField,
) -> String {
  field.prefix
  |> list.append([field.name])
  |> string.join("_")
}

fn form_func(
  params ffp: FormFuncParams,
  fields fields: List(FormField),
) -> glance.Definition(glance.Function) {
  let x = common.dummy_location()

  let use_statements: List(glance.Statement) =
    fields
    |> list.map(use_statement_for(field: _))

  let constr_args: List(glance.Field(glance.Expression)) =
    fields
    |> list.map(field_gleam_token)
    |> list.map(fn(token) {
      ShorthandField(label: token)
    })

  let type_name = ffp.type_.name
  let constr_name = ffp.variant.name

  let func_name = "form_" <> common.snake_case(ffp.type_.name)

  let success_expr: glance.Statement =
    Expression(Call(x, FieldAccess(x, Variable(x, "form"), "success"), [
      UnlabelledField(Call(x, Variable(x, constr_name), constr_args))
    ]))

  let block_inner_exprs =
    use_statements
    |> list.append([success_expr])

  glance.Function(
    location: x,
    name: func_name,
    publicity: Public,
    parameters: [],
    return: Some(NamedType(x, "Form", Some("form"), [
      NamedType(x, type_name, None, []),
    ])),
    body: [
      Expression(Call(x,
        FieldAccess(x, Variable(x, "form"), "new"), [
          UnlabelledField(Block(x, block_inner_exprs)),
        ]
      ))
    ],
  )
  |> glance.Definition(attributes: [], definition: _)
}

fn use_statement_for(
  field field: FormField,
) -> glance.Statement {
  let token = field_gleam_token(field)

  let parser_expr = parser_expr(parser: field.parser)

  let x = common.dummy_location()

  let pipe = fn(left, right) {
    glance.BinaryOperator(x, glance.Pipe, left, right)
  }

  let block_inner_expr =
    field.checks
    |> list.map(check_expr)
    |> list.fold(parser_expr, fn(acc_expr, check_expr) {
      acc_expr
      |> pipe(check_expr)
    })

  glance.Use(x,
    [glance.UsePattern(glance.PatternVariable(x, token), None)],
    Call(x,
      FieldAccess(x, Variable(x, "form"), "field"), [
        UnlabelledField(String(x, token)),
        UnlabelledField(glance.Block(x, [
          Expression(block_inner_expr),
        ]))
      ]
    )
  )
}

fn check_expr(
  check check: Check,
) -> glance.Expression {
  let x = common.dummy_location()

  case check {
    CheckConfirms(token:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_confirms"), [UnlabelledField(Variable(x, token))])
    }

    CheckAccepted -> {
      FieldAccess(x, Variable(x, "form"), "check_accepted")
    }

    CheckFloatLessThan(float:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_float_less_than"), [UnlabelledField(Float(x, float.to_string(float)))])
    }

    CheckFloatMoreThan(float:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_float_more_than"), [UnlabelledField(Float(x, float.to_string(float)))])
    }

    CheckIntLessThan(int:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_int_less_than"), [UnlabelledField(Int(x, int.to_string(int)))])
    }

    CheckIntMoreThan(int:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_int_more_than"), [UnlabelledField(Int(x, int.to_string(int)))])
    }

    CheckNotEmpty -> {
      FieldAccess(x, Variable(x, "form"), "check_not_empty")
    }

    CheckStringLengthLessThan(int:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_string_length_less_than"), [UnlabelledField(Int(x, int.to_string(int)))])
    }

    CheckStringLengthMoreThan(int:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check_string_length_more_than"), [UnlabelledField(Int(x, int.to_string(int)))])
    }

    CustomCheck(func_name:) -> {
      Call(x, FieldAccess(x, Variable(x, "form"), "check"), [UnlabelledField(Variable(x, func_name))])
    }
  }
}

fn parser_expr(
  parser parser: Parser,
) -> glance.Expression {
  let x = common.dummy_location()

  case parser {
    StringParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_string")
    }

    IntParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_int")
    }

    FloatParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_float")
    }

    BoolParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_checkbox")
    }

    OptionParser(inner_parser) -> {
      let inner = parser_expr(inner_parser)

      glance.BinaryOperator(x, glance.Pipe, inner, FieldAccess(x, Variable(x, "form"), "parse_optional"))
    }

    ListParser(inner_parser) -> {
      let inner = parser_expr(inner_parser)

      glance.BinaryOperator(x, glance.Pipe, inner, FieldAccess(x, Variable(x, "form"), "parse_list"))
    }

    EmailParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_email")
    }

    PhoneNumberParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_phone_number")
    }

    ColourParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_colour")
    }

    UriParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_url")
    }

    FormalDateParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_date")
    }

    FormalDateTimeParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_date_time")
    }

    FormalTimeParser -> {
      FieldAccess(x, Variable(x, "form"), "parse_time")
    }

    BirlTimeParser(parser: BirlParseISO8601) -> {
      Call(x, FieldAccess(x, Variable(x, "util"), "birl_time_iso8601_parser"), [])
    }

    CustomParser(func_name:, type_: _) -> {
      Call(x, Variable(x, func_name), [])
    }
  }
}

fn build_form_fields(
  type_ type_: CustomType,
  opts opts: FormFieldOpts,
  module module: String,
  read_module read_module: ModuleReader,
) -> List(FormField) {
  case type_.variants {
    [variant]-> {
      variant.fields
      |> list.flat_map(fn(field) {
        case field {
          glance.LabelledVariantField(label: name, item: type_) -> {
            build_form_fields_(name:, prefix: [], type_:, opts:, module:, read_module:)
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

fn build_form_fields_(
  name name: String,
  prefix prefix: List(String),
  type_ type_: glance.Type,
  opts opts: FormFieldOpts,
  module module: String,
  read_module read_module: ModuleReader,
) -> List(FormField) {
  let opt =
    opts
    |> dict.get(name)
    |> result.unwrap(zero_form_field_opt())

  case type_ {
    glance.NamedType(..) -> {
      FormField(
        name:,
        prefix:,
        parser: type_ |> to_parser(opt:, module:, read_module:),
        checks: opt.checks,
        type_:,
      )
      |> list.wrap
    }

    glance.TupleType(elements: [
      glance.NamedType(name: "Date", parameters: [], ..),
      glance.NamedType(name: "TimeOfDay", parameters: [], ..),
    ], ..) -> {
      FormField(
        name:,
        prefix:,
        parser: FormalDateTimeParser,
        checks: opt.checks,
        type_:,
      )
      |> list.wrap
    }

    glance.TupleType(..) |
    glance.FunctionType(..) |
    glance.VariableType(..) |
    glance.HoleType(..) -> {
      panic as { "`derive form` doesn't know what to do with non-`NamedType`: " <> string.inspect(type_) }
    }
  }
}

type FormFuncParams {
  FormFuncParams(
    type_: CustomType,
    variant: Variant,
  )
}

fn build_form_func_params(
  type_ type_: deriv.Type,
) -> FormFuncParams {
  case type_ {
    deriv.TypeAlias(..) -> {
      panic as { "`derive form` doesn't know how to handle type aliases, namely: " <> string.inspect(type_) }
    }

    deriv.Type(type_: glance.CustomType(variants: [variant], ..) as type_) -> {
      FormFuncParams(type_:, variant:)
    }

    deriv.Type(type_: glance.CustomType(variants: [], ..)) -> {
      panic as { "`derive form` doesn't know how to handle types without any variants, namely: " <> string.inspect(type_) }
    }

    deriv.Type(..) -> {
      panic as { "`derive form` doesn't know how to handle multi-variant types, namely: " <> string.inspect(type_) }
    }
  }
}

fn build_form_field_opts(
  from opts: DerivFieldOpts,
  for ffp: FormFuncParams,
) -> FormFieldOpts {
  opts
  |> dict.to_list()
  |> list.filter_map(fn(t) {
    let #(key, opts) = t

    case key.type_ == ffp.type_.name && key.variant == ffp.variant.name {
      True -> {
        Ok(#(key.field, build_form_field_opt(from: opts, name: key.field, constr: ffp.variant.name)))
      }

      False -> {
        Error(Nil)
      }
    }
  })
  |> dict.from_list
}

fn build_form_field_opt(
  from opts: List(DerivFieldOpt),
  constr constr: String,
  name name: String,
) -> FormFieldOpt {
  opts
  |> list.fold(zero_form_field_opt(), fn(acc, opt) {
    let tokens = opt.strs

    case tokens, acc {
      // FORM PARSER
      ["form", "parse_" <> _], FormFieldOpt(parser_override: Some(..), ..) -> {
        panic as { "`"<> constr <>  "." <> name <> "` already specifies a `parser` override" }
      }
      ["form", "parse_" <> _ as formal_func_name], FormFieldOpt(parser_override: None, ..) -> {
        case formal_func_name {
          "parse_email" |
          "parse_phone_number" |
          "parse_colour" |
          "parse_url" |
          "parse_date" |
          "parse_time" |
          "parse_date_time" -> {
            FormFieldOpt(..acc, parser_override: Some(ParserOverrideFormal(func_name: formal_func_name)))
          }
          _ -> {
            panic as { "Invalid `formal/form` parse func: " <> formal_func_name }
          }
        }
      }
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
      // FORM CHECKS
      ["form", "check_confirms", token], _ -> {
        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckConfirms(token:)]))
      }
      ["form", "check_accepted"], _ -> {
        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckAccepted]))
      }
      ["form", "check_float_less_than", str], _ -> {
        let float =
          str
          |> float.parse
          |> result.lazy_unwrap(fn() {
            panic_parsing(str:, opt_name: "form check_float_less_than", expected: "Float")
          })

        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckFloatLessThan(float:)]))
      }
      ["form", "check_float_more_than", str], _ -> {
        let float =
          str
          |> float.parse
          |> result.lazy_unwrap(fn() {
            panic_parsing(str:, opt_name: "form check_float_more_than", expected: "Float")
          })

        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckFloatMoreThan(float:)]))
      }
      ["form", "check_int_less_than", str], _ -> {
        let int =
          str
          |> int.parse
          |> result.lazy_unwrap(fn() {
            panic_parsing(str:, opt_name: "form check_int_less_than", expected: "Int")
          })

        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckIntLessThan(int:)]))
      }
      ["form", "check_int_more_than", str], _ -> {
        let int =
          str
          |> int.parse
          |> result.lazy_unwrap(fn() {
            panic_parsing(str:, opt_name: "form check_int_more_than", expected: "Int")
          })

        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckIntMoreThan(int:)]))
      }
      ["form", "check_not_empty"], _ -> {
        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckNotEmpty]))
      }
      ["form", "check_string_length_less_than", str], _ -> {
        let int =
          str
          |> int.parse
          |> result.lazy_unwrap(fn() {
            panic_parsing(str:, opt_name: "form check_string_length_less_than", expected: "Int")
          })

        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckStringLengthLessThan(int:)]))
      }
      ["form", "check_string_length_more_than", str], _ -> {
        let int =
          str
          |> int.parse
          |> result.lazy_unwrap(fn() {
            panic_parsing(str:, opt_name: "form check_string_length_more_than", expected: "Int")
          })

        FormFieldOpt(..acc, checks: acc.checks |> list.append([CheckStringLengthMoreThan(int:)]))
      }
      ["form", "check", func_name], _ -> {
        FormFieldOpt(..acc, checks: acc.checks |> list.append([CustomCheck(func_name:)]))
      }
      //
      _, _ -> {
        acc
      }
    }
  })
}

fn panic_parsing(
  str str: String,
  opt_name opt_name: String,
  expected type_: String,
) {
  panic as { "`deriv`'s `" <> opt_name <> "` expected a `" <> type_ <> "`, but got: `" <> string.inspect(str) <> "`" }
}

fn parser_for(
  formal_func_name func_name: String,
  type_name type_name: String,
  params params: List(glance.Type),
) -> Parser {
  case func_name, type_name, params {
    "parse_email", "String", [] ->
      EmailParser

    "parse_phone_number", "String", [] ->
      PhoneNumberParser

    "parse_colour", "String", [] ->
      ColourParser

    "parse_url", "Uri", [] ->
      UriParser

    "parse_date", "Date", [] ->
      FormalDateParser

    "parse_time", "TimeOfDay", [] ->
      FormalTimeParser

    "parse_date_time", _, _ -> {
      panic as { "`deriv` Usage of `formal/form.parse_date_time` not yet implemented" }
    }

    "parse_email", _, _ |
    "parse_phone_number", _, _ |
    "parse_colour", _, _ |
    "parse_url", _, _ |
    "parse_date", _, _ |
    "parse_time", _, _ -> {
      panic as { "`deriv` Type mismatch using `formal/form.`" <> func_name <> " with type: " <> type_name }
    }

    _, _, _ -> {
      panic as { "`deriv` Unknown `formal/form` func override: " <> func_name }
    }
  }
}

fn to_parser(
  type_ type_: glance.Type,
  opt opt: FormFieldOpt,
  module module: String,
  read_module read_module: ModuleReader,
) -> Parser {
  case opt.parser_override, type_ {
    // OVERRIDES

    Some(ParserOverrideFormal(func_name: formal_func_name)), glance.NamedType(name: type_name, parameters: params, ..) -> {
      parser_for(formal_func_name:, type_name:, params:)
    }

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
      |> to_parser(opt:, module:, read_module:,)
      |> OptionParser
    }

    None, glance.NamedType(name: "List", parameters: [param_type], ..) -> {
      param_type
      |> to_parser(opt:, module:, read_module:,)
      |> ListParser
    }

    None, glance.NamedType(name: "Uri", parameters: [], ..) -> {
      UriParser
    }

    None, glance.NamedType(name: "Date", parameters: [], ..) -> {
      FormalDateParser
    }

    None, glance.NamedType(name: "TimeOfDay", parameters: [], ..) -> {
      FormalTimeParser
    }

    None, glance.NamedType(name: "Time", parameters: [], ..) -> {
      BirlTimeParser(parser: BirlParseISO8601)
    }

    None, glance.NamedType(name:, ..) -> {
      let ident = module <> "." <> name

      case common.fetch_custom_type(ident:, read_module:) {
        Error(_) -> {
          panic as { "`derive form` failed to look up custom type: " <> string.inspect(type_) }
        }

        Ok(#(_module, glance.Definition(definition: glance.CustomType(variants: [], ..) as nested_type, ..))) -> {
          panic as { "`derive form` doesn't know how to handle types without any variants, namely: " <> string.inspect(nested_type) }
        }

        Ok(#(_module, glance.Definition(definition: glance.CustomType(variants: [_, _, ..], ..) as nested_type, ..))) -> {
          panic as { "`derive form` doesn't know how to handle multi-variant types, namely: " <> string.inspect(nested_type) }
        }

        Ok(#(_module, glance.Definition(definition: glance.CustomType(name: _name, variants: [_variant], ..) as nested_type, ..))) -> {
          panic as { "`derive form` will eventually treat non-standard types as nested forms, but this is not yet implemented. This was triggered by usage of: " <> string.inspect(nested_type) }
        }
      }
    }

    // DATE TIME PARSERS

    None, glance.TupleType(elements: [
      glance.NamedType(name: "Date", parameters: [], ..),
      glance.NamedType(name: "TimeOfDay", parameters: [], ..),
    ], ..) -> {
      FormalDateTimeParser
    }

    Some(override), glance.TupleType(elements: [
      glance.NamedType(name: "Date", parameters: [], ..),
      glance.NamedType(name: "TimeOfDay", parameters: [], ..),
    ], ..) -> {
      case override {
        ParserOverride(func_name:) -> {
          CustomParser(func_name:, type_:)
        }

        ParserOverrideFormal(func_name: "parse_date_time") -> {
          FormalDateTimeParser
        }

        ParserOverrideFormal(..) -> {
          panic as { "`derive form` `formal/form.parse_date_time` is the only parser that works for: " <> string.inspect(type_) }
        }

        ParserOverrideInner(..) -> {
          panic as { "`derive form`'s `parser inner` doesn't make sense for: " <> string.inspect(type_) }
        }
      }
    }

    // PANIC FOR ANYTHING OTHER THAN `NamedType`

    Some(ParserOverride(..)), _ |
    Some(ParserOverrideFormal(..)), _ |
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
  BoolParser // `parse_checkbox`
  //
  OptionParser(Parser)
  ListParser(Parser)
  //
  EmailParser // `parse_email` -> `String`
  PhoneNumberParser // `parse_phone_number` -> `String`
  ColourParser // `parse_colour` -> `String`
  //
  UriParser // `parse_url` -> `uri.Uri`
  //
  FormalDateParser // `parse_date` -> `calendar.Date`
  FormalDateTimeParser // `parse_date_time` -> `calendar.TimeOfDay`
  FormalTimeParser // `parse_time` -> `calendar.TimeOfDay`
  BirlTimeParser(parser: BirlTimeParser)
  //
  CustomParser(
    func_name: String,
    type_: glance.Type,
  )
}

type BirlTimeParser {
  BirlParseISO8601
}

type Check {
  // t
  CheckConfirms(token: String)
  // Bool
  CheckAccepted
  // Float
  CheckFloatLessThan(float: Float)
  CheckFloatMoreThan(float: Float)
  // Int
  CheckIntLessThan(int: Int)
  CheckIntMoreThan(int: Int)
  // String
  CheckNotEmpty
  CheckStringLengthLessThan(int: Int)
  CheckStringLengthMoreThan(int: Int)
  // custom
  CustomCheck(func_name: String)
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
    //
    type_: glance.Type,
  )
}
//
//
//

type FormFieldOpts = Dict(String, FormFieldOpt)

type FormFieldOpt {
  FormFieldOpt(
    parser_override: Option(ParserOverride),
    checks: List(Check),
  )
}

type ParserOverride {
  ParserOverride(func_name: String)
  ParserOverrideFormal(func_name: String)
  ParserOverrideInner(func_name: String)
}

fn zero_form_field_opt() -> FormFieldOpt {
  FormFieldOpt(
    parser_override: None,
    checks: [],
  )
}

// derive form                -- form parser
// derive form lookups        -- form parser, plus field type & lookups
// derive form lookups lustre -- form parser, plus field type & lookups, plus lustre html example func
pub fn gen(
  t: deriv.Type,
  ctx: Context,
) -> Gen {
  let ffp = build_form_func_params(type_: t)
  let opts = build_form_field_opts(from: ctx.opts, for: ffp)

  case t {
    deriv.TypeAlias(..) ->
      panic as "`deriv.TypeAlias` unimplemented for `deriv/form` "

    deriv.Type(type_:) -> {
      let module = ctx.file.module
      let imports = gen_imports(type_, ctx.deriv)
      let fields = build_form_fields(type_:, opts:, module:, read_module: ctx.module_reader)

      let funcs =
        form_func(
          params: ffp,
          fields:,
        )
        |> list.wrap

      let _checked_deriv_opts =
        case ctx.deriv.opts |> list.contains("lustre"), ctx.deriv.opts |> list.contains("lookups") {
          True, False -> {
            panic as "`derive form` requires the opt `lookups` if using the `lustre` opt"
          }

          _, _ -> {
            Nil
          }
        }

      let #(types, lookups_funcs) =
        case ctx.deriv.opts |> list.contains("lookups") {
          True -> {
            let types = [
              form_field_type(type_:, fields:),
            ]

            let funcs = [
              form_field_lookups_func(type_:, fields:)
            ]

            #(types, funcs)
          }

          False -> {
            #([], [])
          }
        }

      let lustre_example_funcs =
        case ctx.deriv.opts {
          ["lookups", "lustre"] | ["lustre", "lookups"] -> {
            [
              example_lustre_html_form_func(type_:, fields:),
            ]
          }

          _ -> {
            []
          }
        }

      let funcs =
        funcs
        |> list.append(lookups_funcs)
        |> list.append(lustre_example_funcs)

      let src = ""
        funcs
        |> list.map(common.func_str)
        |> string.join("\n\n")

      Gen(file: ctx.file, deriv: ctx.deriv, imports:, funcs:, types:, src:, meta: dict.new())
    }
  }
}

fn gen_imports(
  _type_: CustomType,
  deriv: Derivation,
) -> List(Import) {
  let lookups_imports =
    case deriv.opts {
      ["lookups", ..] -> lookups_imports()
      _ -> []
    }

  let example_imports =
    case deriv.opts {
      ["lookups", "lustre"] -> example_imports()
      _ -> []
    }

  base_imports()
  |> list.append(lookups_imports)
  |> list.append(example_imports)
}

fn base_imports() -> List(Import) {
  [
    common.import_("formal/form"),
  ]
}

fn lookups_imports() -> List(Import) {
  [
    common.import_("deriv/util"),
  ]
}

fn example_imports() -> List(Import) {
  [
    common.import__("deriv/lustre", as_: Some("f"), types: [], values: []),
    // common.import_("gleam/list"),
    // common.import__("gleam/option", as_: None, types: [], values: ["None"]),
    // import lustre/element
    // import lustre/element/html
    // import lustre/attribute
    // import lustre/event
  ]
}

// LUSTRE (FIELD TYPE, LOOKUPS, EXAMPLE USAGE FUNC)

fn field_variant_name(
  type_ type_: CustomType,
  field field: FormField,
) -> String {
  type_.name <> common.pascal_case(field.name)
}

fn form_field_type_name(
  type_ type_: CustomType,
) -> String {
  type_.name <> "Field"
}

fn form_field_type(
  type_ type_: CustomType,
  fields fields: List(FormField),
) -> Definition(CustomType) {
  let x = common.dummy_location()

  let field_type_name = form_field_type_name(type_:)

  let variants =
    fields
    |> list.map(fn(field) {
      Variant(field_variant_name(type_:, field:), [], [])
    })

  CustomType(x, field_type_name, Public, False, [], variants)
  |> Definition(attributes: [], definition: _)
}

fn example_lustre_html_form_func(
  type_ type_: CustomType,
  fields fields: List(FormField),
) -> Definition(Function) {
  let x = common.dummy_location()

  // `import deriv/lustre as f`
  let element = "f" // re-exporting `lustre/element`
  let html = "f"    // re-exporting `lustre/element/html`
  let attr = "f"    // re-exporting `lustre/attribute`
  let event = "f"   // re-exporting `lustre/event`

  let example_func_name =
    "example_lustre_html_form_for_" <> common.snake_case(type_.name)

  let form_type_name = type_.name

  let lookups_func_name = lookups_func_name(type_:)

  let inputs =
    fields
    |> list.map(fn(field) {
      let label = common.snake_case_to_label(field.name)
      let field_variant = field_variant_name(type_:, field:)

      Call(x, Variable(x, "input"), [
        UnlabelledField(Variable(x, field_variant)),
        UnlabelledField(String(x, label)),
      ])
    })

  Function(x, example_func_name, Public,
    [
      FunctionParameter(Some("submit_msg"), Named("submit_msg"), Some(FunctionType(x, [NamedType(x, "List", None, [TupleType(x, [NamedType(x, "String", None, []), NamedType(x, "String", None, [])])])], VariableType(x, "msg")))),
      FunctionParameter(Some("form"), Named("form"), Some(NamedType(x, "Form", Some("form"), [NamedType(x, form_type_name, None, [])]))),
    ],
    Some(NamedType(x, "Element", Some(element), [VariableType(x, "msg")])),
    [
      // STATIC `input` HELPER FUNC
      Assignment(x, Let, PatternVariable(x, "input"), None, Fn(x, [FnParameter(Named("field"), None), FnParameter(Named("label_str"), None)], None, [Assignment(x, Let, PatternVariable(x, "input"), None, Call(x, FieldAccess(x, Variable(x, html), "input"), [ShorthandField("field"), LabelledField("overrides", Call(x, FieldAccess(x, Variable(x, html), "placeholder"), [UnlabelledField(Variable(x, "label_str"))])), LabelledField("err", FieldAccess(x, Variable(x, "util"), "none")), LabelledField("lookup", Call(x, Variable(x, lookups_func_name), [])), ShorthandField("form")])), Assignment(x, Let, PatternVariable(x, "errs"), None, Call(x, FieldAccess(x, Variable(x, html), "ul"), [UnlabelledField(List(x, [], None)), UnlabelledField(Call(x, FieldAccess(x, Variable(x, "util"), "list_map"), [UnlabelledField(FieldAccess(x, FieldAccess(x, Variable(x, "input"), "field"), "errs")), UnlabelledField(Fn(x, [FnParameter(Named("err"), None)], None, [Expression(Call(x, FieldAccess(x, Variable(x, html), "li"), [UnlabelledField(List(x, [], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, html), "text"), [UnlabelledField(Variable(x, "err"))])], None))]))]))]))])), Expression(Call(x, FieldAccess(x, Variable(x, html), "div"), [UnlabelledField(List(x, [], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, html), "div"), [UnlabelledField(List(x, [], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, html), "label"), [UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, attr), "for"), [UnlabelledField(FieldAccess(x, FieldAccess(x, Variable(x, "input"), "field"), "id"))])], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, html), "text"), [UnlabelledField(Variable(x, "label_str"))])], None))])], None))]), Call(x, FieldAccess(x, Variable(x, html), "div"), [UnlabelledField(List(x, [], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, "input"), "render"), [UnlabelledField(Call(x, FieldAccess(x, Variable(x, html), "InputParams"), [LabelledField("class", FieldAccess(x, Variable(x, "util"), "none"))]))]), Variable(x, "errs")], None))])], None))]))])),

      // LUSTRE `html.form`
      Expression(
        Call(x, FieldAccess(x, Variable(x, html), "form"), [
          UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, event), "on_submit"), [UnlabelledField(Variable(x, "submit_msg"))])], None)
          ),

          {
            let submit =
              Call(x, FieldAccess(x, Variable(x, "f"), "p"), [UnlabelledField(List(x, [], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, "f"), "button"), [UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, "f"), "type_"), [UnlabelledField(String(x, "submit"))])], None)), UnlabelledField(List(x, [Call(x, FieldAccess(x, Variable(x, "f"), "text"), [UnlabelledField(String(x, "Submit"))])], None))])], None))])

            UnlabelledField(List(x, inputs |> list.append([submit]), None))
          },
        ])),
    ],
  )
  |> Definition(attributes: [], definition: _)
}


type LookupField {
  LookupField(
    variant: String,
    input_name: String,
    type_: util.GleamType,
  )
}

type TypeSimple {
  TypeSimple(
    name: String,
    params: List(TypeSimple),
  )
}
fn to_type_simple(
  type_ type_: glance.Type,
) -> TypeSimple {
  case type_ {
    glance.NamedType(name:, parameters: params, ..) -> {
      TypeSimple(name:, params: params |> list.map(to_type_simple))
    }

    glance.TupleType(..) |
    glance.FunctionType(..) |
    glance.VariableType(..) |
    glance.HoleType(..) -> {
      panic as { "`derive form` doesn't know what to do with non-`NamedType`: " <> string.inspect(type_) }
    }
  }
}

fn to_gleam_type(
  type_ type_: TypeSimple,
) -> util.GleamType {
  case type_ {
    TypeSimple(name: "String", params: []) -> {
      util.String
    }

    TypeSimple(name: "Int", params: []) -> {
      util.Int
    }

    TypeSimple(name: "Float", params: []) -> {
      util.Float
    }

    TypeSimple(name: "Bool", params: []) -> {
      util.Float
    }

    TypeSimple(name: "Uri", params: []) -> {
      util.Uri
    }

    TypeSimple(name: "Option", params: [inner]) -> {
      util.Option(inner |> to_gleam_type)
    }

    TypeSimple(name: "List", params: [inner]) -> {
      util.List(inner |> to_gleam_type)
    }

    TypeSimple(name: "Date", params: []) -> {
      util.Date
    }

    TypeSimple(name: "TimeOfDay", params: []) -> {
      util.TimeOfDay
    }

    _ -> {
      panic as { "`derive form` doesn't know how to handle this type: " <> string.inspect(type_) }
    }
  }
}

fn lookup_field(
  type_ type_: CustomType,
  field field: FormField,
) -> LookupField {
  LookupField(
    variant: field_variant_name(type_:, field:),
    input_name: common.snake_case(field.name),
    type_: field.type_ |> to_type_simple |>  to_gleam_type,
  )
}

fn lookups_func_name(
  type_ type_: CustomType,
) -> String {
  common.snake_case(type_.name) <> "_field_lookups"
}

fn form_field_lookups_func(
  type_ type_: CustomType,
  fields fields: List(FormField),
) -> Definition(Function) {
  let x = common.dummy_location()

  let gleam_type_module = "util"

  let form_type_name = type_.name
  let form_field_type_name = form_field_type_name(type_:)

  let lookups_func_name = lookups_func_name(type_:)

  let fields = fields |> list.map(lookup_field(field: _, type_:))

  let field_to_name = {
    Assignment(x, Let, PatternVariable(x, "field_to_name"), None, Fn(x, [FnParameter(Named("field"), None)], None, [
      Expression(Case(x, [Variable(x, "field")], list.map(fields, fn(field) {
        Clause([[PatternVariant(x, None, field.variant, [], False)]], None, String(x, field.input_name))
      }))),
    ]))
  }

  let name_to_field =
    Assignment(x, Let, PatternVariable(x, "name_to_field"), None, Fn(x, [FnParameter(Named("name"), None)], None, [
      Expression(Case(x, [Variable(x, "name")], {
        fields
        |> list.map(fn(field) {
          Clause([[PatternString(x, field.input_name)]], None, Call(x, Variable(x, "Ok"), [UnlabelledField(Variable(x, field.variant))]))
        })
        |> list.append([
          Clause([[PatternDiscard(x, "")]], None, Call(x, Variable(x, "Error"), [UnlabelledField(Variable(x, "Nil"))])),
        ])
      }))
    ]))

  let field_to_type =
    Assignment(x, Let, PatternVariable(x, "field_to_type"), None, Fn(x, [FnParameter(Named("field"), None)], None, [
      Expression(Case(x, [Variable(x, "field")], list.map(fields, fn(field) {
        Clause([[PatternVariant(x, None, field.variant, [], False)]], None, FieldAccess(x, Variable(x, gleam_type_module), string.inspect(field.type_)))
      })))
    ]))

  let lookups_return =
    Expression(Call(x, FieldAccess(x, Variable(x, "util"), "DerivedFormLookups"), [
      ShorthandField("name_to_field"),
      ShorthandField("field_to_name"),
      ShorthandField("field_to_type"),
      LabelledField("field_to_dom_id", FieldAccess(x, Variable(x, "util"), "inspect")),
      LabelledField("field_to_default_label", FnCapture(x, None, FieldAccess(x, Variable(x, "util"), "field_to_default_label"), [], [ShorthandField("field_to_name")]))
    ]))

  Function(x, lookups_func_name, Public, [],
    Some(NamedType(x, "DerivedFormLookups", Some("util"), [NamedType(x, form_field_type_name, None, []), NamedType(x, form_type_name, None, [])])),
    [
      field_to_name,
      name_to_field,
      field_to_type,

      lookups_return,
    ],
  )
  |> Definition(attributes: [], definition: _)
}
