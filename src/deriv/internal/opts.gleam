import gleam/io
import gleam/list
import gleam/regexp
import gleam/string
import deriv/internal/common
import gleam/result
import deriv/internal/types.{type DerivFieldOpt, type DerivField}
import glance as g
import gleam/option.{type Option, Some, None}

pub type FromIntoOverride {
  SpecifyField(ident: Ident, field: String)
  ConvAllWith(conv: Conv, inner: Option(Inner))
  ConvTypeWith(ident: Ident, conv: Conv, inner: Option(Inner))
}

pub type Ident {
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

pub type Inner {
  Option
  List
}

pub type Conv {
  Conv(
    subfields: List(String),
    func: Option(ConvFunc),
    inner: Option(Inner),
  )
}

pub type ConvFunc {
  ConvFunc(
    module: Option(String),
    name: String,
    args: ConvArgs,
  )
}

pub type ConvArgs {
  EntireValue
  ValueDotField
}

pub type FromInto {
  From
  Into
}

pub fn build_from_into_field_override(
  kind kind: FromInto,
  opt opt: DerivFieldOpt,
  field field: DerivField,
  type_ type_: g.Type,
) -> Result(FromIntoOverride, Nil) {
  let from_into =
    case kind {
      From -> "from"
      Into -> "into"
    }

  let not_inner = ""

  case opt.strs {
    [kind, ..rest] if kind == from_into -> {
      case rest, not_inner {
        [ident], _ -> {
          result.try(parse_ident(ident:), fn(ident) {
            case ident {
              IdentType(..) ->
                Error(Nil)

              IdentFieldForType(field:, ..) ->
                Ok(SpecifyField(ident:, field:))
            }
          })
        }

        ["*", "using", conv], inner_str |
        ["*", "using", "inner" as inner_str, conv], _ |
        ["using", conv], inner_str |
        ["using", conv, "inner" as inner_str], _ |
        ["using", "inner" as inner_str, conv], _ ->
          parse_override(ident: None, str1: conv, str2: None, inner_str:, field:, type_:,
            to_override: fn(_ident, conv, inner) {
              Ok(ConvAllWith(conv:, inner:))
            }
          )

        [ident, "using", "inner" as inner_str, conv], _ |
        [ident, "using", conv], inner_str ->
          parse_override(ident: Some(ident), str1: conv, str2: None, inner_str:, field:, type_:,
            to_override: fn(ident, conv, inner) {
              use ident <- result.try(ident)
              Ok(ConvTypeWith(ident:, conv:, inner:))
            }
          )

        [ident, "using", conv_or_field_access1, conv_or_field_access2], inner_str -> {
          let str1 = conv_or_field_access1
          let str2 = conv_or_field_access2 |> Some
          parse_override(ident: Some(ident), str1:, str2:, inner_str:, field:, type_:,
            to_override: fn(ident, conv, inner) {
              use ident <- result.try(ident)
              Ok(ConvTypeWith(ident:, conv:, inner:))
            }
          )
        }

        _, _ -> {
          io.println(opt.strs |> string.inspect)
          panic as { "`from` invalid field option: " <> opt.strs |> string.join(" ") }
        }
      }
    }

    _ -> {
      Error(Nil)
    }
  }
}

fn parse_override(
  ident ident: Option(String),
  str1 str1: String,
  str2 str2: Option(String),
  inner_str inner_str: String,
  field field: DerivField,
  type_ type_: g.Type,
  to_override to_override: fn(Result(Ident, Nil), Conv, Option(Inner)) -> Result(FromIntoOverride, Nil),
) -> Result(FromIntoOverride, Nil) {
  let inner = inner_str |> to_inner(relative_to: type_, on: field)

  let #(ident, args) =
    ident
    |> option.to_result(Nil)
    |> result.try(parse_ident)
    |> result.try(fn(ident) {
      // TODO clean up `*` handling
      let args =
        case ident.type_ == "*", ident.type_ |> string.contains("*") {
          True, _ -> ValueDotField
          False, True -> EntireValue
          False, False -> ValueDotField
         }

      let type_ = ident.type_ |> string.replace("*", "")
      let ident =
        case ident {
          IdentFieldForType(..) -> IdentFieldForType(..ident, type_:)
          IdentType(..) -> IdentType(..ident, type_:)
        }

      Ok(#(Ok(ident), args))
    })
    |> result.unwrap(#(Error(Nil), ValueDotField))

  let conv = parse_conv_or_panic(str1:, str2:, args:)

  to_override(ident, conv, inner)
}

pub fn parse_ident(
  ident ident: String,
) -> Result(Ident, Nil) {
  case ident |> string.split(".") {
    [""] -> Error(Nil)
    [type_] ->
      case common.starts_with_uppercase(type_) {
        True -> Ok(IdentType(module: None, type_:))
        False -> Error(Nil)
      }
    [module, type_, field] ->
      case common.starts_with_uppercase(type_) {
        True -> Ok(IdentFieldForType(module: Some(module), type_:, field:))
        False -> Error(Nil)
      }
    [a, b] ->
      case common.starts_with_uppercase(a), common.starts_with_uppercase(b), a, b {
        True, False, type_, field -> Ok(IdentFieldForType(module: None, type_:, field:))
        False, True, module, type_ -> Ok(IdentType(module: Some(module), type_:))
        _, _, _, _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn to_inner(
  inner_str inner_str: String,
  relative_to type_: g.Type,
  on field: DerivField,
) -> Option(Inner) {
  case inner_str == "inner", type_ {
    False, _ -> None
    True, g.NamedType(name: "Option", parameters: [_], ..) -> Some(Option)
    True, g.NamedType(name: "List", parameters: [_], ..) -> Some(List)
    True, _ -> panic as { "`from`: the `inner` option only makes sense in the context of a `List` or `Option` but got: \n" <>
      string.inspect(field) <> "\n" <>
      string.inspect(type_) <> "\n"
    }
  }
}

fn result_combine(
  result1 result1: Result(t, err),
  result2 result2: Result(t, err),
  combine combine: fn(t, t) -> t,
) -> Result(t, #(err, err)) {
  case result1, result2 {
    Ok(x1), Ok(x2) -> Ok(combine(x1, x2))
    Ok(x), Error(_) | Error(_), Ok(x) -> Ok(x)
    Error(err1), Error(err2) -> Error(#(err1, err2))
  }
}

fn parse_conv_or_panic(
  str1 str1: String,
  str2 str2: Option(String),
  args args: ConvArgs,
) -> Conv {
  let field_access =
    parse_field_access(str: str1)
    |> result.lazy_or(fn() {
      str2
      |> option.to_result(Nil)
      |> result.try(parse_field_access(str: _))
    })

  let conv_func =
    parse_conv_func(str: str1, args:)
    |> result.lazy_or(fn() {
      str2
      |> option.to_result(Nil)
      |> result.try(parse_conv_func(str: _, args:))
    })

  let result =
    result_combine(field_access, conv_func, combine: fn(field_access, conv_func) {
      let inner = option.or(conv_func.inner, field_access.inner)

      Conv(
        inner:,
        subfields: field_access.subfields,
        func: conv_func.func
      )
    })

  case result {
    Ok(conv) -> conv
    Error(_) -> panic as {
      "`from`/`into` field convert function specified by `using` is invalid. Valid syntax is `func_name` or `module.func_name` or `.field`, but got:\n" <>
      string.inspect(str1) <> "\n" <>
      string.inspect(str2)
    }
  }
}

// fn parse_conv_or_panic(
//   str str: String,
//   args args: ConvArgs,
// ) -> Conv {
//   let field_access = parse_field_access(str:)
//   let conv_func = parse_conv_func(str:, args:)

//   let result =
//     result_combine(field_access, conv_func, combine: fn(field_access, conv_func) {
//       let inner = option.or(conv_func.inner, field_access.inner)

//       Conv(
//         inner:,
//         subfields: field_access.subfields,
//         func: conv_func.func
//       )
//     })

//   case result {
//     Ok(conv) -> conv
//     Error(_) -> panic as {
//       "`from`/`into` field convert function specified by `using` is invalid. Valid syntax is `func_name` or `module.func_name` or `.field`, but got: " <> str
//     }
//   }
// }

fn parse_conv_func(
  str str: String,
  args args: ConvArgs,
) -> Result(Conv, Nil) {
  case str |> string.split(".") {
    [""] ->
      Error(Nil)

    [name] ->
      Ok(Conv(func: Some(ConvFunc(module: None, name:, args:)), inner: None, subfields: []))

    [module, name] ->
      Ok(Conv(func: Some(ConvFunc(module: Some(module), name:, args:)), subfields: [], inner: None))

    _ ->
      Error(Nil)
  }
}

fn parse_field_access(
  str str: String,
) -> Result(Conv, Nil) {
  let assert Ok(check_re) = "^([.][a-z0-9_]+)+$" |> regexp.from_string
  let assert Ok(scan_re) = "[a-z0-9_]+" |> regexp.from_string
  case regexp.check(check_re, str), regexp.scan(scan_re, str) {
    True, [_, ..] as matches -> {
      let subfields = matches |> list.map(fn(match) { match.content })

      Ok(Conv(subfields:, func: None, inner: None))
    }

    False, _ | _, _ ->
      Error(Nil)
  }
}

fn generalize_field_ident(
  ident ident: Ident,
) -> #(String, String) {
  case ident {
    IdentType(module:, type_:) -> #(module |> option.unwrap(""), type_)
    IdentFieldForType(module:, type_:, ..) -> #(module |> option.unwrap(""), type_)
  }
}

pub fn match_specific(
  field f: DerivField,
  ident ident: Ident,
  overrides overrides: List(FromIntoOverride),
) -> Result(FromIntoOverride, Nil) {
  let module_type = #(ident.module |> option.unwrap(""), ident.type_)

  overrides
  |> list.filter(fn(override) {
    case override {
      SpecifyField(ident:, ..) |
      ConvTypeWith(ident:, ..) ->
        generalize_field_ident(ident) == module_type

      _ ->
        False
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

pub fn match_general(
  field f: DerivField,
  ident ident: Ident,
  overrides overrides: List(FromIntoOverride),
) -> Result(FromIntoOverride, Nil) {
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

// fn parse_ident_with_field(
//   ident ident: String,
// ) -> #(String, String, Option(String)) {
//   case string.split(ident, ".") {
//     [""] -> #("", "", None)
//     [a, b] -> {
//       case a, b, common.starts_with_uppercase(b) {
//         module, type_, True -> #(module, module <> "." <> type_, None)
//         type_, field, False -> #("", type_, Some(field))
//       }
//     }
//     [module, type_, field] -> #(module, module <> "." <> type_, Some(field))
//     [type_] -> #("", type_, None)
//     // _ -> panic // TODO panic w/ error
//     _ -> #("", "", None)
//   }
// }
