import gleam/io
import gleam/list
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
    module: Option(String),
    func: String,
    args: ConvArgs,
    inner: Option(Inner),
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
  from_into from_into: FromInto,
  opt opt: DerivFieldOpt,
  field field: DerivField,
  type_ type_: g.Type,
) -> Result(FromIntoOverride, Nil) {
  let from_into =
    case from_into {
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
        ["using", conv, "inner" as inner_str], _ -> {
          let inner = inner_str |> to_inner(relative_to: type_, on: field)

          Ok(ConvAllWith(conv: { conv |> parse_conv_or_panic }(ValueDotField), inner:))
        }

        ["using", "inner" as inner_str, conv], _ -> {
          let build_conv = conv |> parse_conv_or_panic
          let conv = build_conv(ValueDotField)
          let inner = inner_str |> to_inner(relative_to: type_, on: field)
          Ok(ConvAllWith(conv:, inner:))
        }

        [ident, "using", "inner" as inner_str, conv], _ |
        [ident, "using", conv], inner_str -> {
          let inner = inner_str |> to_inner(relative_to: type_, on: field)

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
            Ok(ConvTypeWith(ident:, conv:, inner:))
          })
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

fn parse_conv_or_panic(
  str str: String,
) -> fn(ConvArgs) -> Conv {
  case str |> string.split(".") {
    [""] -> panic as { "`from` must specify a convert function for `using`" }
    [func] -> fn(args) { Conv(module: None, func:, args:, inner: None) }
    [module, func] -> fn(args) { Conv(module: Some(module), func:, args:, inner: None) }
    _ -> panic as {
      "`from` field convert function specified by `using` is invalid. Valid syntax is `func_name` or `module.func_name`, but got: " <> str
    }
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
