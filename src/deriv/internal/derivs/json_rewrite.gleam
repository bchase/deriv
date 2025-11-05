import gleam/bool
import gleam/io
import gleam/option.{type Option, Some, None}
import gleam/dict
import gleam/result
import gleam/list
import gleam/string
import glance as g
import deriv/internal/types.{type File, type Derivation, type DerivFieldOpt, type Gen, Gen, type DerivFieldOpts, type ModuleReader, DerivFieldOpt} as deriv
import deriv/internal/common.{type BirlTimeKind, BirlTimeISO8601, BirlTimeUnixMicro, BirlTimeUnixMilli, BirlTimeUnix, BirlTimeHTTP, BirlTimeNaive}

// TODO conv:
//   - aggregate nested encodes for shared keys, e.g. `nested.foo` & `nested.bar` in same `#("nested", _)`
//   - `use nested_option <- decode.then(decode.at(`

const deriv_variant_json_key = "_var"

type Context {
  Context(
    deriv: Derivation,
    all_field_opts: DerivFieldOpts,
    file: File,
    module_reader: ModuleReader,
    type_aliases: List(g.TypeAlias),
  )
}

pub fn gen(
  type_: deriv.Type,
  deriv: Derivation,
  field_opts: DerivFieldOpts,
  file: File,
  module_reader: ModuleReader,
) -> Gen {
  let imports =
    case type_ {
      deriv.Type(type_:) ->
        gen_imports(deriv.opts, type_)

      deriv.TypeAlias(..) ->
        []
    }

  let gen_funcs_for_opts =
    [
      #("decode", gen_json_decoders),
      #("encode", gen_json_encoders |> to_gen_func(deriv, module_reader)),
    ]
    |> dict.from_list

  let funcs =
    deriv.opts
    |> list.map(dict.get(gen_funcs_for_opts, _))
    |> result.values
    |> list.flat_map(fn(f) { f(type_, field_opts, file)})

  let src =
    funcs
    |> list.map(common.func_str)
    |> string.join("\n\n")

  Gen(file:, deriv:, imports:, funcs:, types: [], src:, meta: dict.new())
}

fn gen_json_decoders(
  type_: deriv.Type,
  opts: DerivFieldOpts,
  file: File,
) -> List(g.Definition(g.Function)) {
  case type_ {
    deriv.TypeAlias(..) ->
      panic as "not implemented"

    deriv.Type(type_:) -> {
      let type_ = to_decode_type(type_:, opts:)

      [
        type_decoder_func(type_:),
        ..{
          type_.variants
          |> list.map(variant_decoder_func(type_:, variant: _, opts:))
        }
      ]
      |> list.map(g.Definition([], _))
    }
  }
}

fn gen_json_encoders(
  type_: deriv.Type,
  ctx: Context,
) -> List(g.Definition(g.Function)) {
  case type_ {
    deriv.TypeAlias(..) ->
      panic as "not implemented"

    deriv.Type(type_:) -> {
      let type_ = to_decode_type(type_:, opts: ctx.all_field_opts)

      [
        type_encode_func(type_:, opts: ctx.all_field_opts),
      ]
      |> list.map(g.Definition([], _))
    }
  }
}

fn to_gen_func(
  f: fn(deriv.Type, Context) -> List(g.Definition(g.Function)),
  deriv: Derivation,
  module_reader: ModuleReader,
) -> fn(deriv.Type, DerivFieldOpts, File) -> List(g.Definition(g.Function)) {
  fn(
    type_: deriv.Type,
    field_opts: DerivFieldOpts,
    file: File,
  ) {
    f(type_, Context(file:, deriv:, module_reader:, all_field_opts: field_opts, type_aliases: type_aliases_in(file:)))
  }
}

fn type_aliases_in(
  file file: File,
) -> List(g.TypeAlias) {
  let assert Ok(g.Module(type_aliases:, ..)) =
    g.module(file.src)

  type_aliases
  |> list.map(fn(ta) { ta.definition })
}

fn gen_imports(
  opts: List(String),
  type_: g.CustomType,
) -> List(g.Import) {
  [
    decode_imports(opts:, type_:),
    encode_imports(opts:, type_:),
  ]
  |> list.flatten
}

fn decode_imports(
  opts opts: List(String),
  type_ type_: g.CustomType,
) -> List(g.Import) {
  use <- bool.guard(!{opts |> list.contains("decode")}, return: [])

  let standard = [
    common.import__(
      module: "gleam/dynamic/decode",
      as_: None,
      values: [],
      types: ["Decoder"],
    )
  ]

  let util_import = {
    use <- bool.guard(!{type_ |> common.are_any_fields_options}, return: [])

    [
      common.import__(
        module: "deriv/util",
        as_: Some("deriv"),
        values: [],
        types: [],
      )
    ]
  }

  standard
  |> list.append(util_import)
}

fn encode_imports(
  opts opts: List(String),
  type_ type_: g.CustomType,
) -> List(g.Import) {
  use <- bool.guard(!{opts |> list.contains("decode")}, return: [])

  let standard = [
    common.import__(
      module: "gleam/json",
      as_: None,
      values: [],
      types: ["Json"],
    )
  ]

  // let util_import = {
  //   use <- bool.guard(!{type_ |> common.are_any_fields_options}, return: [])

  //   [
  //     common.import__(
  //       module: "deriv/util",
  //       as_: Some("deriv"),
  //       values: [],
  //       types: [],
  //     )
  //   ]
  // }

  standard
  // |> list.append(util_import)
}

//

type Type {
  Type(
    publicity: g.Publicity,
    type_: g.Type,
    pascal_case: String,
    snake_case: String,
    variants: List(Variant),
  )
}

type Variant {
  Variant(
    pascal_case: String,
    snake_case: String,
    fields: List(Field),
  )
}

type Field {
  Field(
    gleam: String,
    json: List(String),
    type_: T,
    variant_pascal_case: String,
    type_pascal_case: String,
  )
}

type T {
  T(
    name: String,
    params: List(T),
  )
}

fn to_glance_type(
  type_ type_: g.CustomType,
) -> g.Type {
  case type_.parameters {
    [_, ..] -> {
      io.println("")
      io.println("`derive json` ISSUE WITH THIS TYPE:")
      io.println(string.inspect(type_))

      panic as {
        "`derive json` only understands unparameterized custom types, but encountered the type printed above"
      }
    }

    [] ->
      g.NamedType(x,
        name: type_.name,
        module: None,
        parameters: [],
      )
  }
}

fn to_decode_type(
  type_ type_: g.CustomType,
  opts opts: DerivFieldOpts,
) -> Type {
  Type(
    publicity: type_.publicity,
    type_: type_ |> to_glance_type,
    pascal_case: type_.name,
    snake_case: type_.name |> common.snake_case,
    variants: {
      type_.variants
      |> list.map(to_decode_variant(
        type_:,
        variant: _,
        opts:,
      ))
    },
  )
}
fn to_decode_variant(
  type_ type_: g.CustomType,
  variant variant: g.Variant,
  opts opts: DerivFieldOpts,
) -> Variant {
  Variant(
    pascal_case: variant.name,
    snake_case: variant.name |> common.snake_case,
    fields: {
      variant.fields
      |> list.map(to_decode_field(
        type_:,
        variant:,
        field: _,
        opts:,
      ))
    },
  )
}
fn to_decode_field(
  type_ custom_type: g.CustomType,
  variant variant: g.Variant,
  field field: g.VariantField,
  opts opts: DerivFieldOpts,
) -> Field {
  case field {
    g.UnlabelledVariantField(..) -> {
      io.println("")
      io.println("`derive json` ISSUE WITH THIS TYPE:")
      io.println(string.inspect(custom_type))
      io.println("")
      io.println("`derive json` ON THIS FIELD:")
      io.println(string.inspect(field))
      io.println("")

      panic as {
        "`derive json` only understands variants with named fields, but encountered the type printed above"
      }
    }

    g.LabelledVariantField(label:, item: type_) -> {
      let field =
        Field(
          gleam: label,
          json: [label],
          type_: type_ |> to_t(custom_type:),
          type_pascal_case: custom_type.name,
          variant_pascal_case: variant.name,
        )


      let json =
        get_field_opt(field:, opts:, desc: "json named", matching: fn(opt) {
          case opt {
            ["json", "named", path] ->
              Ok(path |> string.split("."))

            _ ->
              Error(Nil)
          }
        })
        |> result.unwrap([label])

      Field(..field, json:)
    }
  }
}
fn to_t(
  type_ type_: g.Type,
  custom_type custom_type: g.CustomType,
) -> T {
  case type_ {
    g.TupleType(..) |
    g.FunctionType(..) |
    g.VariableType(..) |
    g.HoleType(..) -> {
      io.println("")
      io.println("`derive json` ISSUE WITH THIS TYPE:")
      io.println(string.inspect(custom_type))
      io.println("")
      io.println("`derive json` ON THIS FIELD:")
      io.println(string.inspect(type_))
      io.println("")

      panic as {
        "`derive json` only understands `glance.NamedType`s, but encountered the type printed above"
      }
    }

    g.NamedType(name:, parameters:, ..) ->
      T(
        name:,
        params: {
          parameters
          |> list.map(to_t(type_: _, custom_type:))
        },
      )
  }
}

fn get_field_opt(
  field field: Field,
  opts opts: DerivFieldOpts,
  desc desc: String,
  matching matching: fn(List(String)) -> Result(t, Nil),
) -> Result(t, Nil) {
  common.get_field_opt(
    opts:,
    type_: field.type_pascal_case,
    variant: field.variant_pascal_case,
    field: field.gleam,
    err_msg: string.join([
      "`deriv` found multiple `",
      desc,
      "` opts for: ",
      field.type_pascal_case,
      " ",
      field.variant_pascal_case,
      ".",
      field.gleam,
    ], ""),
    matching:,
  )
}

// HELPERS

const x = g.Span(-1, -1)

fn string(
  str str: String,
) -> g.Expression {
  g.String(x, str)
}

fn list(
  xs: List(g.Expression)
) -> g.Expression {
  g.List(x, xs, None)
}

fn term(
  str: String,
) -> g.Expression {
  g.Variable(x, str)
}

fn call(
  function f: g.Expression,
  arguments args: List(g.Expression),
) -> g.Expression {
  f |> call_(args |> list.map(g.UnlabelledField))
}

fn call_(
  function f: g.Expression,
  arguments args: List(g.Field(g.Expression)),
) -> g.Expression {
  g.Call(x, f, args)
}

fn dot(
  a: String,
  b: String,
) -> g.Expression {
  g.FieldAccess(x, term(a), b)
}

// DECODER FUNC GEN

fn type_decoder_func(
  type_ type_: Type,
) -> g.Function {
  case type_.variants {
    [variant, ..variants] ->
      type_decoder_func_(type_:, variant:, variants:)

    _ ->
      panic as { "`derive json decode` doesn't know what to do for types with no variants" }
  }
}

fn type_decoder_func_(
  type_ type_: Type,
  variant variant: Variant,
  variants variants: List(Variant),
) -> g.Function {
  let call_decoder = fn(variant) {
    variant
    |> variant_decoder_name(type_:, variant: _)
    |> term
    |> call([])
  }

  g.Function(x,
    name: "decoder_" <> type_.snake_case,
    publicity: type_.publicity,
    parameters: [],
    return: Some(decoder_return_type(type_:)),
    body: {
      "decode"
      |> dot("one_of")
      |> call([
        variant |> call_decoder,
        variants |> list.map(call_decoder) |> list,
      ])
      |> g.Expression
      |> list.wrap
    },
  )
}

fn variant_decoder_name(
  type_ type_: Type,
  variant variant: Variant,
) -> String {
  "decoder_" <> type_.snake_case <> "_" <> variant.snake_case
}

fn decoder_return_type(
  type_ type_: Type,
) -> g.Type {
  g.NamedType(x, module: None,
    name: "Decoder",
    parameters: [type_.type_],
  )
}

fn variant_decoder_func(
  type_ type_: Type,
  variant variant: Variant,
  opts opts: DerivFieldOpts,
) -> g.Function {
  let use_lines =
    variant.fields
    |> list.map(use_decode_field_line(field: _, opts:))

  g.Function(x,
    name: variant_decoder_name(type_:, variant:),
    publicity: type_.publicity,
    parameters: [],
    return: Some(decoder_return_type(type_:)),
    body: {
      use_lines
      |> list.append([decode_success(variant:)])
    },
  )
}

fn use_decode_field_line(
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Statement {
  let field_gleam_name =
    g.UsePattern(
      pattern: g.PatternVariable(x, field.gleam),
      annotation: None,
    )

  let decode_field_call =
    decode_field_call(field:, opts:)

  g.Use(x,
    patterns: [field_gleam_name],
    function: decode_field_call,
  )
}

fn decoder_call(
  type_ type_: T,
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  let type_name =
    type_.name |> common.snake_case

  let decoder_override =
    get_field_opt(field:, opts:, desc: "json decoder", matching: fn(opt) {
      case opt {
        ["json", "decoder", decoder] ->
          Ok(decoder)

        _ ->
          Error(Nil)
      }
    })

  decoder_override
  |> result.map(fn(decoder_name) {
    decoder_name |> term |> call([])
  })
  |> result.lazy_unwrap(fn() {
    case type_.name, type_.params {
      "List", [T(name: "Option", params:[_]) as option_type] ->
        "decode" |> dot("list")
        |> call([
          decoder_call(field:, opts:, type_: option_type),
        ])

      "Option", [T(name: "List", params:[_]) as list_type] ->
        "decode" |> dot("optional")
        |> call([
          decoder_call(field:, opts:, type_: list_type),
        ])

      "Option", [T(params:[], ..) as inner_type] ->
        "decode" |> dot("optional") |> call([decoder_call(field:, opts:, type_: inner_type)])

      "List", [T(params:[], ..) as inner_type] ->
        "decode" |> dot("list") |> call([decoder_call(field:, opts:, type_: inner_type)])

      "String", [] |
      "Int", [] |
      "Float", [] |
      "Bool", [] ->
        "decode" |> dot(type_name |> common.snake_case)

      type_name, _ ->
        { "decoder_" <> type_name |> common.snake_case } |> term |> call([])
    }
  })
}

fn decode_field_call(
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  case field.json, field.type_.name, field.type_.params {
    [], _, _ -> {
      panic as { "`derive decode` needs a JSON property, but found none for: " <> string.inspect(field) }
    }

    [prop], "List", [T(name: "Option", params: [_])] -> {
      "decode" |> dot("field") |> call([
        string(prop),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }
    [prop], "List", [T(params: [], ..)] -> {
      "decode" |> dot("optional_field") |> call([
        string(prop),
        list([]),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }
    [_prop1, _prop2, ..] as props, "List", [T(params: [], ..)] -> {
      "decode" |> dot("subfield") |> call([
        list(props |> list.map(string)),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }

    [prop], "Option", [T(name: "List", params: [_]) as t] -> {
      "decode" |> dot("optional_field") |> call([
        string(prop),
        "deriv" |> dot("none"),
        "decode" |> dot("optional") |> call([
          decoder_call(field:, opts:, type_: t),
        ]),
      ])
    }
    [prop], "Option", [T(params: [], ..)] -> {
      "decode" |> dot("optional_field") |> call([
        string(prop),
        "deriv" |> dot("none"),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }
    [_prop1, _prop2, ..] as props, "Option", [T(name: "List", params: [_])] -> {
      "deriv" |> dot("decode_optional_subfield") |> call([
        list(props |> list.map(string)),
        "deriv" |> dot("none"),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }
    [_prop1, _prop2, ..] as props, "Option", [T(params: [], ..)] -> {
      "decode" |> dot("then") |> call([
        "decode" |> dot("at") |> call([
          list(props |> list.map(string)),
          decoder_call(field:, opts:, type_: field.type_),
        ]),
      ])
    }

    [prop], _, _ -> {
      "decode" |> dot("field") |> call([
        string(prop),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }

    [_prop1, _prop2, ..] as props, _, _ -> {
      "decode" |> dot("subfield") |> call([
        list(props |> list.map(string)),
        decoder_call(field:, opts:, type_: field.type_),
      ])
    }
  }
}

fn decode_success(
  variant variant: Variant,
) -> g.Statement {
  let constr_call =
    variant
    |> variant_decoder_constructor

  "decode"
  |> dot("success")
  |> call([constr_call])
  |> g.Expression
}

fn variant_decoder_constructor(
  variant variant: Variant,
) -> g.Expression {
  let constr =
    variant.pascal_case
    |> term

  let arguments =
    variant.fields
    |> list.map(fn(field) {
      g.ShorthandField(field.gleam)
    })

  constr
  |> call_(arguments:)
}

// ENCODE FUNC GEN

fn type_encode_func(
  type_ type_: Type,
  opts opts: DerivFieldOpts,
) -> g.Function {
  g.Function(x,
    name: "encode_" <> type_.snake_case,
    publicity: type_.publicity,
    parameters: [
      g.FunctionParameter(
        label: None,
        name: g.Named("value"),
        type_: Some(type_.type_),
      )
    ],
    return: Some(g.NamedType(x, name: "Json", module: None, parameters: [])),
    body: {
      g.Case(x, subjects: ["value" |> term], clauses: {
        type_.variants
        |> list.map(variant_encode_case_clause(variant: _, type_:, opts:))
      })
      |> g.Expression
      |> list.wrap
    },
  )
}

fn variant_encode_case_clause(
  type_ type_: Type,
  variant variant: Variant,
  opts opts: DerivFieldOpts,
) -> g.Clause {
  g.Clause(
    patterns: [[
      g.PatternAssignment(x,
        attern: g.PatternVariant(x,
          module: None,
          constructor: variant.pascal_case,
          arguments: [],
          with_spread: True,
        ),
        name: "value",
      ),
    ]],
    guard: None,
    body: {
      "json" |> dot("object") |> call({
        variant.fields
        |> list.map(fn(field) {
          encode_field(properties: field.json, field:, opts:)
        })
        |> list
        |> list.wrap
      })
    }
  )
}

fn encode_field(
  properties path: List(String),
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  case path {
    [prop] ->
      g.Tuple(x, [
        string(prop),
        encode_call(type_: field.type_, field:, opts:),
      ])

    [prop, ..properties] ->
      g.Tuple(x, [
        string(prop),
        "json" |> dot("object") |> call([list([
          encode_field(properties:, field:, opts:),
        ])])
      ])

    [] ->
      panic
  }
}

fn encode_call(
  type_ type_: T,
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  let encode_override =
    get_field_opt(field:, opts:, desc: "json encode", matching: fn(opt) {
      case opt {
        ["json", "encode", encode] ->
          Ok(encode)

        _ ->
          Error(Nil)
      }
    })

  encode_override
  |> result.map(fn(encode_name) {
    encode_name |> term |> call([
      "value" |> dot(field.gleam)
    ])
  })
  |> result.lazy_unwrap(fn() {
    encode_call_(type_:, field:, opts:, value_arg: True)
  })
}

fn encode_call_(
  type_ type_: T,
  field field: Field,
  opts opts: DerivFieldOpts,
  value_arg value_arg: Bool,
) -> g.Expression {
  let value =
    case value_arg {
      True -> "value" |> dot(field.gleam)
      False -> "_" |> term
    }

  case type_.name, type_.params {
    "Option", [inner_type] ->
      "json" |> dot("nullable") |> call([
        value,
        encode_call_(type_: inner_type, field:, opts:, value_arg: False),
      ])

    "List", [inner_type] ->
      "json" |> dot("array") |> call([
        value,
        encode_call_(type_: inner_type, field:, opts:, value_arg: False),
      ])

    "String", [] |
    "Int", [] |
    "Float", [] |
    "Bool", [] ->
      json_encode_func(type_:, field:, opts:) |> call([
        value,
      ])

    _, _ ->
      { "encode_" <> type_.name |> common.snake_case } |> term |> call([
        value,
      ])
  }
}

fn json_encode_func(
  type_ type_: T,
  field field: Field,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  case type_.name, type_.params {
    // "Option", [T(params:[], ..) as inner_type] ->
    //   "json" |> dot("nullable") |> call([
    //     encode_call_(field:, opts:, type_: inner_type),
    //   ])

    // "List", [T(params:[], ..) as inner_type] ->
    //   "json" |> dot("array") |> call([
    //     encode_call_(field:, opts:, type_: inner_type),
    //   ])

    "String", [] |
    "Int", [] |
    "Float", [] |
    "Bool", [] ->
      "json" |> dot(type_.name |> common.snake_case)

    _, _ ->
      { "encode_" <> type_.name |> common.snake_case } |> term
  }
}
