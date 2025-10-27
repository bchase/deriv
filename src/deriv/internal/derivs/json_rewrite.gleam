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
  []
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
  let decode_imports =
    decode_imports(opts:, type_:)

  let encode_imports =
    case opts |> list.contains("encode") {
      False -> []
      True -> []
    }

  [
    decode_imports,
    encode_imports,
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

const default_imports =
  [
    #("decode", [
      g.Import(
        location: g.Span(start: -1, end: -1),
        module: "gleam/dynamic/decode",
        alias: None,
        unqualified_types: [
          g.UnqualifiedImport(
            name: "Decoder",
            alias: None,
          ),
        ],
        unqualified_values: [],
      ),
    ]),
    #("encode", [
      g.Import(
        location: g.Span(start: -1, end: -1),
        module: "gleam/json",
        alias: None,
        unqualified_types: [
          g.UnqualifiedImport(
            name: "Json",
            alias: None,
          ),
        ],
        unqualified_values: [],
      ),
    ]),
  ]


// DECODE

type DecodeType {
  DecodeType(
    publicity: g.Publicity,
    type_: g.Type,
    pascal_case: String,
    snake_case: String,
    variants: List(DecodeVariant),
  )
}

type DecodeVariant {
  DecodeVariant(
    pascal_case: String,
    snake_case: String,
    fields: List(DecodeField),
  )
}

type DecodeField {
  DecodeField(
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
) -> DecodeType {
  DecodeType(
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
) -> DecodeVariant {
  DecodeVariant(
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
) -> DecodeField {
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
        DecodeField(
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

      DecodeField(..field, json:)
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
  field field: DecodeField,
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
  type_ type_: DecodeType,
) -> g.Function {
  case type_.variants {
    [variant, ..variants] ->
      type_decoder_func_(type_:, variant:, variants:)

    _ ->
      panic as { "`derive json decode` doesn't know what to do for types with no variants" }
  }
}

fn type_decoder_func_(
  type_ type_: DecodeType,
  variant variant: DecodeVariant,
  variants variants: List(DecodeVariant),
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
  type_ type_: DecodeType,
  variant variant: DecodeVariant,
) -> String {
  "decoder_" <> type_.snake_case <> "_" <> variant.snake_case
}

fn decoder_return_type(
  type_ type_: DecodeType,
) -> g.Type {
  g.NamedType(x, module: None,
    name: "Decoder",
    parameters: [type_.type_],
  )
}

fn variant_decoder_func(
  type_ type_: DecodeType,
  variant variant: DecodeVariant,
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
  field field: DecodeField,
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

fn decode_field_call(
  field field: DecodeField,
  opts opts: DerivFieldOpts,
) -> g.Expression {
  let decoder = fn(type_name) {
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
    |> result.unwrap(
      "decode" |> dot(type_name |> common.snake_case)
    )
  }

  case field.json, field.type_.name, field.type_.params {
    [], _, _ -> {
      panic as { "`derive decode` needs a JSON property, but found none for: " <> string.inspect(field) }
    }

    [prop], "List", [T(params: [], ..) as t] -> {
     let type_ =
        t.name |> common.snake_case

      "decode" |> dot("optional_field") |> call([
        string(prop),
        list([]),
        "decode" |> dot("list") |> call([decoder(type_)]),
      ])
    }
    [_prop1, _prop2, ..] as props, "List", [T(params: [], ..) as t] -> {
      // TODO
      panic as "UNIMPLEMENTED (`decode.subfield` for `List(t)`)"
    }

    [prop], "Option", [T(params: [], ..) as t] -> {
      let type_ =
        t.name |> common.snake_case

      "decode" |> dot("optional_field") |> call([
        string(prop),
        "deriv" |> dot("none"),
        "decode" |> dot("optional") |> call([decoder(type_)]),
      ])
    }
    [_prop1, _prop2, ..] as props, "Option", [T(params: [], ..) as t] -> {
      // TODO
      panic as "UNIMPLEMENTED (`decode.subfield` for `Option(t)`)"
    }

    [prop], _, _ -> {
      let type_ =
        field.type_.name |> common.snake_case

      "decode" |> dot("field") |> call([
        string(prop),
        decoder(type_),
      ])
    }

    [_prop1, _prop2, ..] as props, _, _ -> {
      let type_ =
        field.type_.name |> common.snake_case

      "decode" |> dot("subfield") |> call([
        list(props |> list.map(string)),
        decoder(type_),
      ])
    }
  }
}

fn decode_success(
  variant variant: DecodeVariant,
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
  variant variant: DecodeVariant,
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
