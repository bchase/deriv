import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/http
import gleam/result
import gleam/string
import gleam/int
import gleam/float
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/regexp
import decode.{type Decoder}
import youid/uuid.{type Uuid}

pub type Form(form, field) {
  Form(
    lookup: Lookups(field),
    fields: Fields(field),
    form: form,
  )
}

pub type Fields(field) = Dict(field, List(String))

pub type GleamType {
  String
  Int
  Float
  Bool
  Option(GleamType)
  List(GleamType)

  Enum(ident: String)

  Uuid
}

pub type Lookups(field) {
  Lookups(
    field_to_id: fn(field) -> String,
    field_to_name: fn(field) -> String,
    field_to_label: fn(field) -> String,
    field_to_type: fn(field) -> GleamType,
    field_is_required: fn(field) -> Bool,
    name_to_field: fn(String) -> Result(field, Nil),
  )
}

pub type Err(field) {
  FieldMissingError(field: field)
  FieldLookupScalarHasMultipleValuesErr(field: field)
  FieldDecodeError(field: field, errs: List(dynamic.DecodeError))
  FormInvalid(errs: Dict(#(field, List(Int)), List(String)))
}

pub fn fields(
  data: List(#(String, String)),
  name_to_field: fn(String) -> Result(field, Nil),
) -> Fields(field) {
  data
  |> list.map(fn(x) {
    let #(key, val) = x

    key
    |> name_to_field
    |> result.map(fn(field) {
      #(field, val)
    })
  })
  |> result.values
  |> list.group(fn(x) {
    let #(key, _val) = x
    key
  })
  |> dict.to_list
  |> list.map(fn(x) {
    let #(key, kvs) = x
    let vals =
      list.map(kvs, fn(x) {
        let #(_key, val) = x
        val
      })
    #(key, vals)
  })
  |> dict.from_list
}

pub fn get_field(
  fields: Fields(field),
  field: field,
  // lookup: Lookups(field),
) -> Option(String) {
  fields
  |> dict.get(field)
  |> result.map(list.first)
  |> result.flatten
  |> option.from_result
}

pub fn maybe(
  result: Result(t, Err(field))
) -> Result(Option(t), Err(field)) {
  case result {
    Ok(x) ->
      Ok(Some(x))

    Error(FieldMissingError(..)) ->
      Ok(None)

    Error(FieldDecodeError(..)) ->
      Ok(None)

    Error(err) ->
      Error(err)
  }
}

pub fn scalar(field: field, decoder: Decoder(t), fields: Fields(field)) -> Result(t, Err(field)) {
  case dict.get(fields, field) {
    Ok([str]) ->
      decode.from(decoder, dynamic.from(str))
      |> result.map_error(FieldDecodeError(field:, errs: _))

    Ok([]) ->
      Error(FieldMissingError(field))

    Ok(_) ->
      Error(FieldLookupScalarHasMultipleValuesErr(field))

    Error(Nil) ->
      Error(FieldMissingError(field))
  }
}

pub fn bool(field: field, fields: Fields(field)) -> Result(Bool, Err(field)) {
  case scalar(field, decoder_bool(), fields) {
    Ok(bool) -> Ok(bool)
    Error(_) -> Ok(False)
  }
}

pub fn array(field: field, decoder: Decoder(t), fields: Fields(field)) -> Result(List(t), Err(field)) {
  case dict.get(fields, field) {
    Ok(strs) ->
      decode.from(decode.list(decoder), dynamic.from(strs))
      |> result.map_error(FieldDecodeError(field:, errs: _))

    Error(Nil) ->
      // Error(FieldMissingError(field))
      Ok([])
  }
}

pub fn decode_(
  data: List(#(String, String)),
  decoder: Decoder(t),
) -> Result(t, List(dynamic.DecodeError)) {
  decode.from(decoder, dynamic.from(dict.from_list(data)))
}

fn decode_string(
  into type_: String,
  using func: fn(String) -> Result(t, err),
) -> Decoder(t) {
  decode.string
  |> decode.then(fn(str) {
    let assert Ok(ws_only_re) = "^\\s*$" |> regexp.from_string

    case str |> regexp.check(ws_only_re, _) {
      False -> decode.into(str)
      True -> decode.fail("String is only whitespace")
    }
  })
  |> decode.then(fn(str) {
    case func(str) {
      Ok(x) -> decode.into(x)
      Error(_) -> decode.fail("Value is not an `" <> type_ <> "`: `\"" <> str <>  "\"`")
    }
  })
}

pub fn decoder_string() -> Decoder(String) {
  decode_string(into: "String", using: Ok)
}

pub fn decoder_int() -> Decoder(Int) {
  decode_string(into: "Int", using: int.parse)
}

pub fn decoder_float() -> Decoder(Float) {
  decode_string(into: "Float", using: float.parse)
}

pub fn decoder_bool() -> Decoder(Bool) {
  decode.one_of([
    decode.string
    |> decode.then(fn(str) {
      str
      |> parse_bool
      |> result.map(decode.into)
      |> result.map_error(decode.fail)
      |> result.unwrap_both
    }),
    decode.into(False),
  ])
}

pub fn bool_value(bool: Bool) -> String {
  case bool {
    True -> "on"
    False -> ""
  }
}

pub fn parse_bool(str: String) -> Result(Bool, String) {
  case string.lowercase(str) {
    "" -> Ok(False)
    "on" -> Ok(True)
    "off" -> Ok(False)
    "true" -> Ok(True)
    "false" -> Ok(False)
    _ -> Error("Value is not a `Bool`: `\"" <> str <>  "\"`")
  }
}

pub fn decoder_uuid() -> Decoder(Uuid) {
  decode.string
  |> decode.then(fn(str) {
    case uuid.from_string(str) {
      Ok(uuid) -> decode.into(uuid)
      Error(_) -> decode.fail("Value is not a `Uuid`: `\"" <> str <>  "\"`")
    }
  })
}
