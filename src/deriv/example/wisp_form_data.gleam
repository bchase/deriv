import gleam/option.{type Option, Some, None}
import gleam/result
import gleam/string
import gleam/dict.{type Dict}
import gleam/list
import gleam/int
import gleam/float
import gleam/dynamic
import gleam/regexp.{Match}
import decode.{type Decoder}

// TODO
// X - arrays
// X - nested forms
// X - optional/nullable
// ? - hashes/maps/dicts
// ? - array of nested

pub type FormData = List(#(String, String))

pub type PersonForm {
  //$ derive form(person)
  PersonForm(
    name: String,
    age: Int,
    kd: Option(Float),
    active: Bool,
    hobbies: List(String),
    pet: Option(PetForm),
  )
}

pub type PetForm {
  PetForm(
    name: String,
    age: Int,
  )
}

//// DERIVED ////

pub fn person_input_name(field: PersonFormField) -> String {
  case field {
    PersonName -> "person[name]"
    PersonAge -> "person[age]"
    PersonKd -> "person[kd]"
    PersonActive -> "person[active]"
    PersonHobbies -> "person[hobbies][]"
    PersonPet(PetName) -> "person[pet][name]"
    PersonPet(PetAge) -> "person[pet][age]"
  }
}

pub fn decode_person_form(form: FormData) -> Result(PersonForm, Nil) {
  decode_person_form_(form, "person")
}
pub fn decode_person_form_(form: FormData, f: String) -> Result(PersonForm, Nil) {
  let fields = fields(form)

  use name <- result.try(scalar(person_form_field(f, PersonName), decode.string, fields))
  use age <- result.try(scalar(person_form_field(f, PersonAge), decoder_int(), fields))
  let kd = option.from_result(scalar(person_form_field(f, PersonKd), decoder_float(), fields))
  use active <- result.try(scalar(person_form_field(f, PersonActive), decoder_bool(), fields))
  use hobbies <- result.try(array(person_form_field(f, PersonHobbies), decode.string, fields))

  // use pet <- result.try({
  //   use name <- result.try(scalar(person_form_field(f, PersonPet(PetName)), decode.string, fields))
  //   use age <- result.try(scalar(person_form_field(f, PersonPet(PetAge)), decoder_int(), fields))

  //   Ok(PetForm(name:, age:))
  // })
  let pet = option.from_result({
    use name <- result.try(scalar(person_form_field(f, PersonPet(PetName)), decode.string, fields))
    use age <- result.try(scalar(person_form_field(f, PersonPet(PetAge)), decoder_int(), fields))

    Ok(PetForm(name:, age:))
  })

  Ok(PersonForm(name:, age:, kd:, active:, hobbies:, pet:))
}

pub type PersonFormField {
  PersonName
  PersonAge
  PersonKd
  PersonActive
  PersonHobbies
  PersonPet(PetFormField)
}

pub type PetFormField {
  PetName
  PetAge
}

fn person_form_field(form: String, field: PersonFormField) -> Field {
  case field {
    PersonName -> Field(keys:[ Key(form), Key("name") ])
    PersonAge -> Field(keys:[ Key(form), Key("age") ])
    PersonKd -> Field(keys:[ Key(form), Key("kd") ])
    PersonActive -> Field(keys:[ Key(form), Key("active") ])
    PersonHobbies -> Field(keys:[ Key(form), Key("hobbies"), Array ])
    PersonPet(PetName) -> Field(keys: [ Key(form), Key("pet"), Key("name") ])
    PersonPet(PetAge) -> Field(keys: [ Key(form), Key("pet"), Key("age") ])
  }
}


//// GENERIC DERIV HELPERS ////

type Key {
  Key(name: String)
  Array
}

type Field {
  Field(
    keys: List(Key),
  )
}

type TokenParse {
  TokenParse(
    matches: List(String),
    rest: String
  )
}

fn get_keys(acc: TokenParse) -> Result(TokenParse, String) {
  case acc.rest {
    "" ->
      Ok(acc)

    key -> {
      let assert Ok(key_re) = regexp.from_string("^\\[(\\w*)\\]")
      let brackets_length = string.length("[]")

      case regexp.scan(key_re, key) {
        [Match(_, [None])] -> {
          let rest = string.drop_start(acc.rest, brackets_length)
          let matches = acc.matches |> list.append([""])

          TokenParse(matches:, rest:)
          |> get_keys
        }

        [Match(_, [Some(key)])] -> {
          let rest = string.drop_start(acc.rest, string.length(key) + brackets_length)
          let matches = acc.matches |> list.append([key])

          TokenParse(matches:, rest:)
          |> get_keys
        }

        _ ->
          Error("Regex match failed on partial key: `\"" <> key <> "`\"")
      }
    }
  }
}

fn get_form(key: String) -> Result(TokenParse, String) {
  let assert Ok(form_name_re) = regexp.from_string("^(\\w+)")

  case regexp.scan(form_name_re, key) {
    [Match(_, [Some(form)])] -> {
      let rest = string.drop_start(key, string.length(form))

      Ok(TokenParse(matches: [form], rest:))
    }

    _ ->
      Error("Regex form name match failed on key: `\"" <> key <> "`\"")
  }
}

fn parse_field(key: String) -> Result(Field, String) {
  use acc <- result.try(get_form(key))
  use acc <- result.try(get_keys(acc))

  to_field(acc.matches)
}

fn to_field(tokens: List(String)) -> Result(Field, String) {
  case tokens {
    [] ->
      Error("Empty field tokens")

    keys -> {
      let keys =
        keys
        |> list.map(fn(key) {
          case key {
            "" -> Array
            token -> Key(token)
          }
        })

      Ok(Field(keys:))
    }
  }
}

type Fields = Dict(Field, List(String))

fn fields(form: FormData) -> Fields {
  form
  |> list.map(fn(field) {
    let #(key_str, val) = field

    key_str
    |> parse_field
    |> result.map(fn(key) { #(key, val) })
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

fn scalar(field: Field, decoder: Decoder(t), fields: Fields) -> Result(t, Nil) {
  use strs <- result.try(dict.get(fields, field))

  case strs {
    [str] ->
      decode.from(decoder, dynamic.from(str))
      |> result.replace_error(Nil)

    _ ->
      Error(Nil)
  }
}

fn array(field: Field, decoder: Decoder(t), fields: Fields) -> Result(List(t), Nil) {
  use strs <- result.try(dict.get(fields, field))

  decode.from(decode.list(decoder), dynamic.from(strs))
  |> result.replace_error(Nil)
}

pub fn decode_(form: FormData, decoder: Decoder(t)) -> Result(t, List(dynamic.DecodeError)) {
  decode.from(decoder, dynamic.from(dict.from_list(form)))
}

fn decode_string(
  into type_: String,
  using func: fn(String) -> Result(t, err),
) -> Decoder(t) {
  decode.string
  |> decode.then(fn(str) {
    case func(str) {
      Ok(x) -> decode.into(x)
      Error(_) -> decode.fail("Value is not an `" <> type_ <> "`: `\"" <> str <>  "\"`")
    }
  })
}

fn decoder_int() -> Decoder(Int) {
  decode_string(into: "Int", using: int.parse)
}

fn decoder_float() -> Decoder(Float) {
  decode_string(into: "Float", using: float.parse)
}

fn decoder_bool() -> Decoder(Bool) {
  decode.string
  |> decode.then(fn(str) {
    case string.lowercase(str) {
      "true" -> decode.into(True)
      "false" -> decode.into(False)
      _ -> decode.fail("Value is not a `Bool`: `\"" <> str <>  "\"`")
    }
  })
}

// fn decode_string_list() -> Decoder(List(String)) {
//   decode.into([])
// }

// pub fn decode_string_list_(field: String) -> Decoder(List(String)) {
//   decode.into([])
//   |> decode.then(decode_string_list_rec(_, field:, idx: 0))
//   |> decode.map(list.reverse)
// }

// fn decode_string_list_rec(
//   acc acc: List(String),
//   field field: String,
//   idx idx: Int,
// ) -> Decoder(List(String)) {
//   // let decoder_field =

//   case todo {
//     Error(_) -> decode.into(acc)
//     Ok(x) -> decode_string_list_rec([x, ..acc], field:, idx: idx+1)
//  }
// }

// pub fn decoder_person_form() -> Decoder(PersonForm) {
//   decode.into({
//     use name <- decode.parameter
//     use age <- decode.parameter
//     use kd <- decode.parameter
//     use active <- decode.parameter
//     use hobbies <- decode.parameter
//     PersonForm(name:, age:, kd:, active:, hobbies:)
//   })
//   |> decode.field("person[name]", decode.string)
//   |> decode.field("person[age]", decoder_int())
//   |> decode.field("person[kd]", decoder_float())
//   |> decode.field("person[active]", decoder_bool())
//   |> decode.field("person[hobbies]", decode_string_list())
// }

// pub fn decode_person_form_(form: FormData) -> Result(PersonForm, Nil) {
//   let fields = dynamic.from(fields(form))

//   let decoder: Decoder(PersonForm) =
//     decode.into({
//       // use name <- scalar_("person[name]", decode.string)
//       use name <- decode.parameter
//       // use age <- result.try(scalar("person[age]", decoder_int(), fields))
//       // use kd <- result.try(scalar("person[kd]", decoder_float(), fields))
//       // use active <- result.try(scalar("person[active]", decoder_bool(), fields))
//       // use hobbies <- result.try(array("person[hobbies][]", decode.string, fields))
//       // use name <- todo // scalar_("person[name]", decode.string, fields)
//       let age = 0
//       let kd = 0.0
//       let active = True
//       let hobbies = []

//       PersonForm(name:, age:, kd:, active:, hobbies:)
//     })
//     |> decode.field("", decode.string)

//   decode.from(decoder, fields)
//   |> result.replace_error(Nil)
// }

// type Scalar(t) {
//   Scalar(t)
// }
// fn scalar_(
//   key: String,
//   decoder: Decoder(t),
// ) -> Decoder(t) {
//   case parse_field(key) {
//     Error(_) -> decode.fail("") // TODO
//     Ok(field) ->
//       decode.into({
//         use val <- decode.parameter
//         Scalar(val)
//       })
//       |> decode.field(field, decoder)
//       |> decode.then(fn(x) {
//         let Scalar(val) = x
//         decode.into(val)
//       })
//   }
//   // use vals <- decode.field()
//   // use field <- result.try(parse_field(key) |> result.replace_error(Nil))
//   // use strs <- result.try(dict.get(fields, field))

//   // case strs {
//   //   [str] ->
//   //     decode.from(decoder, dynamic.from(str))
//   //     |> result.replace_error(Nil)

//   //   _ ->
//   //     Error(Nil)
//   // }
// }

pub fn suppress_option_warnings() -> List(Option(Nil)) { [None, Some(Nil)] }
