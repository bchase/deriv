import gleam/result
import gleam/json.{type Json}
import gleam/option.{type Option}
import gleam/time/timestamp.{type Timestamp}
import deriv/gen/types.{type ExprGen} as x
import bchase/casing
import deriv/internal/glance as ast
import glance as g

pub fn handle_case() -> ExprGen {
  x.VariantClauseCaseExprGen(clauses: [{
    use var <- x.variant_name()
    let handler_func = ast.term("handle_" <> casing.snake(var))

    use req <- x.variant_shorthand_field("f")
    use req_type <- x.variant_shorthand_type("f")

    use req_type_param_types <- x.type_params(req_type)

    use #(
      _param_type_single_variant_all_labelled_fields,
      encode_func,
    ) <- x.try({case req_type_param_types {
      [
        #(g.NamedType(..), _param_type),
        #(g.NamedType(name: return_type, ..), _),
      ] -> {
        // let handler_args =
        //   case param_type |> result.map(fn(td) { td.def.definition }) {
        //     Ok(g.CustomType(variants: [variant], ..)) -> {
        //       let #(labelled_field_names, unlabelled_fields) =
        //         variant.fields
        //         |> list.map(fn(field) {
        //           case field {
        //             g.LabelledVariantField(label:, ..) -> Ok(label)
        //             g.UnlabelledVariantField(..) -> Error(Nil)
        //           }
        //         })
        //         |> result.partition

        //       case unlabelled_fields {
        //         [] ->
        //           []

        //         _all_fields_labelled ->
        //           labelled_field_names
        //       }
        //     }

        //     _ ->
        //       []
        //   }

        let encode_func = ast.term("encode_" <> casing.snake(return_type))

        Ok(#(Nil, encode_func))
      }

      _ ->
        Error(Nil)
    }})

    let resp_func = ast.term("resp_func")

    //

    handler_func
    |> ast.pipe({
      resp_func
      |> ast.call_([
        req,
        g.LabelledField("encode", encode_func)
      ])
    })
    |> x.variant_success()
  }])
}

//

pub type Id(resource) {
  Id(id: String)
}

pub type Record(resource) {
  Record(
    id: Id(resource),
    resource: resource,
    created_at: Timestamp,
    updated_at: Timestamp,
    archived_at: Option(Timestamp),
  )
}

pub type ConfirmDelete(resource) {
  ConfirmDelete
}

//

pub type Err {
  Err(msg: String)
}

pub type Resp {
  Resp(
    result: Result(Json, Err),
  )
}

// fn resp(
//   result: Result(t, Err),
//   encode: fn(t) -> Json,
// ) -> Resp {
//   result
//   |> result.map(encode)
//   |> Resp
// }

//

pub type Person {
  Person(
    name: String,
  )
}

pub type PersonForm {
  PersonForm(
    name: String,
  )
}

//

pub opaque type F(param, return) {
  F(param: param)
}

pub fn resp_func(
  handler handler: fn(param) -> Result(return, Err),
  f f: F(param, return),
  encode encode: fn(return) -> Json,
) -> Resp {
  f.param
  |> handler
  |> result.map(encode)
  |> Resp
}

//

fn encode_temp(temp: Temp) -> Json {
  todo
}

fn encode_distance(distance: Distance) -> Json {
  todo
}

//

pub type Req = Api
pub type Api {
  GetTemp(f: F(City, Temp))
  // GetAltitude(f: F(City, Distance))
}

pub fn handle(
  req req: Req,
) -> Resp {
  { //$ gen deriv/internal/dummy/api.handle_case req
    case req {
      GetTemp(f:) -> handle_get_temp |> resp_func(f:, encode: encode_temp)
    }
  }
}

// pub fn handle(
//   req req: Req,
// ) -> Resp {
//   case req {
//     GetTemp(f:) ->
//       handle_get_temp
//       |> resp_func(f:, encode: encode_temp)
//   }
// }

//

pub type City {
  Kyoto
}

pub type Temp {
  Celcius(degrees: Int)
}

pub type Distance {
  Meters(meters: Float)
}

fn handle_get_temp(
  city: City,
) -> Result(Temp, Err) {
  case city {
    Kyoto ->
      Ok(Celcius(degrees: 38))
  }
}

 fn handle_get_altitude(
  city: City,
) -> Result(Distance, Err) {
  todo
}

//

// pub fn handle(
//   req req: Req,
// ) -> Resp {
//   case req {
//     PeopleList ->
//       handle_people_list()
//       |> resp(json.array(_, encode_record(_, encode_person)))

//     PeopleCreate(form:) ->
//       handle_people_create(form:)
//       |> resp(encode_record(_, encode_person))

//     PeopleRead(id:) ->
//       handle_people_read(id:)
//       |> resp(encode_record(_, encode_person))

//     PeopleUpdate(id:, form:) ->
//       handle_people_update(id:, form:)
//       |> resp(encode_record(_, encode_person))

//     PeopleDelete(id:, confirm:) ->
//       handle_people_delete(id:, confirm:)
//       |> resp(encode_record(_, encode_person))
//   }
// }

// pub fn handle_people_list(
// ) -> Result(List(Record(Person)), Err) {
//   todo
// }

// pub fn handle_people_create(
//   form form: PersonForm,
// ) -> Result(Record(Person), Err) {
//   todo
// }

// pub fn handle_people_read(
//   id id: Id(Person),
// ) -> Result(Record(Person), Err) {
//   todo
// }

// pub fn handle_people_update(
//   id id: Id(Person),
//   form form: PersonForm,
// ) -> Result(Record(Person), Err) {
//   todo
// }

// pub fn handle_people_delete(
//   id id: Id(Person),
//   confirm confirm: ConfirmDelete(Person),
// ) -> Result(Record(Person), Err) {
//   todo
// }

// //

// fn encode_record(
//   record: Record(resource),
//   encode: fn(resource) -> Json,
// ) -> Json {
//   todo
// }

// fn encode_person(person: Person) -> Json {
//   todo
// }

// //

// pub type Req {
//   PeopleList
//   PeopleCreate(form: PersonForm)
//   PeopleRead(id: Id(Person))
//   PeopleUpdate(id: Id(Person), form: PersonForm)
//   PeopleDelete(id: Id(Person), confirm: ConfirmDelete(Person))
// }

// //

// pub fn handle(
//   req req: Req,
// ) -> Resp {
//   case req {
//     PeopleList ->
//       handle_people_list()
//       |> resp(json.array(_, encode_record(_, encode_person)))

//     PeopleCreate(form:) ->
//       handle_people_create(form:)
//       |> resp(encode_record(_, encode_person))

//     PeopleRead(id:) ->
//       handle_people_read(id:)
//       |> resp(encode_record(_, encode_person))

//     PeopleUpdate(id:, form:) ->
//       handle_people_update(id:, form:)
//       |> resp(encode_record(_, encode_person))

//     PeopleDelete(id:, confirm:) ->
//       handle_people_delete(id:, confirm:)
//       |> resp(encode_record(_, encode_person))
//   }
// }

// pub fn handle_people_list(
// ) -> Result(List(Record(Person)), Err) {
//   todo
// }

// pub fn handle_people_create(
//   form form: PersonForm,
// ) -> Result(Record(Person), Err) {
//   todo
// }

// pub fn handle_people_read(
//   id id: Id(Person),
// ) -> Result(Record(Person), Err) {
//   todo
// }

// pub fn handle_people_update(
//   id id: Id(Person),
//   form form: PersonForm,
// ) -> Result(Record(Person), Err) {
//   todo
// }

// pub fn handle_people_delete(
//   id id: Id(Person),
//   confirm confirm: ConfirmDelete(Person),
// ) -> Result(Record(Person), Err) {
//   todo
// }

// //

// fn encode_record(
//   record: Record(resource),
//   encode: fn(resource) -> Json,
// ) -> Json {
//   todo
// }

// fn encode_person(person: Person) -> Json {
//   todo
// }
