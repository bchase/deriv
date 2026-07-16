import gleam/string
import gleam/result
import gleam/json.{type Json}
import gleam/option.{type Option, Some, None}
import gleam/time/timestamp.{type Timestamp}
import deriv/gen/types.{type ExprGen} as x
import bchase/casing
import deriv/internal/glance.{z} as ast
import deriv/gen/types
import glance as g

pub fn handle_case() -> ExprGen {
  x.VariantClauseCaseExprGen(clauses: [{
    types.failure("fix")
  }])

  // x.VariantClauseCaseExprGen(clauses: [{
  //   use var <- x.variant_name()
  //   let handler_func_name = "handle_" <> casing.snake(var)
  //   let handler_func = ast.term(handler_func_name)

  //   use req <- x.variant_shorthand_field("f")
  //   use req_type <- x.variant_shorthand_type("f")

  //   use req_type_param_types <- x.type_params(req_type)

  //   use #(
  //     param_type,
  //     return_type,
  //     encode_func,
  //   ) <- x.try({case req_type_param_types {
  //     [
  //       #(g.NamedType(..) as param_type, _param_custom_type),
  //       #(g.NamedType(name: return_type_name, ..) as return_type, _),
  //     ] -> {
  //       let encode_func = ast.term("encode_" <> casing.snake(return_type_name))

  //       Ok(#(param_type, return_type, encode_func))
  //     }

  //     _ ->
  //       Error("needs `NamedType` for req/resp types, but got: " <> string.inspect(req_type_param_types))
  //   }})

  //   let resp_func = ast.term("resp_func")

  //   use <- x.ensure_func(g.Definition([], g.Function(z,
  //     name: handler_func_name,
  //     publicity: g.Private,
  //     parameters: [
  //       g.FunctionParameter(
  //         label: None,
  //         name: g.Named("req"),
  //         type_: Some(param_type),
  //       ),
  //     ],
  //     return: Some(g.NamedType(z, "Result", None, [
  //       return_type,
  //       g.NamedType(z, "Err", None, []),
  //     ])),
  //     body: [
  //       // ast.term("Nil") |> g.Expression,
  //       g.Todo(z, None) |> g.Expression,
  //     ],
  //   )))

  //   //

  //   handler_func
  //   |> ast.pipe({
  //     resp_func
  //     |> ast.call_([
  //       req,
  //       g.LabelledField("encode", encode_func)
  //     ])
  //   })
  //   |> x.variant_success()
  // }])
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

pub type City {
  Kyoto
}

pub type Temp {
  Celcius(degrees: Int)
}

pub fn encode_temp(_temp: Temp) -> Json { todo }

pub type Distance {
  Meters(meters: Float)
}

pub fn encode_distance(_distance: Distance) -> Json { todo }

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

fn handle_get_temp(req: City) -> Result(Temp, Err) {
  todo
}
