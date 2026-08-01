import bchase/casing
import deriv/gen/types.{type ExprGen} as x
import deriv/internal/dummy/api.{type City, type Distance, type Err, type Resp, type Temp, resp_func}
import deriv/internal/glance.{z} as ast
import glance as g
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/time/timestamp.{type Timestamp}

pub fn handle_case() -> ExprGen {
  x.VariantClauseCaseExprGen(clauses: [{
    use var, _type_def <- x.variant()
    let handler_func_name = "handle_" <> casing.snake(var.name)
    let handler_func = ast.term(handler_func_name)

    use req, req_type, _req_type_def <- x.pun_variant_named_param("f")

    use req_type_param_types <- x.type_params(req_type)

    use #(
      param_type,
      return_type,
      encode_func,
    ) <- x.try({case req_type_param_types {
      [
        #(g.NamedType(..) as param_type, _param_custom_type),
        #(g.NamedType(name: return_type_name, ..) as return_type, _),
      ] -> {
        let encode_func =
          "api" |> ast.dot("encode_" <> casing.snake(return_type_name))

        x.success(#(param_type, return_type, encode_func))
      }

      _ ->
        x.failure("needs `NamedType` for req/resp types, but got: " <> string.inspect(req_type_param_types))
    }})

    let resp_func = ast.term("resp_func")

    use <- x.ensure_func(g.Definition([], g.Function(z,
      name: handler_func_name,
      publicity: g.Private,
      parameters: [
        g.FunctionParameter(
          label: None,
          name: g.Named("req"),
          type_: Some(param_type),
        ),
      ],
      return: Some(g.NamedType(z, "Result", None, [
        return_type,
        g.NamedType(z, "Err", None, []),
      ])),
      body: [
        // ast.term("Nil") |> g.Expression,
        g.Todo(z, None) |> g.Expression,
      ],
    )))

    //

    handler_func
    |> ast.pipe({
      resp_func
      |> ast.call_([
        req,
        g.LabelledField("encode", encode_func)
      ])
    })
    |> x.variant_clause_success
  }])
}

pub fn handle(
  req req: api.Req,
) -> Resp {
  { //$ deriv/internal/dummy/server.handle_case subject:req
    case req {
      api.GetTemp(f:) -> handle_get_temp |> resp_func(
        f:,
        encode: api.encode_temp,
      )
      api.GetAltitude(f:) -> handle_get_altitude |> resp_func(
        f:,
        encode: api.encode_distance,
      )
    }
  }
}

fn handle_get_altitude(req: City) -> Result(Distance, Err) {
  todo
}

fn handle_get_temp(req: City) -> Result(Temp, Err) {
  todo
}