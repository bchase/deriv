import deriv/gen/types.{type ExprGen}
import deriv/internal/dummy/api as deriv_internal_dummy_api
import deriv/internal/dummy/gen/after as deriv_internal_dummy_gen_after
import deriv/internal/dummy/gen/before as deriv_internal_dummy_gen_before
import deriv/internal/dummy/lookup as deriv_internal_dummy_lookup
import gleam/dict.{type Dict}

pub fn expr_gens() -> Dict(#(String, String), ExprGen) {
  [
    #(
      #("deriv/internal/dummy/api", "handle_case"),
      deriv_internal_dummy_api.handle_case(),
    ),
    #(
      #("deriv/internal/dummy/gen/after", "gen"),
      deriv_internal_dummy_gen_after.gen(),
    ),
    #(
      #("deriv/internal/dummy/gen/before", "gen"),
      deriv_internal_dummy_gen_before.gen(),
    ),
    #(
      #("deriv/internal/dummy/lookup", "to_str"),
      deriv_internal_dummy_lookup.to_str(),
    ),
  ]
  |> dict.from_list
}
