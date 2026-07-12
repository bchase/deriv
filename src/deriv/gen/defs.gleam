import deriv/gen/types.{type ExprGen}
import deriv/internal/dummy/gen/after as deriv_internal_dummy_gen_after
import deriv/internal/dummy/gen/before as deriv_internal_dummy_gen_before
import gleam/dict.{type Dict}

pub fn expr_gens() -> Dict(#(String, String), ExprGen) {
  [
    #(
      #("deriv/internal/dummy/gen/after", "gen"),
      deriv_internal_dummy_gen_after.gen(),
    ),
    #(
      #("deriv/internal/dummy/gen/before", "gen"),
      deriv_internal_dummy_gen_before.gen(),
    ),
  ]
  |> dict.from_list
}
