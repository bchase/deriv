import gleam/erlang/process
import bchase/io
import gleam/string
import gleam/dict
import deriv/gen/defs
import radiate

pub fn main() -> Nil {
  let assert Ok(_) =
    radiate.new()
    |> radiate.add_dir("src")
    |> radiate.start

  let self = process.new_subject()

  print(self, every: 1_000)
}

pub fn print(
  subj subj: process.Subject(Nil),
  every ms: Int,
) -> Nil {
  process.send_after(subj, ms, Nil)

  let _ = process.receive(subj, ms)

  io.println(defs.expr_gens() |> dict.keys |> string.inspect)

  print(subj, every: ms)
}
