import gleam/io
import gleam/string
import gleam/result.{try}
import simplifile
import glance

pub fn main() {
  let file_path = "./glance_dump_target.gleam"

  case {
    use src <- try(simplifile.read(file_path) |> result.map_error(string.inspect))
    use module <- try(glance.module(src) |> result.map_error(string.inspect))

    module
    |> string.inspect
    |> io.println
    |> Ok
  } {
    Error(err) -> io.println(err)
    Ok(_) -> Nil
  }
}
