import bchase/io
import glance_printer
import gleam/string
import gleam/option.{Some, None}
import gleam/result.{try}
import gleam/dict
import gleam/list
import shellout
import glance as g
import deriv/gen/types.{type GleamPath, GleamPath, type GleamFile}
import deriv/internal/gen.{type ExprGen}
import deriv/internal/glance.{z, call, term, dot, format_gleam_expr} as _

pub fn foo() -> ExprGen {
  todo
}

pub fn main() -> Nil {
  let assert Ok(output) = shellout.command(in: ".", opt: [],  run: "find", with: ["src/"])
  let filepaths = output |> string.split("\n")

  let files =
    filepaths
    |> list.map(gen.load_gleam_file)
    |> result.values
    |> list.filter_map(fn(file) {
      let funcs = has_reference(in: file, module: "deriv/internal/gen", type_: "ExprGen")
      case funcs {
        [] -> Error(Nil)
        _ -> Ok(#(
          file.path.full |> string.join("/"),
          funcs |> list.map(fn(func) { func.definition.name }),
        ))
      }
    })
    |> list.map(fn(t) {
      let #(module, func_names) = t
      let alias = module |> string.replace("/", "_")
      #(
        g.Import(z, module:, alias: Some(g.Named(alias)), unqualified_types: [], unqualified_values: []),
        func_names |> list.map(fn(func) { alias |> dot(func) })
      )
    })
    |> list.each(fn(t) {
      let #(import_, funcs) = t

      g.Module([g.Definition([], import_)], [], [], [], [])
      |> glance_printer.print
      |> string.trim
      |> io.println

      funcs
      |> list.each(fn(func) {
        func
        |> format_gleam_expr(indent: 2)
        |> io.println
      })

      io.println("")
    })

  Nil
}

fn has_reference(
  in file: GleamFile,
  module target_module: String,
  type_ target_type: String,
) -> List(g.Definition(g.Function)) {
  file.ast.functions
  |> dict.values
  |> list.filter_map(fn(func) {
    use type_ <- try(func.definition.return |> option.to_result(Nil))

    use #(mod, type_) <- try(case type_ {
      g.NamedType(module:, name:, ..) -> Ok(#(module, name))
      _ -> Error(Nil)
    })

    use import_ <- try(case mod {
      Some(mod) ->
        file.ast.imports.named
        |> dict.get(mod)

      None ->
        file.ast.imports
        |> types.all_imports
        |> list.find(fn(import_) {
          import_.definition.unqualified_types
          |> list.any(fn(type_) {
            type_.name == target_type
          })
        })
    })

    case import_.definition.module == target_module && type_ == target_type {
      True -> Ok(func)
      False -> Error(Nil)
    }
  })
}
