import gleam/erlang/process
import gleam/bool
import simplifile
import deriv/internal/common
import gleam/pair
import bchase/io
import glance_printer
import gleam/string
import gleam/option.{Some, None}
import gleam/result.{try}
import gleam/dict
import gleam/list
import shellout
import glance as g
import deriv/gen/types.{type GleamFile}
import deriv/internal/gen
import deriv/internal/glance.{z, call, dot, pipe} as _
import radiate
import deriv/gen/defs

const func_name = "expr_gens"

pub fn main() -> Nil {
  let assert Ok(output) = shellout.command(in: ".", opt: [],  run: "find", with: ["src/"])
  let filepaths = output |> string.split("\n")

  update_gen_defs_gleam_file(filepaths:)
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

pub fn update_gen_defs_gleam_file(
  filepaths filepaths: List(String),
) -> Nil {
  filepaths
  |> list.map(gen.load_gleam_file)
  |> result.values
  |> list.filter_map(fn(file) {
    let funcs = has_reference(in: file, module: "deriv/gen/types", type_: "ExprGen")
    case funcs {
      [] ->
        Error(Nil)

      _ -> Ok(#(
        file.path.full |> string.join("/"),
        funcs |> list.map(fn(func) { func.definition.name }),
      ))
    }
  })
  |> list.map(fn(t) {
    let #(module, func_names) = t
    let alias = module |> string.replace("/", "_")

    let func_tuple_expr =
      func_names
      |> list.map(fn(func) {
        g.Tuple(z, [
          g.Tuple(z, [
            g.String(z, module),
            g.String(z, func),
          ]),
          alias |> dot(func) |> call([]),
        ])
      })

    #(
      g.Import(z, module:, alias: Some(g.Named(alias)), unqualified_types: [], unqualified_values: []),
      func_tuple_expr,
    )
  })
  |> list.unzip
  |> pair.map_second(list.flatten)
  |> fn(t) {
    let #(imports, func_exprs) = t

    let imports =
      imports
      |> list.map(g.Definition([], _))
      |> list.append([
        g.Import(z, "deriv/gen/types", None, [
          g.UnqualifiedImport("ExprGen", None),
        ], [])
        |> g.Definition([], _),
        g.Import(z, "gleam/dict", None, [
          g.UnqualifiedImport("Dict", None),
        ], [])
        |> g.Definition([], _),
      ])

    let func = g.Definition([], g.Function(z,
      name: func_name,
      publicity: g.Public,
      parameters: [],
      return: Some(g.NamedType(z,
        "Dict", None, [
          g.TupleType(z, [
            g.NamedType(z, "String", None, []),
            g.NamedType(z, "String", None, []),
          ]),
          g.NamedType(z, "ExprGen", None, []),
        ]
      )),
      body: [
        g.Expression(
          g.List(z, func_exprs, None)
          |> pipe("dict" |> dot("from_list"))
        ),
      ],
    ))

    let src =
      g.Module(imports, [], [], [], [func])
      |> glance_printer.print
      |> common.gleam_format

    src
    |> io.println

    let assert Ok(_) = simplifile.write("src/deriv/gen/defs.gleam", src)

    Nil
  }
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
    use <- bool.guard(func.definition.publicity != g.Public, Error(Nil))

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
