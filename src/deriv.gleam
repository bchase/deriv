import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import glance.{type CustomType}
import gleam/regexp
import simplifile
import shellout
import deriv/types.{type File, File, type Output, Output, type GenFunc, type Imports, type Import, Import, type Gen, Gen, type Derivation, Derivation}
import deriv/parser
import deriv/json
import gleam/io

const all_gen_funcs: List(#(String, GenFunc)) =
  [
    #("json", json.gen),
  ]

const all_deriv_imports: List(Imports) =
  [
    json.imports,
  ]

pub fn main() {
  let filepaths = find_project_src_gleam_filepaths()

  filepaths
  |> gen_derivs()
  |> write_to_files()
}

fn find_project_src_gleam_filepaths() -> List(String) {
  let assert Ok(output) = shellout.command(in: ".", run: "find", with: ["src", "-name", "*.gleam"], opt: [])

  output
  |> string.trim
  |> string.split("\n")
}

// GEN DERIVS

fn gen_derivs(filepaths: List(String)) -> List(Gen) {
  let gen_funcs = all_gen_funcs |>  dict.from_list

  filepaths
  |> list.index_map(fn(path, idx) {
    let file = read_file(path, idx)

    file
    |> parse_types_and_derivations
    |> gen_type_derivs(file, gen_funcs)
  })
  |> list.flatten
}

fn read_file(filepath: String, idx: Int) -> File {
  let assert Ok(src) = simplifile.read(filepath)
  let module = file_path_to_gleam_module_str(filepath)
  File(module: , src:, idx: idx+1)
}

fn file_path_to_gleam_module_str(path: String) -> String {
  let assert Ok(leading_src_slash) = regexp.from_string("^src[/]")
  let assert Ok(trailing_dot_gleam) = regexp.from_string("[.]gleam$")

  path
  |> regexp.replace(each: leading_src_slash, in: _, with: "")
  |> regexp.replace(each: trailing_dot_gleam, in: _, with: "")
}

fn parse_types_and_derivations(file: File) -> List(#(CustomType, List(Derivation))) {
  let assert Ok(parsed) = glance.module(file.src)

  parsed.custom_types
  |> list.map(fn(ct) { ct.definition })
  |> list.map(parser.parse_type_with_derivations(_, file.src))
  |> result.values
}

fn gen_type_derivs(
  xs: List(#(CustomType, List(Derivation))),
  file: File,
  gen_funcs: Dict(String, GenFunc),
) -> List(Gen) {
  xs
  |> list.flat_map(gen_type_derivs_(_, file, gen_funcs))
}

fn gen_type_derivs_(
  x: #(CustomType, List(Derivation)),
  file: File,
  gen_funcs: Dict(String, GenFunc),
) -> List(Gen) {
  let #(type_, derivs) = x

  derivs
  |> list.map(fn(d) {
    case dict.get(gen_funcs, d.name) {
      Error(_) -> Error(Nil)
      Ok(f) -> {
        let src = f(type_, d.opts, file)
        Ok(Gen(file:, deriv: d, src:))
      }
    }
  })
  |> result.values
}

// WRITE TO FILES

fn write_to_files(xs: List(Gen)) -> Nil {
  xs
  |> list.group(fn(gen) {
    Output(module: gen.file.module, deriv: gen.deriv.name)
  })
  |> dict.each(fn(output, gens) {
    let output_path = output_path(output)
    let output_src = build_output_src(gens)

    io.println("// " <> output_path)
    io.println(output_src)

    // let dir = string.replace(output_path, {output.deriv <> ".gleam"}, "")
    // let assert Ok(_) = simplifile.create_directory_all(dir)
    // let assert Ok(_) = simplifile.write(output_path, output_src)
  })
}

fn output_path(output: Output) -> String {
  [
    "src",
    "deriv",
    output.module,
    output.deriv <> ".gleam",
  ]
  |> string.join("/")
}

fn build_output_src(gens: List(Gen)) -> String {
  let module_imports = build_module_imports(gens)
  let deriv_imports = build_deriv_imports(gens)

  let defs = list.map(gens, fn(gen) { gen.src })

  [
    module_imports,
    deriv_imports,
  ]
  |> list.filter(fn(str) { str != "" })
  |> string.join("\n")
  |> fn(imports) { [imports] }
  |> list.append(defs)
  |> string.join("\n\n")
}

fn build_module_imports(gens: List(Gen)) -> String {
  let files =
    gens
    |> list.map(fn(g) { g.file })
    |> list.unique

  list.map(files, fn(file) {
    "import MODULE as mINDEX"
    |> string.replace(each: "MODULE", with: file.module)
    |> string.replace(each: "INDEX", with: file.idx |> int.to_string)
  })
  |> string.join("\n")
}

fn build_deriv_imports(gens: List(Gen)) -> String {
  let derivs = list.map(gens, fn(g) { g.deriv })

  let deriv_imports =
    all_deriv_imports
    |> list.flatten
    |> dict.from_list

  derivs
  |> list.flat_map(fn(d) {
    list.map(d.opts, fn(opt) {
      #(d.name, opt)
    })
  })
  |> list.unique
  |> list.map(dict.get(deriv_imports, _))
  |> result.values
  |> list.map(string.trim)
  |> fn(strs) {
    case strs {
      [] -> Error(Nil)
      _ -> Ok(string.join(strs, "\n"))
    }
  }
  |> result.unwrap("")
}

fn consolidate_imports(all_imports: List(Import)) -> List(Import) {
  all_imports
  |> list.group(fn(i) { i.module })
  |> dict.to_list
  |> list.map(fn(x) {
    let #(module, imports) = x

    let alias =
      imports
      |> list.map(fn(i) { i.alias })
      |> option.values
      |> fn(aliases) {
        case aliases {
          [] -> None
          [alias] -> Some(alias)
          _ -> panic as {
            [
              { "0 or 1 allowed, but for module `" <> module <> "` multiple module aliases found :" },
              ..aliases // TODO trailing "," here blows up `glance` but not gleam compiler
            ]
            |> string.join(" ")
          }
        }
      }

    let types =
      imports
      |> list.flat_map(fn(i) { i.types })
      |> list.unique

    let constructors =
      imports
      |> list.flat_map(fn(i) { i.constructors })
      |> list.unique

    Import(
      module:,
      types:,
      constructors:,
      alias:,
    )
  })
}

fn import_src(i: Import) -> String {
  let alias =
    case i.alias {
      None -> ""
      Some(name) -> "as " <> name
    }

  let types =
    i.types
    |> list.map(fn(t) { "type " <> t })

  let types_and_constructors =
    case list.append(types, i.constructors) {
      [] -> ""
      xs -> {
        let str = string.join(xs, ", ")

        ".{" <> str <> "}"
      }
    }

  let module_with_types_and_constructors =
    i.module <> types_and_constructors

  [
    "import",
    module_with_types_and_constructors,
    alias,
  ]
  |> list.filter(fn(str) { str != "" })
  |> string.join(" ")
}

fn imports_src(imports: List(Import)) -> String {
  imports
  |> consolidate_imports
  |> list.map(import_src)
  |> string.join("\n")
}
