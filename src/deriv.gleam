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
import deriv/types.{type File, File, type Output, Output, type Write, Write, type GenFunc, type Import, Import, type Gen, Gen, type Derivation, Derivation, type DerivFieldOpt}
import deriv/parser
import deriv/json
import gleam/io

const all_gen_funcs: List(#(String, GenFunc)) =
  [
    #("json", json.gen),
  ]

pub fn main() {
  let filepaths = find_project_src_gleam_filepaths()

  filepaths
  |> load_files
  |> gen_derivs
  |> build_writes
  |> perform_file_writes
}

fn find_project_src_gleam_filepaths() -> List(String) {
  let assert Ok(output) = shellout.command(in: ".", run: "find", with: ["src", "-name", "*.gleam"], opt: [])

  output
  |> string.trim
  |> string.split("\n")
}

// GEN DERIVS

fn load_files(filepaths: List(String)) -> List(File) {
  filepaths
  |> list.index_map(fn(path, idx) {
    read_file(path, idx)
  })
}

pub fn gen_derivs(files: List(File)) -> List(Gen) {
  let gen_funcs = all_gen_funcs |>  dict.from_list

  files
  |> list.map(fn(file) {
    file
    |> parse_types_and_derivations
    |> list.flat_map(gen_type_derivs(_, file, gen_funcs))
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

fn parse_types_and_derivations(file: File) -> List(#(CustomType, List(Derivation), Dict(String, List(DerivFieldOpt)))) {
  let assert Ok(parsed) = glance.module(file.src)

  parsed.custom_types
  |> list.map(fn(ct) { ct.definition })
  |> list.map(parser.parse_type_with_derivations(_, file.src))
  |> result.values
}

fn gen_type_derivs(
  x: #(CustomType, List(Derivation), Dict(String, List(DerivFieldOpt))),
  file: File,
  gen_funcs: Dict(String, GenFunc),
) -> List(Gen) {
  let #(type_, derivs, field_opts) = x

  derivs
  |> list.map(fn(d) {
    case dict.get(gen_funcs, d.name) {
      Error(_) -> Error(Nil)
      Ok(f) -> {
        Ok(f(type_, d, field_opts, file))
      }
    }
  })
  |> result.values
}

// WRITE TO FILES

pub fn build_writes(xs: List(Gen)) -> List(Write) {
  xs
  |> list.group(fn(gen) {
    Output(module: gen.file.module, deriv: gen.deriv.name)
  })
  |> dict.map_values(fn(output, gens) {
    let output_path = output_path(output)
    let output_src = build_output_src(gens)

    Write(
      filepath: output_path,
      src: output_src,
      output: output,
    )
  })
  |> dict.values
}

fn perform_file_writes(xs: List(Write)) -> Nil {
  xs
  |> list.each(fn(write) {
    // case string.ends_with(write.filepath, "mvar/json.gleam") {
    //   False -> Nil
    //   True -> {
    //     io.println("// " <> write.filepath)
    //     io.println(write.src)
    //   }
    // }

    let dir = string.replace(write.filepath, {write.output.deriv <> ".gleam"}, "")
    let assert Ok(_) = simplifile.create_directory_all(dir)
    let assert Ok(_) = simplifile.write(write.filepath, write.src)
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
  let deriv_imports =
    gens
    |> list.flat_map(fn(gen) { gen.imports })
    |> imports_src

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
        case list.unique(aliases) {
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

pub fn stop_warning() { io.debug("") }
