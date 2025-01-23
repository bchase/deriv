import gleam/option.{Some, None}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import glance.{type CustomType, type Import, Import}
import gleam/regexp
import simplifile
import shellout
import tom
import deriv/types.{type File, File, type Output, Output, OutputInline, type Write, Write, type GenFunc, type Gen, Gen, type Derivation, Derivation, type DerivFieldOpt}
import deriv/parser
import deriv/json as deriv_json
import deriv/util
import gleam/io

import decode as d
import gleam/json
// import deriv/deriv/example/mvar/json as mvar

const all_gen_funcs: List(#(String, GenFunc)) =
  [
    #("json", deriv_json.gen),
  ]

pub fn main() {
  // let r1 =
  //   [
  //     "NOPE",
  //     "{\"var1\":\"yeet\"}",
  //   ]
  //   |> list.map(fn(str) {
  //     json.decode(from: str, using: d.from(mvar.d_t(), _))
  //     |> io.debug
  //   })

  // panic as "DONE"

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

fn project_name() -> String {
  let assert Ok(output) = shellout.command(in: ".", run: "cat", with: ["gleam.toml"], opt: [])
  let assert Ok(config) = tom.parse(output)

  case tom.get_string(config, ["name"]) {
    Error(_) -> panic as "Cannot determine project name from `gleam.toml`"
    Ok(name) -> name
  }
}

pub fn gen_derivs(files: List(File)) -> List(Gen) {
  let gen_funcs = all_gen_funcs |>  dict.from_list

  let project_derivs_module_name = project_name() <> "/derivs"

  let type_inline_gens =
    files
    |> list.filter(fn(file) {
      file.module != project_derivs_module_name
    })
    |> list.map(fn(file) {
      let file = File(..file, idx: None)

      file
      |> parse_types_and_derivations
      |> list.flat_map(gen_type_derivs(_, file, gen_funcs))
    })
    |> list.flatten
    |> list.map(fn(gen) {
      Gen(..gen, meta: dict.from_list([#("source", "inline")]))
    })

  let derivs_file =
    list.find(files, fn(file) {
      file.module == project_derivs_module_name
    })

  let derivs_file_import_gens =
    case derivs_file {
      Ok(file) -> derivs_file_import_gens(file, files, gen_funcs)
      _ -> []
    }
    |> list.map(fn(gen) {
      Gen(..gen, meta: dict.from_list([#("source", "import")]))
    })

  [
    type_inline_gens,
    derivs_file_import_gens,
  ]
  |> list.flatten
}

fn read_file(filepath: String, idx: Int) -> File {
  let assert Ok(src) = simplifile.read(filepath)
  let module = file_path_to_gleam_module_str(filepath)
  File(module: , src:, idx: Some(idx+1))
}

fn file_path_to_gleam_module_str(path: String) -> String {
  let assert Ok(leading_src_slash) = regexp.from_string("^src[/]")
  let assert Ok(trailing_dot_gleam) = regexp.from_string("[.]gleam$")

  path
  |> regexp.replace(each: leading_src_slash, in: _, with: "")
  |> regexp.replace(each: trailing_dot_gleam, in: _, with: "")
}

fn gleam_module_str_to_file_path(module: String) -> String {
  "src/" <> module <> ".gleam"
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
  let #(inline_derivs, other_derivs) =
    xs
    |> list.partition(fn(gen) {
      gen.meta
      |> dict.get("source")
      |> result.map(fn(source) { source == "inline" })
      |> result.unwrap(False)
    })

  let same_file_writes =
    inline_derivs
    |> build_same_file_writes

  let diff_file_writes =
    other_derivs
    |> build_different_file_writes

  [
    diff_file_writes,
    same_file_writes,
  ]
  |> list.flatten
}

pub fn build_same_file_writes(xs: List(Gen)) -> List(Write) {
  xs
  |> list.group(fn(gen) {
    gen.file.module
  })
  |> dict.map_values(fn(module, gens) {
    let orig_src =
      gens
      |> list.first
      |> result.map(fn(gen) { gen.file.src })
      |> fn(r) {
        case r {
          Ok(src) -> src
          Error(_) -> panic as { "`File.src` could not be found for `" <> module <> "`" }
        }
      }

    let output_path = gleam_module_str_to_file_path(module)
    let output = OutputInline(module: module, filepath: output_path)

    let funcs =
      gens
      |> list.flat_map(fn(gen) { gen.funcs })
      |> list.map(fn(func) { #(func.name, func.src) })

    let module_imports: List(Import) = build_module_imports(gens, output)
    let deriv_imports: List(Import) = list.flat_map(gens, fn(gen) { gen.imports })
    let all_imports: List(Import) = [module_imports, deriv_imports] |> list.flatten

    let func_src_with_imports = util.update_funcs(orig_src, funcs)

    let output_src =
      func_src_with_imports
      |> consolidate_imports_for(all_imports)
      |> string.trim

    Write(
      filepath: output_path,
      src: output_src,
      output: output,
    )
  })
  |> dict.values
}

pub fn build_different_file_writes(xs: List(Gen)) -> List(Write) {
  xs
  |> list.group(fn(gen) {
    Output(module: gen.file.module, deriv: gen.deriv.name)
  })
  |> dict.map_values(fn(output, gens) {
    let output_path = output_path(output)
    let output_src =
      gens
      |> build_output_src(output)
      |> string.trim

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
    case write.output {
      Output(deriv:, ..) -> {
        let dir = string.replace(write.filepath, {deriv <> ".gleam"}, "")
        let assert Ok(_) = simplifile.create_directory_all(dir)

        Nil
      }

      _ -> Nil
    }

    let assert Ok(_) = simplifile.write(write.filepath, write.src)
  })
}

fn output_path(output: Output) -> String {
  case output {
    Output(module:, deriv:) ->
      [
        "src",
        "deriv",
        module,
        deriv <> ".gleam",
      ]
      |> string.join("/")

    OutputInline(filepath:, ..) ->
      filepath
  }
}

fn build_output_src(gens: List(Gen), output: Output) -> String {
  case output {
    Output(..) -> {
      let module_imports = build_module_imports(gens, output)
      let deriv_imports = list.flat_map(gens, fn(gen) { gen.imports })

      let defs = list.map(gens, fn(gen) { gen.src })

      let func_src = string.join(defs, "\n\n")

      let all_imports =
      [
        module_imports,
        deriv_imports,
      ]
      |> list.flatten

      consolidate_imports_for(func_src, all_imports)
    }

    OutputInline(..) -> {
      // TODO don't think this logic is ever run...
      panic as "don't think this logic ever runs..."
      // let module_imports = build_module_imports(gens, output)
      // let deriv_imports = list.flat_map(gens, fn(gen) { gen.imports })

      // let defs = list.map(gens, fn(gen) { gen.src })

      // [
      //   module_imports,
      //   deriv_imports,
      // ]
      // |> string.join("\n")
      // |> fn(imports) { [imports] }
      // |> list.append(defs)
      // |> string.join("\n\n")
    }
  }
}

fn build_module_imports(gens: List(Gen), _output: Output) -> List(Import) {
  let files =
    gens
    |> list.map(fn(g) { g.file })
    |> list.unique

  list.flat_map(files, fn(file) {
    case file.idx {
      Some(idx) ->
        Import(
          module: file.module,
          alias: Some(glance.Named("m" <> int.to_string(idx))),
          unqualified_types: [],
          unqualified_values: [],
        )
        // "import MODULE as mINDEX"
        // |> string.replace(each: "MODULE", with: file.module)
        // |> string.replace(each: "INDEX", with: idx |> int.to_string)
        |> fn(x) { [x] }

      None ->
        []
    }
  })
}

pub fn consolidate_imports_for(src: String, add add_imports: List(Import)) -> String {
  let assert Ok(module) = glance.module(src)

  let curr_imports =
    module.imports
    |> list.map(fn(d) { d.definition })

  let new_imports =
    [ curr_imports, add_imports ]
    |> list.flatten
    |> consolidate_imports
    |> list.map(import_src)
    |> string.join("\n")
    |> string.trim

  let src_without_imports =
    src
    |> string.split("\n")
    |> list.reverse
    |> list.take_while(fn(str) { !string.starts_with(str, "import") })
    |> list.reverse
    |> string.join("\n")
    |> string.trim

  [
    new_imports,
    src_without_imports,
  ]
  |> string.join("\n\n")
}

pub fn consolidate_imports(all_imports: List(Import)) -> List(Import) {
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
            io.debug(aliases)
            "0 or 1 aliases allowed, but for module `" <> module <> "` multiple aliases found (see above)"
          }
        }
      }

    let unqualified_types =
      imports
      |> list.flat_map(fn(i) { i.unqualified_types })
      |> list.unique

    let unqualified_values =
      imports
      |> list.flat_map(fn(i) { i.unqualified_values })
      |> list.unique

    Import(
      module:,
      alias:,
      unqualified_types:,
      unqualified_values:,
    )
  })
  |> list.sort(fn(a,b) { string.compare(a.module, b.module) })
}

fn unqualified_import_str(uqi: glance.UnqualifiedImport,  is_type is_type: Bool) -> String {
  let type_ =
    case is_type {
      True -> "type "
      False -> ""
    }

  let alias =
    case uqi.alias {
      Some(alias) -> " as " <> alias
      None -> ""
    }

  type_ <> uqi.name <> alias
}

fn import_src(i: Import) -> String {
  let alias =
    case i.alias {
      Some(glance.Named(name)) -> "as " <> name
      Some(glance.Discarded(name)) -> "as _" <> name
      None -> ""
    }

  let types =
    i.unqualified_types
    |> list.sort(fn(a,b) { string.compare(a.name, b.name) })
    |> list.map(unqualified_import_str(_, is_type: True))

  let funcs =
    i.unqualified_values
    |> list.sort(fn(a,b) { string.compare(a.name, b.name) })
    |> list.map(unqualified_import_str(_, is_type: False))

  let types_and_constructors =
    case list.append(types, funcs) {
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


///// ///// ///// ///// ///// /////

fn derivs_file_import_gens(
  derivs_file: File,
  all_files: List(File),
  gen_funcs: Dict(String, GenFunc),
) -> List(Gen) {
  let assert Ok(module) = glance.module(derivs_file.src)

  write_suppress_warning_type_aliases_to_file(module)

  module.imports
  |> list.map(fn(i) { i.definition })
  |> list.map(parser.parse_import_with_derivations(_, derivs_file.src))
  |> result.values
  |> list.flat_map(fn(x) {
    let #(import_, derivs) = x
    let #(file, types_and_derivs) = load_types_from_file(import_, derivs, all_files)

    types_and_derivs
    |> list.flat_map(gen_type_derivs(_, file, gen_funcs))
  })
}

fn load_types_from_file(import_: glance.Import, derivs: List(Derivation), all_files: List(File)) -> #(File, List(#(CustomType, List(Derivation), Dict(String, List(DerivFieldOpt))))) {
  let assert Ok(file) = list.find(all_files, fn(f) { f.module == import_.module })
  let assert Ok(module) = glance.module(file.src)

  let type_names =
    import_.unqualified_types
    |> list.map(fn(i) { i.name })

  let types_and_derivs =
    module.custom_types
    |> list.filter(fn(t) { list.contains(type_names, t.definition.name) })
    |> list.map(fn(t) {
      #(t.definition, derivs, dict.new())
    })

  #(file, types_and_derivs)
}

fn write_suppress_warning_type_aliases_to_file(module: glance.Module) -> Nil {
  let src =
    module
    |> suppress_warnings_types_to_write
    |> list.map(fn(t) { "pub type SuppressWarnings" <> t <> " = " <> t })
    |> string.join("\n")

  let project_derivs_filepath = "src/" <> project_name() <> "/derivs.gleam"

  let assert Ok(_) = simplifile.append(project_derivs_filepath, src)

  Nil
}

fn suppress_warnings_types_to_write(module: glance.Module) -> List(String) {
  let import_types =
    module.imports
    |> list.flat_map(fn(i) {
      i.definition.unqualified_types
      |> list.map(fn(t) {
        t.name
      })
    })

  let type_aliases =
    module.type_aliases
    |> list.map(fn(ta) {
      ta.definition.name
    })

  import_types
  |> list.filter(fn(t) {
    !list.contains(type_aliases, "SuppressWarnings" <> t)
  })
}
