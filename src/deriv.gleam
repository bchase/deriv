import bchase/casing
import gleam/option.{Some, None, type Option}
import deriv/internal/glance.{z, term, call} as _
//
import gleam/dict.{type Dict}
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import glance.{type CustomType, type TypeAlias, type Import, Import}
import gleam/regexp
import simplifile
import shellout
import tom
import deriv/internal/types.{type File, File, type Output, Output, OutputInline, type Write, Write, type GenFunc, type Gen, Gen, type Derivation, type DerivFieldOpts, type ModuleReader} as deriv
import deriv/internal/parser
import deriv/internal/derivs/json as deriv_json
import deriv/internal/derivs/from_into as deriv_from_into
import deriv/internal/derivs/zero as deriv_zero
import deriv/internal/derivs/enum as deriv_enum
import deriv/internal/derivs/form as deriv_form
import deriv/internal/derivs/functor as deriv_functor
import deriv/internal/common
import deriv/gen/types.{type ExprGen} as x
import deriv/internal/derivs/zero
import gleam/io
import argv
import glint

fn option_guard(
  option option: Option(a),
  fail fail: b,
  cont cont: fn(a) -> b,
) -> b {
  case option {
    Some(x) -> cont(x)
    None -> fail
  }
}

pub fn zero() -> ExprGen {
  x.CustomTypeDeriveExprGen(gens: [{
    use type_ <- x.custom_type()

    use src <- x.local_custom_type_src(type_:)

    use opts <- x.try(
      case parser.parse_type_with_derivations(type_, src) {
        Ok(#(_type, _derivs, opts)) -> x.success(opts)
        Error(Nil) -> x.failure("zero failed to parse derive opts")
      }
    )

    let imports = zero.gen_imports(type_) |> list.map(glance.Definition([], _))
    let func = zero.zero_func(type_, opts)

    use <- x.ensure_imports(imports)
    use <- x.ensure_func(func)

    // let variant =
    //   td.def.definition.variants
    //   |> list.sort(fn(a, b) {
    //     int.compare(
    //       list.length(a.fields),
    //       list.length(b.fields),
    //     )
    //   })
    //   |> list.first
    //   |> option.from_result

    // use variant <- option_guard(variant, x.failure("cannot derive zero with no variants: " <> string.inspect(td)))

    // let func = glance.Definition([], glance.Function(z,
    //   name: "zero_" <> casing.snake(td.def.definition.name),
    //   publicity: td.def.definition.publicity,
    //   return: Some(x.to_glance_type(td.def.definition, td.qualified)),
    //   parameters: [],
    //   body: [
    //     case variant.fields {
    //       [] ->
    //         term(variant.name)

    //       _ ->
    //         term(variant.name) |> call(list.map(variant.fields |> list.map(fn(f) { f.item }), zero_for(_, td)))
    //     }
    //     |> glance.Expression
    //   ],
    // ))

    // case td.def.definition.variants {
    //   [] ->
    //     todo

    //   [var1, ..vars] ->
    //     todo
    // }

    x.success(Nil)
  }])
}

fn zero_for(
  type_ type_: glance.Type,
  ctx ctx: a,
) -> glance.Expression {
  case type_ {
    glance.NamedType(name:, module:, parameters:, ..) -> todo

    glance.TupleType(elements:, ..) -> todo

    glance.FunctionType(..) |
    glance.VariableType(..) |
    glance.HoleType(..) -> panic as {
      "unable to derive zero for: " <> string.inspect(type_) <> "\n" <>
      string.inspect(ctx)
    }
  }
}

const all_type_gen_funcs: List(#(String, GenFunc)) =
  [
    #("json", deriv_json.gen),
    #("from", deriv_from_into.gen_from),
    #("into", deriv_from_into.gen_into),
    #("zero", deriv_zero.gen),
    #("enum", deriv_enum.gen),
    #("form", deriv_form.gen),
    #("functor", deriv_functor.gen),
  ]

pub fn main() -> Nil {
  let args = argv.load().arguments

  glint.new()
  |> glint.as_module()
  |> glint.add(at: [], do: code_gen_cmd())
  |> glint.run(args)
}

fn code_gen_cmd(
) -> glint.Command(Nil) {
  use <- glint.command_help("Generates derivations")
  use _named_args, _args, _flags <- glint.command()

  exec_code_gen()
}

fn exec_code_gen() -> Nil {
  let filepaths = find_project_src_gleam_filepaths()

  filepaths
  |> load_files
  |> gen_derivs(common.fetch_module)
  |> build_writes
  |> perform_file_writes
}

fn find_project_src_gleam_filepaths() -> List(String) {
  let assert Ok(output) = shellout.command(in: ".", opt: [],
    run: "find", with: [
      "src/",
      "-name", "*.gleam",
      "-exec", "grep", "-l", "derive", "{}", "+",
    ]
  )

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
  let config = common.gleam_toml()

  case tom.get_string(config, ["name"]) {
    Error(_) -> panic as "Cannot determine project name from `gleam.toml`"
    Ok(name) -> name
  }
}

pub fn gen_derivs(
  files: List(File),
  module_reader: ModuleReader,
) -> List(Gen) {
  let gen_funcs = all_type_gen_funcs |>  dict.from_list

  let project_derivs_module_name = project_name() <> "/derivs"

  let type_inline_gens =
    files
    |> list.filter(fn(file) {
      file.module != project_derivs_module_name
    })
    |> list.flat_map(fn(file) {
      let file = File(..file, idx: None)

      let custom_type_gens =
        file
        |> parse_types_and_derivations
        |> list.flat_map(gen_type_derivs(_, file, gen_funcs, module_reader))

      let type_aliases_gens =
        file
        |> parse_type_aliases_and_derivations
        |> list.flat_map(gen_type_derivs(_, file, gen_funcs, module_reader))

      [
        custom_type_gens,
        type_aliases_gens,
      ]
      |> list.flatten
    })
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

fn parse_types_and_derivations(file: File) -> List(#(deriv.Type, List(Derivation), DerivFieldOpts)) {
  let parsed =
    case glance.module(file.src) {
      Error(err) -> {
        common.debug(file)
        common.debug(err)
        panic
      }

      Ok(x) -> x
    }

  parsed.custom_types
  |> list.map(fn(ct) { ct.definition })
  |> list.map(parser.parse_type_with_derivations(_, file.src))
  |> list.map(result.map(_, to_deriv_type))
  |> result.values
}

fn to_deriv_type(
  t: #(CustomType, List(Derivation), DerivFieldOpts),
) -> #(deriv.Type, List(Derivation), DerivFieldOpts) {
    let #(type_, derivs, opts) = t

    #(deriv.Type(type_:), derivs, opts)
}

fn to_deriv_type_alias(
  t: #(TypeAlias, List(Derivation), DerivFieldOpts),
) -> #(deriv.Type, List(Derivation), DerivFieldOpts) {
    let #(type_alias, derivs, opts) = t

    #(deriv.TypeAlias(type_alias:), derivs, opts)
}

fn parse_type_aliases_and_derivations(file: File) -> List(#(deriv.Type, List(Derivation), DerivFieldOpts)) {
  let parsed =
    case glance.module(file.src) {
      Error(err) -> {
        common.debug(file)
        common.debug(err)
        panic
      }

      Ok(x) -> x
    }

  parsed.type_aliases
  |> list.map(fn(ta) { ta.definition })
  |> list.map(parser.parse_type_aliases_with_derivations(_, file.src))
  |> list.map(result.map(_, to_deriv_type_alias))
  |> result.values
}

fn gen_type_derivs(
  x: #(deriv.Type, List(Derivation), DerivFieldOpts),
  file: File,
  gen_funcs: Dict(String, GenFunc),
  module_reader: ModuleReader,
) -> List(Gen) {
  let #(type_, derivs, field_opts) = x

  let to_ctx =
    deriv.Context(
      deriv: _,
      opts: field_opts,
      file:,
      module_reader:,
    )

  derivs
  |> list.map(fn(deriv) {
    case dict.get(gen_funcs, deriv.name) {
      Error(_) -> Error(Nil)
      Ok(f) -> {
        Ok(f(type_, deriv |> to_ctx))
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
      |> list.map(fn(func) { #(common.func_name(func), common.func_str(func)) })

    let types =
      gens
      |> list.flat_map(fn(gen) { gen.types })
      |> list.map(fn(type_) { #(type_.definition.name, common.type_str(type_)) })

    let consts =
      gens
      |> list.flat_map(fn(gen) { gen.consts })
      |> list.map(fn(const_) { #(const_.definition.name, const_.definition) })

    let module_imports: List(Import) = build_module_imports(gens, output)
    let deriv_imports: List(Import) = list.flat_map(gens, fn(gen) { gen.imports })
    let all_imports: List(Import) = [module_imports, deriv_imports] |> list.flatten

    let output_src =
      orig_src
      |> common.update_types(types)
      |> common.update_consts(consts)
      |> common.update_funcs(funcs)
      |> common.consolidate_imports_for(all_imports)
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

    let assert Ok(_) = simplifile.write(write.filepath, write.src |> string.trim)
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

      common.consolidate_imports_for(func_src, all_imports)
    }

    OutputInline(..) -> {
      // TODO don't think this logic is ever run...
      panic as "unimplemented"
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
          location: common.dummy_location(),
          module: file.module,
          alias: Some(glance.Named("m" <> int.to_string(idx))),
          unqualified_types: [],
          unqualified_values: [],
        )
        |> list.wrap

      None ->
        []
    }
  })
}

pub fn stop_warning() { common.debug("") }

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
    |> list.map(to_deriv_type)
    |> list.flat_map(gen_type_derivs(_, file, gen_funcs, common.fetch_module))
  })
}

fn load_types_from_file(import_: glance.Import, derivs: List(Derivation), all_files: List(File)) -> #(File, List(#(CustomType, List(Derivation), DerivFieldOpts))) {
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
