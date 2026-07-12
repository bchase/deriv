import gleam/pair
import gleam/result
import gleam/list
import gleam/option.{type Option, Some, None}
import bchase/number.{type Number}
import gleam/int
import gleam/float
import gleam/string
import bchase/function.{always}
import bchase/lens.{type Lens, Lens}
import bchase/list.{push as list_push} as _

pub opaque type ReadWriteResult(t, e, r, w) {
  ReadWriteResult(
    run: fn(r, w) -> #(Result(t, e), w),
  )
}

// fold ...

pub fn run(
  rw rw: ReadWriteResult(t, e, r, w),
  read read: r,
  write write: w,
) -> Result(t, e) {
  rw.run(read, write)
  |> pair.first
}

pub fn run_(
  rw rw: ReadWriteResult(t, e, r, w),
  read read: r,
  write write: w,
) -> #(Result(t, e), w) {
  rw.run(read, write)
}

pub fn pure(
  val val: t,
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(fn(_read, writes) {
    #(Ok(val), writes)
  })
}

pub fn fail(
  err err: e,
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, writes) {
    #(Error(err), writes)
  })
}

pub fn do(
  rw rw: ReadWriteResult(a, e, r, w),
  cont cont: fn(a) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  ReadWriteResult(run: fn(read, writes) {
    case run_(rw, read, writes) {
      #(Error(err), writes) ->
        #(Error(err), writes)

      #(Ok(x), writes) ->
        cont(x) |> run_(read, writes)
    }
  })
}

pub fn bind(
  rw rw: ReadWriteResult(a, e, r, w),
  cont cont: fn(a) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  do(rw:, cont:)
}

pub fn map(
  rw rw: ReadWriteResult(a, e, r, w),
  apply f: fn(a) -> b,
) -> ReadWriteResult(b, e, r, w) {
  use x <- do(rw)
  pure(f(x))
}

pub fn map_list(
  rw rw: ReadWriteResult(List(a), e, r, w),
  apply f: fn(a) -> b,
) -> ReadWriteResult(List(b), e, r, w) {
  map(rw, list.map(_, f))
}

pub fn map_ok(
  rw rw: ReadWriteResult(Result(a, e1), e, r, w),
  apply f: fn(a) -> b,
) -> ReadWriteResult(Result(b, e1), e, r, w) {
  map(rw, result.map(_, f))
}

pub fn map_some(
  rw rw: ReadWriteResult(Option(a), e, r, w),
  apply f: fn(a) -> b,
) -> ReadWriteResult(Option(b), e, r, w) {
  map(rw, option.map(_, f))
}

pub fn replace(
  rw rw: ReadWriteResult(a, e, r, w),
  val val: b
) -> ReadWriteResult(b, e, r, w) {
  use _ <- do(rw)
  pure(val)
}

pub fn flatten(
  rw rw: ReadWriteResult(ReadWriteResult(t, e, r, w), e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  use rw <- do(rw)
  rw
}

pub fn to_result(
  rw rw: ReadWriteResult(a, e, r, Nil),
  cont cont: fn(#(Result(a, e), Nil)) -> ReadWriteResult(b, e, r, Nil),
) -> ReadWriteResult(b, e, r, Nil) {
  to_result_shared_writes(rw, cont)
}

pub fn to_result_shared_writes(
  rw rw: ReadWriteResult(a, e, r, w),
  cont cont: fn(#(Result(a, e), w)) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  ReadWriteResult(run: fn(read, write) {
    cont(run_(rw, read, write)).run(read, write) // TODO `.run` --> `_run`
  })
}

pub fn to_result_separate_writes(
  write zero: w1,
  rw rw: ReadWriteResult(a, e, r, w1),
  cont cont: fn(#(Result(a, e), w1)) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  ReadWriteResult(run: fn(read, write) {
    cont(run_(rw, read, zero)).run(read, write) // TODO `.run` --> `_run`
  })
}

pub fn from_result(
  result result: Result(t, e),
) -> ReadWriteResult(t, e, r, w) {
  case result {
    Ok(x) -> pure(x)
    Error(err) -> fail(err)
  }
}

pub fn from_result_set_writes(
  write write: w,
  result result: Result(t, e),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, _write) {
    #(result, write)
  })
}

pub fn ok(
  result result: Result(t, e1),
  err map: fn(e1) -> e,
) -> ReadWriteResult(t, e, r, w) {
  case result {
    Ok(x) -> pure(x)
    Error(err) -> fail(map(err))
  }
}

pub fn some(
  option option: Option(t),
  err err: fn() -> e,
) -> ReadWriteResult(t, e, r, w) {
  case option {
    Some(x) -> pure(x)
    None -> fail(err())
  }
}

pub fn do_ok(
  result result: Result(a, e1),
  err err: fn(e1) -> e,
  cont cont: fn(a) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  do(ok(result:, err:), cont:)
}

pub fn do_some(
  option option: Option(a),
  err err: fn() -> e,
  cont cont: fn(a) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  do(some(option:, err:), cont:)
}

//

pub fn read(
  cont cont: fn(r) -> ReadWriteResult(t, e, r, w)
) -> ReadWriteResult(t, e, r, w) {
  do(
    ReadWriteResult(run: fn(read, writes) {
      #(Ok(read), writes)
    }),
    cont,
  )
}

//

pub fn writes(
  cont cont: fn(w) -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, writes) {
    #(Ok(writes), writes)
  })
  |> do(cont)
}

pub fn writes_(
  at lens: Lens(w, vs),
  cont cont: fn(vs) -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, writes) {
    #(Ok(lens.get(writes)), writes)
  })
  |> do( cont)
}

pub fn write(
  val new: v,
  into lens: Lens(w, vs),
  using f: fn(vs, v) -> vs,
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, write) {
    let old = lens.get(write)
    let write = lens.set(write, f(old, new))
    #(Ok(Nil), write)
  })
  |> do(always(cont()))
}

pub fn set(
  val new: w,
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, _write) {
    #(Ok(Nil), new)
  })
  |> do(always(cont()))
}

pub fn set_(
  val new: v,
  into lens: Lens(w, v),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, write) {
    let write = lens.set(write, new)
    #(Ok(Nil), write)
  })
  |> do(always(cont()))
}

pub fn push(
  el val: w,
  cont cont: fn() -> ReadWriteResult(t, e, r, List(w)),
) -> ReadWriteResult(t, e, r, List(w)) {
  write(val:, using: list_push, into: lens.identity, cont:)
}

pub fn concat(
  list val: List(w),
  cont cont: fn() -> ReadWriteResult(t, e, r, List(w)),
) -> ReadWriteResult(t, e, r, List(w)) {
  write(val:, using: list.append, into: lens.identity, cont:)
}

pub fn append(
  str val: String,
  cont cont: fn() -> ReadWriteResult(t, e, r, String),
) -> ReadWriteResult(t, e, r, String) {
  write(val:, using: string.append, into: lens.identity, cont:)
}

pub fn add_number(
  num val: Number,
  cont cont: fn() -> ReadWriteResult(t, e, r, Number),
) -> ReadWriteResult(t, e, r, Number) {
  write(val:, using: number.add, into: lens.identity, cont:)
}

pub fn add_int(
  int val: Int,
  cont cont: fn() -> ReadWriteResult(t, e, r, Int),
) -> ReadWriteResult(t, e, r, Int) {
  write(val:, using: int.add, into: lens.identity, cont:)
}

pub fn add_float(
  float val: Float,
  cont cont: fn() -> ReadWriteResult(t, e, r, Float),
) -> ReadWriteResult(t, e, r, Float) {
  write(val:, using: float.add, into: lens.identity, cont:)
}

pub fn push_(
  el val: v,
  at into: Lens(w, List(v)),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  write(val:, using: list_push, into:, cont:)
}

pub fn append_(
  str val: String,
  at into: Lens(w, String),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  write(val:, using: string.append, into:, cont:)
}

pub fn concat_(
  list val: List(v),
  at into: Lens(w, List(v)),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  write(val:, using: list.append, into:, cont:)
}

pub fn add_number_(
  num val: Number,
  at into: Lens(w, Number),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  write(val:, using: number.add, into:, cont:)
}

pub fn add_int_(
  int val: Int,
  at into: Lens(w, Int),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  write(val:, using: int.add, into:, cont:)
}

pub fn add_float_(
  float val: Float,
  at into: Lens(w, Float),
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  write(val:, using: float.add, into:, cont:)
}

//

pub fn fold(
  over list: List(t),
  from acc: acc,
  with f: fn(acc, t) -> ReadWriteResult(acc, e, r, w),
) -> ReadWriteResult(acc, e, r, w) {
  case list {
    [] ->
      pure(acc)

    [x, ..xs] -> {
      use acc <- do(f(acc, x))
      fold(over: xs, from: acc, with: f)
    }
  }
}

// pub fn map_m(
//   arg arg: a,
//   cont cont: fn(a) ->  ReadWriteResult(b, e, r, w),
// ) -> ReadWriteResult(b, e, r, w) {
// }

// pub fn map_m_(
//   arg arg: a,
//   cont cont: fn(a) ->  ReadWriteResult(b, e, r, w),
// ) -> ReadWriteResult(Nil, e, r, w) {
// }

pub fn sequence_shared_write_stop_on_err(
  rws rws: List(ReadWriteResult(t, e, r, w)),
) -> ReadWriteResult(List(t), e, r, w) {
  ReadWriteResult(run: fn(read, write) {
    let #(xs, status, write) =
      list.fold_until(rws, #([], Ok(Nil), write), fn(acc, rw) {
        case run_(rw, read, acc.2) {
          #(Ok(x), write) ->
            #([x, ..acc.0], Ok(Nil), write)
            |> list.Continue

          #(Error(err), write) ->
            #(acc.0, Error(err), write)
            |> list.Stop
        }
      })

    case status {
      Ok(_) ->
        #(Ok(xs), write)

      Error(err) ->
        #(Error(err), write)
    }
  })
}

pub fn sequence_shared_write_continue_on_err(
  rws rws: List(ReadWriteResult(t, e, r, w)),
) -> ReadWriteResult(List(Result(t, e)), e, r, w) {
  ReadWriteResult(run: fn(read, write) {
    list.fold(rws, #([], write), fn(acc, rw) {
      run_(rw, read, acc.1)
      |> pair.map_first(fn(result) { [result, ..acc.0] })
    })
    |> pair.map_first(list.reverse)
    |> pair.map_first(Ok)
  })
}

//

pub type Log {
  Log(
    total: Int,
    msgs: List(String),
  )
}

const zero_log = Log(total: 0, msgs: [])

const total = Lens(get: get_total, set: set_total)
fn get_total(x: Log) { x.total }
fn set_total(x: Log, total) { Log(..x, total:)}

const msgs = Lens(get: get_msgs, set: set_msgs)
fn get_msgs(x: Log) { x.msgs }
fn set_msgs(x: Log, msgs) { Log(..x, msgs:)}

pub fn app() {
  use <- push_("start", msgs)

  use <- add_int_(1, total)
  use r <- to_result_separate_writes(zero_log, {
    use <- push_("inner", msgs)
    pure(123)
  })
  echo r
  use <- add_int_(2, total)
  use _ <- do(fail("woops"))
  use <- add_int_(3, total)
  use writes <- writes_(at: total)

  use <- push_("end", msgs)

  use read <- read()

  pure("success " <> read <> " " <> string.inspect(writes))
}

pub fn main() {
  app()
  |> run_("hi", zero_log)
  |> echo

  Nil
}
