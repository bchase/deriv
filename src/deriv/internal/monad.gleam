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

// do_
// do_ok ...
// map_ok, map_some, map_list ...
// fold ...
// flatten
// sequence
// sequence_

pub fn run(
  rw rw: ReadWriteResult(t, e, r, w),
  read read: r,
  zero zero: w,
) -> #(Result(t, e), w) {
  rw.run(read, zero)
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
    case run(rw, read, writes) {
      #(Ok(x), writes) -> cont(x).run(read, writes)
      #(Error(err), writes) -> #(Error(err), writes)
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
  zero zero: w1,
  rw rw: ReadWriteResult(a, e, r, w1),
  cont cont: fn(Result(a, e)) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  ReadWriteResult(run: fn(read, write) {
    let #(result, _write) = run(rw, read, zero)
    cont(result).run(read, write)
  })
}

pub fn to_result_(
  zero zero: w,
  rw rw: ReadWriteResult(a, e, r, w),
  cont cont: fn(#(Result(a, e), w)) -> ReadWriteResult(b, e, r, w),
) -> ReadWriteResult(b, e, r, w) {
  ReadWriteResult(run: fn(read, write) {
    cont(run(rw, read, zero)).run(read, write)
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

pub fn from_result_(
  write write: w,
  result result: Result(t, e),
) -> ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(_read, _write) {
    #(result, write)
  })
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
  do(
    ReadWriteResult(run: fn(_read, writes) {
      #(Ok(writes), writes)
    }
  ), cont)
}

pub fn writes_(
  at lens: Lens(w, vs),
  cont cont: fn(vs) -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  do(
    ReadWriteResult(run: fn(_read, writes) {
      #(Ok(lens.get(writes)), writes)
    }
  ), cont)
}

pub fn write(
  val new: v,
  into lens: Lens(w, vs),
  using f: fn(vs, v) -> vs,
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  do(
    ReadWriteResult(run: fn(_read, write) {
      let old = lens.get(write)
      let write = lens.set(write, f(old, new))
      #(Ok(Nil), write)
    }
  ), always(cont()))
}

pub fn push(
  el val: w,
  cont cont: fn() -> ReadWriteResult(t, e, r, List(w)),
) -> ReadWriteResult(t, e, r, List(w)) {
  write(val:, using: list_push, into: lens.identity, cont:)
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
  use r <- to_result_(zero_log, {
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
  |> run("hi", zero_log)
  |> echo

  Nil
}
