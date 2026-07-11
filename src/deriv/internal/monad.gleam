import gleam/string
import bchase/function.{always}
import bchase/lens.{type Lens, Lens}
import bchase/list.{push as list_push} as _

pub opaque type ReadWriteResult(t, e, r, w) {
  ReadWriteResult(run: fn(r, w) -> #(Result(t, e), w))
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
    case rw.run(read, writes) {
      #(Ok(x), writes) -> cont(x).run(read, writes)
      #(Error(err), writes) -> #(Error(err), writes)
    }
  })
}

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

pub fn write(
  val val: v,
  using f: fn(w, v) -> w,
  cont cont: fn() -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  do(
    ReadWriteResult(run: fn(_read, writes) {
      #(Ok(Nil), f(writes, val))
    }
  ), always(cont()))
}

pub fn write_list(
  val val: w,
  cont cont: fn() -> ReadWriteResult(t, e, r, List(w)),
) -> ReadWriteResult(t, e, r, List(w)) {
  write(val, list_push, cont)
}

pub fn writes(
  cont cont: fn(w) -> ReadWriteResult(t, e, r, w),
) -> ReadWriteResult(t, e, r, w) {
  do(
    ReadWriteResult(run: fn(_read, writes) {
      #(Ok(writes), writes)
    }
  ), cont)
}

pub fn run(
  rw rw: ReadWriteResult(t, e, r, w),
  read read: r,
  zero zero: w,
) -> #(Result(t, e), w) {
  rw.run(read, zero)
}

//

pub fn app() {
  use read <- read()

  use <- write_list(1)
  // use _ <- do(fail("woops"))
  use <- write_list(2)
  use writes <- writes()
  use <- write_list(3)

  pure("success " <> read <> " " <> string.inspect(writes))
}


pub fn main() {
  app()
  |> run("hi", [])
  |> echo

  Nil
}
