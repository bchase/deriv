import gleam/order
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/dict.{type Dict}
import gleam/regexp
import form
// TODO mv validation

pub type ValidationErrs(field) = Dict(#(field, List(Int)), List(String))

pub type ValidationResult(field) {
  ValidationSuccess
  ValidationFailure(
    field: field,
    indexes: List(Int),
    errs: List(String),
  )
}

pub fn is_present(
  target: #(field, String),
) -> ValidationResult(field) {
  let #(field, str) = target

  let assert Ok(whitespace_only_re) =
    "^\\s*$"
    |> regexp.from_string

  case regexp.check(whitespace_only_re, str) {
    False -> ValidationSuccess
    True -> ValidationFailure(field:, indexes: [], errs: ["must be present"])
  }
}

pub fn length(
  target: #(field, String),
  order: Order,
  count: Int,
) -> ValidationResult(field) {
  let #(field, str) = target

  case compare_(int.compare(string.length(str), count), order) {
    True -> ValidationSuccess
    False -> {
      let err =
        "must be " <> validate_order_str(order) <> " than " <> int.to_string(count) <> "long"

      ValidationFailure(field:, indexes: [], errs: [err])
    }
  }
}

pub type Order {
  Gt
  Gte
  Eq
  Lt
  Lte
}

fn compare_(o1: order.Order, o2: Order) -> Bool {
  case o2, o1 {
    Gt, order.Gt -> True
    Eq, order.Eq -> True
    Lt, order.Lt -> True

    Gte, order.Gt -> True
    Gte, order.Eq -> True

    Lte, order.Lt -> True
    Lte, order.Eq -> True

    _, _ -> False
  }
}

fn validate_order_str(order: Order) -> String {
  case order {
    Gt -> "greater than"
    Gte -> "greater than or equal to"
    Eq -> "equal to"
    Lt -> "less than"
    Lte -> "less than or equal to"
  }
}

pub fn int(
  target: #(field, Int),
  order: Order,
  int2: Int,
) -> ValidationResult(field) {
  let #(field, int1) = target

  case compare_(int.compare(int1, int2), order) {
    True -> ValidationSuccess
    False -> {
      let err =
        "must be " <> validate_order_str(order) <> " than " <> int.to_string(int2)

      ValidationFailure(field:, indexes: [], errs: [err])
    }
  }
}

pub fn float(
  target: #(field, Float),
  order: Order,
  float2: Float,
) -> ValidationResult(field) {
  let #(field, float1) = target

  case compare_(float.compare(float1, float2), order) {
    True -> ValidationSuccess
    False -> {
      let err =
        "must be " <> validate_order_str(order) <> " than " <> float.to_string(float2)

      ValidationFailure(field:, indexes: [], errs: [err])
    }
  }
}

pub fn run(
  vs: List(ValidationResult(field)),
  form: form
) -> Result(form, form.Err(field)) {
  vs
  |> to_errors
  |> fn(errs) { // TODO generic
    case dict.is_empty(errs) {
      True ->
        Ok(form)

      False ->
        errs
        |> form.FormInvalid
        |> Error
    }
  }
}

fn to_errors(
  validations: List(ValidationResult(field))
) -> ValidationErrs(field) {
  validations
  |> list.fold(dict.new(), fn(acc, vr) {
    case vr {
      ValidationSuccess ->
        acc

      ValidationFailure(field:, indexes:, errs:) -> {
        [#(#(field, indexes), errs)]
        |> dict.from_list
        |> dict.combine(acc, fn(errs_new, errs_acc) {
          list.append(errs_acc, errs_new)
        })
      }
    }
  })
}
