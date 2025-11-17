import examples/into/foo as f

pub type Bar {
  //$ derive into examples/into/foo.Foo as f
  //$ derive into Missing
  Bar(
    title: String,
    //$ into field examples/into/foo.Foo name
    count: Int,
  )
}

pub type Missing {
  Missing(
    title: String,
    count: Int,
    bonus: Bool,
  )
}

// pub type UserForm {
//   UserForm(
//     first_name: String,
//     last_name: String,
//     email_address: String,
//   )
// }

// pub fn insert_user(
//   first_name first_name: String,
//   last_name last_name: String,
//   email email: String,
// ) -> Nil {
//   todo
// }

// pub fn insert_user_from_user_form(
//   user_form user_form: UserForm,
// ) -> Nil {
//   insert_user(
//     first_name: user_form.first_name,
//     last_name: user_form.last_name,
//     email: user_form.email_address,
//   )
// }

pub fn into_foo_from_bar(value: Bar) -> f.Foo {
  f.Foo(name: value.title, count: value.count)
}

pub fn into_missing_from_bar(value: Bar, bonus bonus: Bool) -> Missing {
  Missing(title: value.title, count: value.count, bonus:)
}
