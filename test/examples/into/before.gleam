import examples/into/bar.{type Baz}
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

pub type Named {
  //$ derive into examples/into/foo.Foo
  //$ derive into examples/into/bar.Bar
  //$ derive into examples/into/bar.Baz
  Named(
    name: String,
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
