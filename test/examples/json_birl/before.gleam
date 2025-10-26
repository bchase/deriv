import birl.{type Time}

pub type DateTimeExamples {
  //$ derive json decode encode
  DateTimeExamples(
    t0: Time,
    t1: Time, //$ json birl iso8601
    t2: Time, //$ json birl naive
    t3: Time, //$ json birl http
    t4: Time, //$ json birl unix
    t5: Time, //$ json birl unix_milli
    t6: Time, //$ json birl unix_micro
  )
}
