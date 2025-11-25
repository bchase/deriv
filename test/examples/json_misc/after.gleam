import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

pub type Task {
  //$ derive json decode encode
  Task(
    id: String,
    name: String,
    project: NamedResource(Project),
  )
}

pub type Project {
  Project(
    name: String,
  )
}

pub type NamedResource(resource) {
  //$ derive json decode encode
  NamedResource(
    id: Int,
    name: String,
  )
}

pub fn decoder_task() -> Decoder(Task) {
  decode.one_of(decoder_task_task(), [])
}

pub fn decoder_task_task() -> Decoder(Task) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use project <- decode.field("project", decoder_named_resource())
  decode.success(Task(id:, name:, project:))
}

pub fn encode_task(value: Task) -> Json {
  case value {
    Task(..) as value ->
      json.object([
        #("id", json.string(value.id)),
        #("name", json.string(value.name)),
        #("project", encode_named_resource(value.project)),
      ])
  }
}

pub fn decoder_named_resource() -> Decoder(NamedResource(resource)) {
  decode.one_of(decoder_named_resource_named_resource(), [])
}

pub fn decoder_named_resource_named_resource() -> Decoder(
  NamedResource(resource),
) {
  use id <- decode.field("id", decode.int)
  use name <- decode.field("name", decode.string)
  decode.success(NamedResource(id:, name:))
}

pub fn encode_named_resource(value: NamedResource(resource)) -> Json {
  case value {
    NamedResource(..) as value ->
      json.object([
        #("id", json.int(value.id)),
        #("name", json.string(value.name)),
      ])
  }
}
