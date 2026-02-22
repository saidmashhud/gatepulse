package io.hookline.models

import zio.json.*

final case class Event(
  event_id:   String,
  tenant_id:  String,
  topic:      String,
  payload:    zio.json.ast.Json,
  created_at: String
)

object Event:
  given JsonDecoder[Event] = DeriveJsonDecoder.gen[Event]
  given JsonEncoder[Event] = DeriveJsonEncoder.gen[Event]
