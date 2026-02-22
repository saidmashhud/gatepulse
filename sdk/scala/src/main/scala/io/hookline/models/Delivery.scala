package io.hookline.models

import zio.json.*

final case class Delivery(
  job_id:         String,
  event_id:       String,
  endpoint_id:    String,
  tenant_id:      String,
  state:          String,
  attempt_count:  Int,
  next_attempt_at: Option[String],
  created_at:     String
)

object Delivery:
  given JsonDecoder[Delivery] = DeriveJsonDecoder.gen[Delivery]
  given JsonEncoder[Delivery] = DeriveJsonEncoder.gen[Delivery]

final case class DlqEntry(
  dlq_id:      String,
  job_id:      String,
  event_id:    String,
  endpoint_id: String,
  tenant_id:   String,
  reason:      String,
  created_at:  String
)

object DlqEntry:
  given JsonDecoder[DlqEntry] = DeriveJsonDecoder.gen[DlqEntry]
  given JsonEncoder[DlqEntry] = DeriveJsonEncoder.gen[DlqEntry]
