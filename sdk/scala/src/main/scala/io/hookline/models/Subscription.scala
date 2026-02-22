package io.hookline.models

import zio.json.*

final case class Subscription(
  subscription_id: String,
  tenant_id:       String,
  endpoint_id:     String,
  topic_pattern:   String,
  filter:          Option[zio.json.ast.Json],
  transform:       Option[zio.json.ast.Json],
  created_at:      String
)

object Subscription:
  given JsonDecoder[Subscription] = DeriveJsonDecoder.gen[Subscription]
  given JsonEncoder[Subscription] = DeriveJsonEncoder.gen[Subscription]

final case class CreateSubscriptionRequest(
  endpoint_id:   String,
  topic_pattern: String,
  filter:        Option[zio.json.ast.Json] = None,
  transform:     Option[zio.json.ast.Json] = None
)

object CreateSubscriptionRequest:
  given JsonEncoder[CreateSubscriptionRequest] = DeriveJsonEncoder.gen[CreateSubscriptionRequest]
