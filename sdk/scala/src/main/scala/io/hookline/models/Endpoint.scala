package io.hookline.models

import zio.json.*

final case class Endpoint(
  endpoint_id:  String,
  tenant_id:    String,
  url:          String,
  description:  Option[String],
  secret:       Option[String],
  enabled:      Boolean,
  created_at:   String
)

object Endpoint:
  given JsonDecoder[Endpoint] = DeriveJsonDecoder.gen[Endpoint]
  given JsonEncoder[Endpoint] = DeriveJsonEncoder.gen[Endpoint]

final case class CreateEndpointRequest(
  url:         String,
  description: Option[String] = None,
  secret:      Option[String] = None,
  enabled:     Option[Boolean] = None
)

object CreateEndpointRequest:
  given JsonEncoder[CreateEndpointRequest] = DeriveJsonEncoder.gen[CreateEndpointRequest]

final case class UpdateEndpointRequest(
  url:         Option[String] = None,
  description: Option[String] = None,
  secret:      Option[String] = None,
  enabled:     Option[Boolean] = None
)

object UpdateEndpointRequest:
  given JsonEncoder[UpdateEndpointRequest] = DeriveJsonEncoder.gen[UpdateEndpointRequest]
