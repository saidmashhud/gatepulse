package io.hookline.models

import zio.json.*

final case class ApiKey(
  key_id:     String,
  tenant_id:  String,
  name:       String,
  scopes:     List[String],
  created_at: String
)

object ApiKey:
  given JsonDecoder[ApiKey] = DeriveJsonDecoder.gen[ApiKey]
  given JsonEncoder[ApiKey] = DeriveJsonEncoder.gen[ApiKey]

/** Returned once on creation; the plaintext key is never stored. */
final case class ApiKeyCreated(
  key_id:     String,
  tenant_id:  String,
  name:       String,
  scopes:     List[String],
  key:        String,
  created_at: String
)

object ApiKeyCreated:
  given JsonDecoder[ApiKeyCreated] = DeriveJsonDecoder.gen[ApiKeyCreated]

final case class CreateApiKeyRequest(
  name:   String,
  scopes: List[String]
)

object CreateApiKeyRequest:
  given JsonEncoder[CreateApiKeyRequest] = DeriveJsonEncoder.gen[CreateApiKeyRequest]
