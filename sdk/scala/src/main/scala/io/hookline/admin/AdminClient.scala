package io.hookline.admin

import io.hookline.HookLineError
import zio.*
import zio.json.*

case class Tenant(
  id: String,
  name: String,
  apiKey: Option[String],
  createdAt: Long
) derives JsonDecoder, JsonEncoder

object Tenant:
  given JsonDecoder[Tenant] = DeriveJsonDecoder.gen[Tenant]

case class CreateTenantRequest(
  id: Option[String] = None,
  name: Option[String] = None
) derives JsonEncoder

object CreateTenantRequest:
  given JsonEncoder[CreateTenantRequest] = DeriveJsonEncoder.gen[CreateTenantRequest]

/** Admin operations on tenants. */
trait TenantsAdmin:
  def create(req: CreateTenantRequest = CreateTenantRequest()): IO[HookLineError, Tenant]
  def list(): IO[HookLineError, List[Tenant]]
  def get(id: String): IO[HookLineError, Tenant]
  def delete(id: String): IO[HookLineError, Unit]

/** Top-level admin client grouping admin sub-services. */
trait AdminClient:
  def tenants: TenantsAdmin
