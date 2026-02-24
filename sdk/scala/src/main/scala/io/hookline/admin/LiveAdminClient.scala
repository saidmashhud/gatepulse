package io.hookline.admin

import io.hookline.HookLineError
import io.hookline.HookLineClient.Live
import zio.*
import zio.http.*
import zio.json.*

/** Concrete ZIO HTTP implementation of AdminClient. */
private[hookline] final class LiveAdminClient(http: Live) extends AdminClient:

  val tenants: TenantsAdmin = new TenantsAdmin:

    def create(req: CreateTenantRequest = CreateTenantRequest()): IO[HookLineError, Tenant] =
      http.post[CreateTenantRequest, Tenant]("/v1/tenants", req)

    def list(): IO[HookLineError, List[Tenant]] =
      http.get[ListResponse[Tenant]]("/v1/tenants").map(_.items)

    def get(id: String): IO[HookLineError, Tenant] =
      http.get[Tenant](s"/v1/tenants/$id")

    def delete(id: String): IO[HookLineError, Unit] =
      http.delete(s"/v1/tenants/$id")

private[admin] final case class ListResponse[A](items: List[A])
private[admin] object ListResponse:
  given [A: JsonDecoder]: JsonDecoder[ListResponse[A]] = DeriveJsonDecoder.gen[ListResponse[A]]
