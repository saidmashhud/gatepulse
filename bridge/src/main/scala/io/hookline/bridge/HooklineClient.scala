package io.hookline.bridge

import zio.*
import zio.http.*
import zio.json.*

/** Publishes events to HookLine via POST /v1/events.
 *
 *  At-least-once: returns ZIO success only when HookLine responds 2xx.
 *  On non-2xx the ZIO fails with PublishError — caller retries.
 *  HookLine deduplicates via event_id so Kafka-offset-derived IDs are safe.
 */
trait HooklineClient:
  def publish(event: HooklinePayload): Task[Unit]

object HooklineClient:

  final case class PublishError(status: Int, body: String)
    extends Exception(s"HookLine returned $status: $body")

  val live: ZLayer[Client & BridgeConfig, Nothing, HooklineClient] =
    ZLayer.fromZIO(
      for
        client <- ZIO.service[Client]
        cfg    <- ZIO.service[BridgeConfig]
        url    <- ZIO.fromEither(URL.decode(s"${cfg.hookline.url}/v1/events"))
                    .mapError(e => new Exception(s"Invalid HOOKLINE_URL '${cfg.hookline.url}': $e"))
                    .orDie   // bad URL is a configuration error — fail at startup, not per-request
      yield Live(client, url, cfg.hookline.apiKey)
    )

  private final class Live(client: Client, eventsUrl: URL, apiKey: String) extends HooklineClient:

    private val authHeader = Header.Authorization.Bearer(apiKey)

    def publish(event: HooklinePayload): Task[Unit] =
      val body = Body.fromString(event.toJson)
      val request = Request(
        url     = eventsUrl,
        method  = Method.POST,
        headers = Headers(authHeader, Header.ContentType(MediaType.application.json)),
        body    = body
      )
      // ZIO.scoped: zio-http 3.x needs a Scope for the response lifecycle
      ZIO.scoped {
        client.request(request).flatMap { response =>
          if response.status.isSuccess then ZIO.unit
          else
            response.body.asString.flatMap { respBody =>
              ZIO.fail(PublishError(response.status.code, respBody))
            }
        }
      }

  def publish(event: HooklinePayload): ZIO[HooklineClient, Throwable, Unit] =
    ZIO.serviceWithZIO[HooklineClient](_.publish(event))
