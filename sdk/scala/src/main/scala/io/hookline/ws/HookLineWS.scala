package io.hookline.ws

import io.hookline.HookLineError
import zio.*
import zio.http.*
import zio.http.socket.*
import zio.json.*
import zio.stream.ZStream

/** A frame sent or received over the HookLine WebSocket connection. */
sealed trait WSFrame derives JsonDecoder, JsonEncoder

object WSFrame:
  case class Subscribe(topic: String)                           extends WSFrame
  case class Unsubscribe(topic: String)                         extends WSFrame
  case class Publish(topic: String, payload: zio.json.ast.Json) extends WSFrame
  case class MarkRead(eventId: String)                          extends WSFrame
  case class Event(topic: String, payload: zio.json.ast.Json)  extends WSFrame
  case class Presence(topic: String, data: zio.json.ast.Json)  extends WSFrame

/** ZIO-based HookLine WebSocket client.
 *
 *  Acquire with `HookLineWS.connect`:
 *  {{{
 *    ZIO.scoped {
 *      HookLineWS.connect("ws://localhost:8080", "my-api-key").flatMap { ws =>
 *        ws.subscribe("orders.*") *>
 *          ws.onEvent.take(10).runCollect
 *      }
 *    }
 *  }}}
 */
trait HookLineWS:
  def subscribe(topic: String): IO[HookLineError, Unit]
  def unsubscribe(topic: String): IO[HookLineError, Unit]
  def publish(topic: String, payload: zio.json.ast.Json): IO[HookLineError, Unit]
  def markRead(eventId: String): IO[HookLineError, Unit]

  /** Stream of incoming event frames. */
  def onEvent: ZStream[Any, HookLineError, WSFrame.Event]

  /** Stream of incoming presence update frames. */
  def onPresence: ZStream[Any, HookLineError, WSFrame.Presence]

object HookLineWS:

  /** Connect to the HookLine WebSocket endpoint.
   *
   *  The returned resource is scoped — the connection is closed when the scope
   *  is released.
   *
   *  @param baseUrl  HTTP(S) or WS(S) base URL, e.g. "http://localhost:8080"
   *  @param apiKey   bearer token / API key
   */
  def connect(baseUrl: String, apiKey: String): ZIO[Scope, HookLineError, HookLineWS] =
    val wsUrl = baseUrl.replaceFirst("^http", "ws").stripSuffix("/") + s"/v1/ws?token=$apiKey"
    for
      eventQ    <- Queue.unbounded[WSFrame.Event]
      presenceQ <- Queue.unbounded[WSFrame.Presence]
      sendQ     <- Queue.unbounded[String]
      url       <- ZIO.fromEither(URL.decode(wsUrl))
                     .mapError(e => HookLineError.NetworkError(new Exception(s"Invalid WS URL: $e")))
      _         <- ZIO.acquireRelease(
                     Client.default.flatMap(_.socket(url, socketApp(sendQ, eventQ, presenceQ)))
                       .mapError(HookLineError.NetworkError(_))
                   )(_ => ZIO.unit)
    yield Live(sendQ, eventQ, presenceQ)

  private def socketApp(
    sendQ: Queue[String],
    eventQ: Queue[WSFrame.Event],
    presenceQ: Queue[WSFrame.Presence]
  ): WebSocketApp[Any] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
          zio.json.ast.Json.decoder.decodeJson(text) match
            case Right(json) =>
              json.get(zio.json.ast.JsonCursor.field("type").isString) match
                case Right(t) if t.value == "event"    =>
                  eventQ.offer(WSFrame.Event(
                    topic   = jsonStr(json, "topic"),
                    payload = jsonField(json, "payload")
                  )).unit
                case Right(t) if t.value == "presence" =>
                  presenceQ.offer(WSFrame.Presence(
                    topic = jsonStr(json, "topic"),
                    data  = jsonField(json, "data")
                  )).unit
                case _ => ZIO.unit
            case Left(_) => ZIO.unit

        case ChannelEvent.Read(WebSocketFrame.Close(_, _)) => ZIO.unit
        case ChannelEvent.Registered =>
          ZStream.fromQueue(sendQ)
            .mapZIO(msg => channel.send(ChannelEvent.Read(WebSocketFrame.text(msg))))
            .runDrain
            .forkDaemon
            .unit
        case _ => ZIO.unit
      }
    }

  private def jsonStr(json: zio.json.ast.Json, key: String): String =
    json.get(zio.json.ast.JsonCursor.field(key).isString).fold(_ => "", _.value)

  private def jsonField(json: zio.json.ast.Json, key: String): zio.json.ast.Json =
    json.get(zio.json.ast.JsonCursor.field(key)).getOrElse(zio.json.ast.Json.Null)

  private final class Live(
    sendQ: Queue[String],
    eventQ: Queue[WSFrame.Event],
    presenceQ: Queue[WSFrame.Presence]
  ) extends HookLineWS:

    def subscribe(topic: String): IO[HookLineError, Unit] =
      send(s"""{"type":"subscribe","topic":"$topic"}""")

    def unsubscribe(topic: String): IO[HookLineError, Unit] =
      send(s"""{"type":"unsubscribe","topic":"$topic"}""")

    def publish(topic: String, payload: zio.json.ast.Json): IO[HookLineError, Unit] =
      send(s"""{"type":"publish","topic":"$topic","payload":${payload.toJson}}""")

    def markRead(eventId: String): IO[HookLineError, Unit] =
      send(s"""{"type":"mark_read","event_id":"$eventId"}""")

    def onEvent: ZStream[Any, HookLineError, WSFrame.Event] =
      ZStream.fromQueue(eventQ)

    def onPresence: ZStream[Any, HookLineError, WSFrame.Presence] =
      ZStream.fromQueue(presenceQ)

    private def send(msg: String): IO[HookLineError, Unit] =
      sendQ.offer(msg).unit
