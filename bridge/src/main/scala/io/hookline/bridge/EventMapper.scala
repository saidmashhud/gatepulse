package io.hookline.bridge

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.kafka.consumer.CommittableRecord
import java.nio.charset.StandardCharsets

/** Wire type sent to POST /v1/events */
final case class HooklinePayload(
  topic: String,
  @jsonField("event_id") eventId: String,
  payload: Json
)

object HooklinePayload:
  given JsonEncoder[HooklinePayload] = DeriveJsonEncoder.gen[HooklinePayload]

/** Maps a raw Kafka record to a HookLine event.
 *
 *  topic:   Kafka topic, with optional prefix stripped per config
 *  eventId: "kafka-{topic}-{partition}-{offset}" — deterministic idempotency key
 *  payload: Kafka value as JSON; if not valid JSON, wrapped as {"_raw":"..."}
 */
object EventMapper:

  def map(
    record: CommittableRecord[Array[Byte], Array[Byte]],
    cfg: KafkaConfig
  ): HooklinePayload =
    val kafkaTopic    = record.record.topic()
    val hooklineTopic = cfg.stripPrefix(kafkaTopic)
    val eventId       = s"kafka-$kafkaTopic-${record.record.partition()}-${record.record.offset()}"
    val valueStr      = valueAsString(record.value)
    val keyStr        = valueAsString(record.key)
    val payload       = parsePayload(valueStr, keyStr)
    HooklinePayload(topic = hooklineTopic, eventId = eventId, payload = payload)

  private def valueAsString(bytes: Array[Byte]): String =
    Option(bytes).map(new String(_, StandardCharsets.UTF_8)).getOrElse("")

  private def parsePayload(value: String, key: String): Json =
    val keyField: Option[(String, Json)] =
      Option.when(key.nonEmpty)("_kafka_key" -> Json.Str(key))

    value.fromJson[Json] match
      case Right(obj: Json.Obj) =>
        val base =
          if key.nonEmpty && obj.fields.forall(_._1 != "_kafka_key")
          then obj.fields :+ ("_kafka_key" -> Json.Str(key))
          else obj.fields
        Json.Obj(base)
      case Right(other) =>
        val fields = Chunk("_value" -> other) ++ Chunk.fromIterable(keyField)
        Json.Obj(fields)
      case Left(_) =>
        val fields = Chunk("_raw" -> Json.Str(value)) ++ Chunk.fromIterable(keyField)
        Json.Obj(fields)
