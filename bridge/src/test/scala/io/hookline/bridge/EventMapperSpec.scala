package io.hookline.bridge

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.kafka.consumer.CommittableRecord
import zio.kafka.consumer.CommittableRecord
import zio.test.*
import zio.test.Assertion.*
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.common.TopicPartition

object EventMapperSpec extends ZIOSpecDefault:

  private val testKafkaCfg = KafkaConfig(
    bootstrapServers = "localhost:9092",
    groupId          = "test",
    topics           = "orders.created",
    topicPrefix      = "",
    autoOffsetReset  = "latest"
  )

  private val prefixedKafkaCfg = testKafkaCfg.copy(topicPrefix = "prod.")

  private def makeRecord(
    topic: String,
    partition: Int,
    offset: Long,
    key: String,
    value: String
  ): CommittableRecord[Array[Byte], Array[Byte]] =
    val cr = new ConsumerRecord[Array[Byte], Array[Byte]](
      topic, partition, offset,
      key.getBytes("UTF-8"),
      value.getBytes("UTF-8")
    )
    CommittableRecord(cr, _ => ZIO.unit, None)

  def spec = suite("EventMapper")(

    test("deterministic event_id from kafka coordinates") {
      val rec = makeRecord("orders.created", 0, 42L, "", """{"amount":99}""")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      assertTrue(ev.eventId == "kafka-orders.created-0-42")
    },

    test("topic passthrough when no prefix configured") {
      val rec = makeRecord("orders.created", 0, 0L, "", "{}")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      assertTrue(ev.topic == "orders.created")
    },

    test("topic prefix is stripped") {
      val rec = makeRecord("prod.orders.created", 0, 0L, "", "{}")
      val ev  = EventMapper.map(rec, prefixedKafkaCfg)
      assertTrue(ev.topic == "orders.created")
    },

    test("valid JSON object payload passes through") {
      val rec = makeRecord("t", 0, 0L, "", """{"order_id":"123","amount":99}""")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      ev.payload match
        case obj: Json.Obj =>
          assertTrue(obj.fields.exists(_._1 == "order_id"))
        case _ => assertTrue(false)
    },

    test("kafka key injected as _kafka_key") {
      val rec = makeRecord("t", 0, 0L, "order-123", """{"amount":50}""")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      ev.payload match
        case obj: Json.Obj =>
          assertTrue(obj.fields.exists { case (k, v) =>
            k == "_kafka_key" && v == Json.Str("order-123")
          })
        case _ => assertTrue(false)
    },

    test("non-JSON value wrapped in _raw") {
      val rec = makeRecord("t", 0, 0L, "", "plain-text-not-json")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      ev.payload match
        case obj: Json.Obj =>
          assertTrue(obj.fields.exists(_._1 == "_raw"))
        case _ => assertTrue(false)
    },

    test("non-object JSON (array) wrapped in _value") {
      val rec = makeRecord("t", 0, 0L, "", """[1,2,3]""")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      ev.payload match
        case obj: Json.Obj =>
          assertTrue(obj.fields.exists(_._1 == "_value"))
        case _ => assertTrue(false)
    },

    test("payload serialises to valid JSON") {
      val rec = makeRecord("orders.created", 2, 100L, "k", """{"x":1}""")
      val ev  = EventMapper.map(rec, testKafkaCfg)
      val json = ev.toJson
      assertTrue(json.contains("\"topic\""))
      assertTrue(json.contains("\"event_id\""))
      assertTrue(json.contains("\"payload\""))
    }
  )
