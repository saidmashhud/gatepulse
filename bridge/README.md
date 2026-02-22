# HookLine Bridge

Scala/ZIO service that consumes Kafka topics and publishes events to HookLine.

```
Kafka topics → Bridge → POST /v1/events → HookLine → webhook delivery
```

## At-least-once guarantees

- Kafka offset committed **only after** HookLine returns 2xx
- `event_id = "kafka-{topic}-{partition}-{offset}"` — deterministic, enables HookLine idempotency deduplication
- Crash between publish and commit → Kafka redelivers → HookLine deduplicates silently

## Configuration

| Env var | Default | Description |
|---------|---------|-------------|
| `KAFKA_BOOTSTRAP_SERVERS` | `localhost:9092` | Comma-separated brokers |
| `KAFKA_GROUP_ID` | `hookline-bridge` | Consumer group |
| `KAFKA_TOPICS` | `events` | Comma-separated topics to consume |
| `KAFKA_TOPIC_PREFIX` | `` | Strip this prefix before publishing (e.g. `prod.`) |
| `KAFKA_AUTO_OFFSET_RESET` | `latest` | `earliest` or `latest` |
| `HOOKLINE_URL` | `http://localhost:8080` | HookLine base URL |
| `HOOKLINE_API_KEY` | `dev-secret` | Bearer token for auth |
| `BRIDGE_PARALLELISM` | `4` | Concurrent publishes per batch |
| `BRIDGE_RETRY_MAX` | `5` | Max publish retries per record |
| `BRIDGE_RETRY_BASE_MS` | `200` | Base backoff in ms (exponential) |

## Topic mapping

By default the Kafka topic name becomes the HookLine topic directly:

```
Kafka: orders.created  →  HookLine topic: orders.created
```

With `KAFKA_TOPIC_PREFIX=prod.`:
```
Kafka: prod.orders.created  →  HookLine topic: orders.created
```

Subscribers on HookLine use glob patterns as usual (`orders.*`, `orders.#`).

## Running locally

```bash
# Start Kafka + bridge (HookLine should already be running)
cd bridge
docker compose up -d

# Publish a test Kafka message
docker compose exec kafka kafka-console-producer \
  --bootstrap-server localhost:9092 \
  --topic orders.created \
  --property "key.separator=:" \
  --property "parse.key=true" <<< 'order-001:{"order_id":"001","amount":99}'

# Watch HookLine deliveries
curl -s http://localhost:8080/v1/deliveries \
  -H "Authorization: Bearer dev-secret" | jq .
```

## Building

```bash
sbt assembly        # fat JAR → target/scala-3.3.3/hookline-bridge-assembly-0.1.0.jar
sbt test            # unit tests (no Kafka needed)
docker build -t hookline-bridge .
```

## Payload mapping

| Kafka value | HookLine payload |
|-------------|-----------------|
| `{"order_id":"1"}` (JSON object) | `{"order_id":"1"}` |
| `[1,2,3]` (JSON non-object) | `{"_value":[1,2,3]}` |
| `plain text` (not JSON) | `{"_raw":"plain text"}` |

When `key` is set on the Kafka record, `_kafka_key` is injected into the payload.
