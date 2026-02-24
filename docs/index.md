# HookLine

**HookLine** is an open-source webhook delivery service built for reliability, performance, and developer experience. It handles fan-out delivery, retries, dead-letter queues, signatures, and real-time streaming out of the box.

## Features

| Feature | Description |
|---------|-------------|
| **Durable storage** | Append-only segmented log in C11, CRC32 integrity, 256 MB segments |
| **At-least-once delivery** | Exponential backoff with full jitter, configurable retry count |
| **Dead letter queue** | Failed deliveries inspectable and requeueable via API |
| **HMAC-SHA256 signatures** | Per-endpoint signing with constant-time verification |
| **Topic routing** | Glob patterns (`orders.*`, `events.#`) with fan-out to N subscriptions |
| **Filter DSL** | Per-subscription JSON filter (eq/gt/lt/contains/and/or/not) on event payload |
| **Transform DSL** | Rename, add, remove, or override fields before delivery |
| **SSE stream** | Real-time event feed with cursor-based resume (`Last-Event-ID`) |
| **Delivery status stream** | Live `deliveries.status` events on SSE after each attempt |
| **Developer inbox** | Built-in test receiver with browser UI at `/v1/dev/inbox/ui` |
| **Prometheus metrics** | Throughput, latency (P50/P95/P99), DLQ, SSE, store IPC |
| **Admin API** | Stats, compaction, pause/resume, audit log, secrets rotation |
| **Web console** | Full CRUD UI at `/console/` (vanilla JS, no build step) |
| **Backup CLI** | `./bin/hl store snapshot create/restore` with safe pause/rsync/resume |

## Quick Start

```bash
# 1. Start
HL_API_KEY=my-secret docker compose up -d

# 2. Create a test inbox (no external server needed)
INBOX=$(curl -s -X POST http://localhost:8080/v1/dev/inbox \
  -H "Authorization: Bearer my-secret" | python3 -m json.tool)
echo $INBOX

TOKEN=$(echo $INBOX | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
RECV_URL=$(echo $INBOX | python3 -c "import sys,json; print(json.load(sys.stdin)['receive_url'])")

# 3. Create endpoint + subscription
EP_ID=$(curl -s -X POST http://localhost:8080/v1/endpoints \
  -H "Authorization: Bearer my-secret" \
  -H "Content-Type: application/json" \
  -d "{\"url\":\"$RECV_URL\",\"secret\":\"my-hmac-secret\",\"enabled\":true}" \
  | python3 -c "import sys,json; print(json.load(sys.stdin)['endpoint_id'])")

curl -s -X POST http://localhost:8080/v1/subscriptions \
  -H "Authorization: Bearer my-secret" \
  -H "Content-Type: application/json" \
  -d "{\"endpoint_id\":\"$EP_ID\",\"topic_pattern\":\"orders.*\"}"

# 4. Publish an event
curl -s -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer my-secret" \
  -H "Content-Type: application/json" \
  -d '{"topic":"orders.created","payload":{"order_id":"123","amount":99}}'

# 5. Check inbox (wait ~1s for delivery)
sleep 2
curl -s "http://localhost:8080/v1/dev/inbox/messages?token=$TOKEN" | python3 -m json.tool

# Or open the UI:
open "http://localhost:8080/v1/dev/inbox/ui?token=$TOKEN"
```

## Architecture

```
┌──────────────────────────────────────────────────────────┐
│  HookLine Container                                     │
│                                                          │
│  ┌───────────┐  UNIX socket  ┌────────────────────────┐ │
│  │  hl_store │◄─────────────►│   Erlang/OTP Node      │ │
│  │  (C11)    │  len-prefixed │                        │ │
│  │           │  JSON IPC     │  hl_api   — HTTP/REST  │ │
│  │ segments  │               │  hl_core  — routing    │ │
│  │ + index   │               │  hl_delivery — workers │ │
│  │ + queue   │               │  hl_stream — SSE       │ │
│  │ + DLQ     │               │  hl_cluster — leader   │ │
│  └───────────┘               └────────────────────────┘ │
│                                                          │
│  HTTP :8080                                              │
└──────────────────────────────────────────────────────────┘
```

### Event lifecycle

```
Publish → validate → store (append-only) → route → filter → fan-out jobs
  └→ poller claims job → worker HTTP POST → ack/retry/DLQ
                                          └→ SSE deliveries.status event
```

## Testing

```bash
# Smoke tests (23 checks)
HL_API_KEY=change-me-in-production bash test/integration.sh

# Complex E2E scenarios (16 scenarios)
HL_API_KEY=change-me-in-production bash test/e2e.sh
```

## Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `HL_PORT` | `8080` | HTTP listen port |
| `HL_API_KEY` | — | Master API key (required) |
| `HL_TENANT_ID` | `default` | Default tenant |
| `HL_DELIVERY_WORKERS` | `16` | Concurrent delivery workers |
| `HL_MAX_PAYLOAD_BYTES` | `524288` | Max event body size (512 KB) |
| `HL_MAX_QUEUE_DEPTH` | `100000` | Queue depth before 429 |
| `HL_RETENTION_SECS` | `604800` | Segment retention (7 days) |
| `HL_DATA_DIR` | `/var/lib/hookline` | Store data directory |

Full reference → [`docs/production/checklist.md`](production/checklist.md)

## API Reference

| Resource | Endpoints |
|----------|-----------|
| Events | `POST /v1/events` · `GET /v1/events` · `GET /v1/events/:id` |
| Endpoints | `POST /v1/endpoints` · `GET` · `PATCH /:id` · `DELETE /:id` |
| Subscriptions | `POST /v1/subscriptions` · `GET` · `DELETE /:id` |
| Deliveries | `GET /v1/deliveries` · `GET /v1/deliveries/:id` |
| Replay | `POST /v1/replay` · `GET /v1/replay/:id` |
| DLQ | `GET /v1/dlq` · `POST /v1/dlq/:id/requeue` · `DELETE /v1/dlq/:id` |
| Stream (SSE) | `GET /v1/stream?topic=PATTERN` |
| Dev Inbox | `POST /v1/dev/inbox` · `GET /v1/dev/inbox/ui` · `GET /v1/dev/inbox/messages` |
| Admin | `GET /v1/admin/stats` · `POST /v1/admin/compact` · `GET /v1/admin/audit` |
| API Keys | `POST /v1/apikeys` · `GET /v1/apikeys` · `DELETE /v1/apikeys/:id` |

Canonical OpenAPI source is `openapi/openapi.yaml`; runtime serves it at `/openapi.yaml`.

## License

Apache 2.0
