# HookLine

> **What is HookLine?** A single-binary, Docker-first webhook delivery service. Drop it into any stack — publish events via REST, and HookLine durably delivers them to your HTTP endpoints with retries, signatures, and replay. No external dependencies, no JVM, no message broker. Just `docker run`.

**HookLine** is an open-source webhook delivery service — a self-hosted alternative to Svix/Hookdeck. It accepts events, durably stores them in a custom C-based append-only log, and delivers them to your HTTP endpoints with retries, signatures, and replay.

## Features

- **At-least-once delivery** with exponential backoff and jitter
- **Per-endpoint concurrency** (`max_in_flight`) and **rate limiting** (`rate_limit_rps`)
- **HMAC-SHA256 signatures** on every webhook request
- **Dead Letter Queue** — failed events stored, inspectable, and requeueable
- **Replay** — re-deliver events by `event_id`, time range, topic, or endpoint
- **Real-time SSE stream** — subscribe to events with topic filtering and `Last-Event-ID` resume
- **WebSocket transport** — bidirectional, browser-native; publish + subscribe + presence + offline queue
- **Presence system** — track online users per topic, `GET /v1/presence/{topic}`
- **Dev Inbox** — built-in test endpoint for local development
- **Prometheus metrics** at `/metrics`
- **Single Docker container** — no external dependencies

## Multi-tenancy

HookLine supports both single-tenant (default) and multi-tenant deployments.
In multi-tenant mode, each tenant gets isolated endpoints, events, and API keys.

```bash
HL_ADMIN_KEY=<admin-secret> \
HL_SINGLE_TENANT=false \
docker compose up -d
```

See [docs/multi-tenancy.md](docs/multi-tenancy.md) for the full setup guide.

---

## Quickstart

```bash
# Start HookLine
docker-compose up -d

# Create a webhook endpoint
curl -X POST http://localhost:8080/v1/endpoints \
  -H "Authorization: Bearer dev-secret" \
  -H "Content-Type: application/json" \
  -d '{"url":"http://localhost:3001/webhook","enabled":true,"secret":"my-secret"}'

# Create a subscription (endpoint → topic)
curl -X POST http://localhost:8080/v1/subscriptions \
  -H "Authorization: Bearer dev-secret" \
  -H "Content-Type: application/json" \
  -d '{"endpoint_id":"<endpoint_id>","topic_pattern":"orders.#"}'

# Publish an event
curl -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer dev-secret" \
  -H "Content-Type: application/json" \
  -d '{"topic":"orders.created","payload":{"order_id":"123","amount":99}}'

# Check delivery attempts
curl "http://localhost:8080/v1/deliveries?event_id=<event_id>" \
  -H "Authorization: Bearer dev-secret"
```

---

## Dev Inbox

Create a local webhook receiver without any external service:

```bash
# Create an inbox
INBOX=$(curl -s -X POST http://localhost:8080/v1/dev/inbox \
  -H "Authorization: Bearer dev-secret" -d '{}')
TOKEN=$(echo $INBOX | jq -r .token)
RECEIVE_URL=$(echo $INBOX | jq -r .receive_url)

# Use the receive URL as your endpoint URL
# View received webhooks
curl "http://localhost:8080/v1/dev/inbox/messages?token=$TOKEN" \
  -H "Authorization: Bearer dev-secret" | jq .
```

---

## WebSocket

HookLine provides a bidirectional WebSocket endpoint at `/v1/ws` that shares the same pubsub bus as SSE. Every `publish_event` call delivers to SSE and WS subscribers simultaneously.

```bash
# Connect (requires websocat)
websocat "ws://localhost:8080/v1/ws?token=$HL_API_KEY&topics=orders.#"

# Publish a message over WebSocket
{"type":"publish","topic":"orders.created","payload":{"id":"123"},"client_id":"1"}

# Expected ack:   {"type":"ack","client_id":"1","event_id":"evt_..."}
# Expected event: {"type":"event","id":"evt_...","topic":"orders.created",...}
```

**Supported client message types:** `publish`, `subscribe`, `unsubscribe`, `read`

**Supported server message types:** `event`, `ack`, `presence`

### Presence

```bash
curl http://localhost:8080/v1/presence/orders.# \
  -H "Authorization: Bearer $HL_API_KEY"
# {"online":[{"user_id":"alice","joined_at":...}],"count":1}
```

### JavaScript SDK

```ts
const ws = client.connect({
  topics: ["orders.#"],
  onEvent:    (e) => console.log(e),
  onPresence: (p) => console.log(p),
});
ws.publish("orders.created", { id: "123" });
```

### Go SDK

```go
wsc, _ := client.ConnectWS(hookline.WSOptions{
    Topics:  []string{"orders.#"},
    OnEvent: func(e hookline.WSEvent) { fmt.Println(e) },
})
_ = wsc.Publish("orders.created", map[string]any{"id": "123"}, "")
```

See [docs/websocket.md](docs/websocket.md) for the full protocol reference.

---

---

## CLI

The CLI script is `bin/hl`:

```bash
export HL_URL=http://localhost:8080
export HL_API_KEY=dev-secret

./bin/hl endpoints add --url https://my.app/hooks --secret s3cr3t
./bin/hl subs add --endpoint <id> --topic "orders.#"
./bin/hl events publish --topic orders.created --payload '{"id":1}'
./bin/hl deliveries ls --event <event_id>
./bin/hl dlq ls
./bin/hl replay --event <event_id>
./bin/hl stream --topic "orders.#"
./bin/hl inbox create
```

---

## W3C Trace Context

HookLine propagates [W3C Trace Context](https://www.w3.org/TR/trace-context/) headers through the delivery pipeline. Include `traceparent` (and optionally `tracestate`) when publishing events — they will be forwarded in webhook delivery requests.

```bash
curl -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer dev-secret" \
  -H "traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" \
  -d '{"topic":"orders.created","payload":{"order_id":"123"}}'
```

## Webhook Signature Verification

Every request includes:
- `X-GP-Event-Id` — unique event identifier
- `X-GP-Topic` — event topic
- `X-GP-Delivery-Id` — delivery attempt identifier
- `X-GP-Timestamp` — Unix timestamp in milliseconds
- `X-GP-Signature` — HMAC-SHA256 signature
- `traceparent` — W3C Trace Context (if provided on publish)
- `tracestate` — W3C Trace Context (if provided on publish)

**Signature format:** `v1=<hex_hmac_sha256(secret, "{timestamp}.{body}")>`

### Node.js
```javascript
const crypto = require('crypto');

function verifySignature(secret, timestamp, body, signature) {
  const expected = 'v1=' + crypto
    .createHmac('sha256', secret)
    .update(`${timestamp}.${body}`)
    .digest('hex');
  return crypto.timingSafeEqual(
    Buffer.from(signature),
    Buffer.from(expected)
  );
}

app.post('/webhook', (req, res) => {
  const ts  = req.headers['x-gp-timestamp'];
  const sig = req.headers['x-gp-signature'];
  if (!verifySignature('my-secret', ts, req.body, sig)) {
    return res.status(401).send('Unauthorized');
  }
  // Process idempotently using x-gp-event-id
  const eventId = req.headers['x-gp-event-id'];
  // ... handle event
  res.sendStatus(200);
});
```

### Python
```python
import hmac, hashlib

def verify_signature(secret: str, timestamp: str, body: bytes, signature: str) -> bool:
    msg = f"{timestamp}.".encode() + body
    expected = "v1=" + hmac.new(secret.encode(), msg, hashlib.sha256).hexdigest()
    return hmac.compare_digest(expected, signature)

@app.route('/webhook', methods=['POST'])
def webhook():
    ts  = request.headers.get('X-GP-Timestamp')
    sig = request.headers.get('X-GP-Signature')
    if not verify_signature('my-secret', ts, request.data, sig):
        return 'Unauthorized', 401
    event_id = request.headers.get('X-GP-Event-Id')
    # Idempotency: skip if event_id already processed
    return 'OK', 200
```

---

## Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `HL_PORT` | `8080` | HTTP listen port |
| `HL_LISTEN_ADDR` | `0.0.0.0` | HTTP bind address |
| `HL_API_KEY` | — | API key (required in `api_key` mode unless `HL_ADMIN_KEY` is set) |
| `HL_ADMIN_KEY` | — | Optional admin override key |
| `HL_TENANT_ID` | `default` | Tenant ID (single-tenant mode) |
| `HL_SINGLE_TENANT` | `true` | Single vs multi-tenant mode |
| `HL_STORE_SOCKET` | `/tmp/hl_store.sock` | Path to C store daemon socket |
| `HL_DATA_DIR` | `/var/lib/hookline` | Storage directory |
| `HL_RETRY_MAX_ATTEMPTS` | `10` | Max delivery attempts |
| `HL_RETENTION_SECS` | `604800` | Event retention (7 days) |
| `HL_DELIVERY_WORKERS` | `16` | Concurrent delivery workers |
| `HL_CONSOLE_ENABLED` | `true` | Enable/disable the web console |
| `HL_CONSOLE_PASS` | — | Optional Basic Auth password for `/console` (user: `admin`) |

---

## API Reference

Canonical OpenAPI source is [`contracts/openapi.yaml`](contracts/openapi.yaml).
Runtime endpoint **`/openapi.yaml`** serves the same contract.

Key endpoints:

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/v1/events` | Publish an event |
| `GET`  | `/v1/events` | List events (paginated) |
| `GET`  | `/v1/events/:id` | Get event by ID |
| `POST` | `/v1/endpoints` | Create endpoint |
| `GET`  | `/v1/endpoints` | List endpoints |
| `PATCH`| `/v1/endpoints/:id` | Update endpoint |
| `DELETE`| `/v1/endpoints/:id` | Delete endpoint |
| `POST` | `/v1/subscriptions` | Create subscription |
| `GET`  | `/v1/subscriptions` | List subscriptions |
| `DELETE`| `/v1/subscriptions/:id` | Delete subscription |
| `GET`  | `/v1/deliveries` | List delivery attempts |
| `GET`  | `/v1/deliveries/:id` | Get attempt by ID |
| `GET`  | `/v1/dlq` | List DLQ entries |
| `POST` | `/v1/dlq/:id/requeue` | Requeue DLQ entry |
| `DELETE`| `/v1/dlq/:id` | Delete DLQ entry |
| `POST` | `/v1/replay` | Replay events |
| `GET`  | `/v1/replay/:id` | Replay status |
| `GET`  | `/v1/stream` | SSE event stream |
| `GET`  | `/healthz` | Liveness probe |
| `GET`  | `/readyz` | Readiness probe |
| `GET`  | `/metrics` | Prometheus metrics |

---

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                   HookLine (Erlang/OTP)            │
│                                                     │
│  hl_api ──► hl_core ──► hl_store_client ──┐        │
│     │                                      │        │
│     └──► hl_stream (SSE)      hl_delivery ─┤        │
│                                            │        │
│  hl_dev_inbox (ETS)                        │        │
└────────────────────────────────────────────┼────────┘
                                             │ UNIX socket
                                             ▼
                              ┌─────────────────────────┐
                              │  hl_store (C daemon)    │
                              │                         │
                              │  append-only log        │
                              │  hash index             │
                              │  job queue              │
                              │  DLQ                    │
                              └─────────────────────────┘
```

**hl_api** — Cowboy HTTP server, auth middleware, all REST handlers
**hl_core** — validation, topic matching, delivery job creation
**hl_delivery** — poller + workers: claim → HTTP POST → ack/nack/DLQ
**hl_stream** — SSE loop handler with pubsub and replay
**hl_store** — C11 daemon: segmented append-only log with CRC32 integrity

---

## Development

**Requirements:** Erlang/OTP 28+, rebar3, GCC, Docker

```bash
# Build C daemon
cd c && make

# Run C tests
cd c && make test && ./build/gp_store_tests

# Build Erlang
rebar3 compile

# Run Erlang unit tests
rebar3 eunit

# Run integration tests (requires running instance)
HL_URL=http://localhost:8080 HL_API_KEY=<api-key> ./test/integration.sh

# Run trace propagation tests
HL_URL=http://localhost:8080 HL_API_KEY=<api-key> ./test/trace-propagation.sh

# Run production-readiness gate (load + soak + chaos)
HL_URL=http://localhost:8080 HL_AUTH_MODE=api_key HL_API_KEY=<api-key> ./test/production-readiness.sh

# Start locally
docker-compose up
```

---

## Used by

**[Mashgate](https://github.com/saidmashhud/mashgate)** — open-source payment infrastructure for Central Asia — uses HookLine for webhook delivery.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Security

See [SECURITY.md](SECURITY.md) for reporting vulnerabilities.

## License

Apache-2.0 — see [LICENSE](LICENSE).
