# hl_store IPC Protocol

`hl_store` is a C11 daemon that manages the append-only log, hash index, job queue, and DLQ. The Erlang side (`hl_store_client`) communicates with it exclusively via a UNIX domain socket.

## Transport

- **Socket type:** `SOCK_STREAM` (UNIX domain)
- **Path:** configurable via `HL_STORE_SOCKET` (default: `/run/hookline/gp_store.sock`)
- **Framing:** length-prefixed — every message is preceded by a 4-byte big-endian `uint32` payload length

```
┌──────────────┬──────────────────────────────────┐
│  Length (4B) │  Payload (Length bytes, JSON)    │
│  big-endian  │                                  │
└──────────────┴──────────────────────────────────┘
```

All payloads are UTF-8 encoded JSON objects.

## Request Format

Every request from Erlang → C has the form:

```json
{
  "op":        "<operation>",
  "tenant_id": "<tenant>",
  ...operation-specific fields...
}
```

## Response Format

Every response from C → Erlang has the form:

```json
{
  "ok":    true | false,
  "error": "<error_code>",   // only when ok=false
  ...operation-specific fields...
}
```

## Operations

### `write_event`

Append an event to the log.

**Request:**
```json
{
  "op":        "write_event",
  "tenant_id": "acme",
  "event_id":  "evt_01HXYZ",
  "topic":     "orders.created",
  "payload":   { ... },
  "_trace":    { "traceparent": "00-...", "tracestate": "..." }
}
```

**Response:**
```json
{
  "ok":     true,
  "seg":    42,
  "offset": 1024
}
```

`seg` and `offset` form the cursor for SSE streaming.

---

### `read_event`

Read a single event by ID.

**Request:**
```json
{
  "op":        "read_event",
  "tenant_id": "acme",
  "event_id":  "evt_01HXYZ"
}
```

**Response:**
```json
{
  "ok":       true,
  "event_id": "evt_01HXYZ",
  "topic":    "orders.created",
  "payload":  { ... },
  "_trace":   { "traceparent": "00-..." }
}
```

---

### `list_events`

Paginated event listing by time range.

**Request:**
```json
{
  "op":        "list_events",
  "tenant_id": "acme",
  "from_ts":   1700000000000,
  "to_ts":     1700003600000,
  "limit":     100
}
```

**Response:**
```json
{
  "ok":     true,
  "items":  [ { ... } ],
  "cursor": "42:2048"
}
```

---

### `enqueue_job`

Add a delivery job to the queue.

**Request:**
```json
{
  "op":          "enqueue_job",
  "tenant_id":   "acme",
  "job_id":      "job_01HABC",
  "event_id":    "evt_01HXYZ",
  "endpoint_id": "ep_01HDEF",
  "attempt":     1,
  "deliver_at":  1700000000000
}
```

**Response:**
```json
{ "ok": true }
```

---

### `claim_jobs`

Claim pending jobs for delivery (atomic, at-most-once per worker).

**Request:**
```json
{
  "op":        "claim_jobs",
  "tenant_id": "acme",
  "limit":     16,
  "node_id":   "hookline@node1",
  "lease_secs": 30
}
```

**Response:**
```json
{
  "ok":   true,
  "jobs": [ { "job_id": "...", "event_id": "...", "endpoint_id": "...", "attempt": 1 } ]
}
```

---

### `ack_job`

Mark a delivery job as successfully delivered.

**Request:**
```json
{
  "op":          "ack_job",
  "tenant_id":   "acme",
  "job_id":      "job_01HABC",
  "http_status": 200,
  "latency_ms":  142
}
```

**Response:**
```json
{ "ok": true }
```

---

### `nack_job`

Mark a delivery job as failed; re-enqueue with backoff or move to DLQ.

**Request:**
```json
{
  "op":          "nack_job",
  "tenant_id":   "acme",
  "job_id":      "job_01HABC",
  "http_status": 503,
  "attempt":     3,
  "max_attempts": 10,
  "next_attempt_at": 1700000300000
}
```

**Response:**
```json
{ "ok": true, "dlq": false }
```

`dlq: true` means the job was moved to DLQ (max attempts exhausted).

---

### `list_dlq`

List DLQ entries for a tenant.

**Request:**
```json
{
  "op":        "list_dlq",
  "tenant_id": "acme",
  "limit":     50
}
```

**Response:**
```json
{
  "ok":      true,
  "entries": [ { "job_id": "...", "event_id": "...", "endpoint_id": "...", "failed_at": 1700000000000 } ]
}
```

---

### `requeue_dlq`

Move a DLQ entry back to the job queue.

**Request:**
```json
{
  "op":        "requeue_dlq",
  "tenant_id": "acme",
  "job_id":    "job_01HABC"
}
```

**Response:**
```json
{ "ok": true }
```

---

### `write_endpoint` / `read_endpoint` / `list_endpoints` / `delete_endpoint`

CRUD for endpoint metadata.

**Request (write):**
```json
{
  "op":          "write_endpoint",
  "tenant_id":   "acme",
  "endpoint_id": "ep_01HDEF",
  "url":         "https://your-app.example.com/webhooks",
  "secret":      "whsec_...",
  "enabled":     true,
  "max_in_flight": 8,
  "rate_limit_rps": 100,
  "max_retries":  10
}
```

---

### `write_subscription` / `list_subscriptions` / `delete_subscription`

CRUD for subscription metadata.

**Request (write):**
```json
{
  "op":              "write_subscription",
  "tenant_id":       "acme",
  "subscription_id": "sub_01HGHI",
  "endpoint_id":     "ep_01HDEF",
  "topic_pattern":   "orders.#"
}
```

---

### `queue_stats`

Queue depth and inflight count for a tenant.

**Request:**
```json
{
  "op":        "queue_stats",
  "tenant_id": "acme"
}
```

**Response:**
```json
{
  "ok":          true,
  "queue_depth": 42,
  "in_flight":   8
}
```

---

## Error Codes

| Code | Meaning |
|------|---------|
| `not_found` | Entity does not exist |
| `already_exists` | Duplicate key on write |
| `io_error` | C store disk I/O failure |
| `corrupt` | CRC32 check failed on read |
| `unknown_op` | Unrecognised `op` field |
| `invalid_request` | Missing required field |

## Invariants

1. **Append-only log** — events are never modified or deleted; compaction removes segments outside the retention window.
2. **CRC32 integrity** — every log segment entry includes a CRC32 checksum; reads verify it.
3. **At-most-once claim** — `claim_jobs` is atomic; the same job cannot be claimed by two workers simultaneously.
4. **Lease expiry** — orphaned claimed jobs (node crashed mid-delivery) are returned to the queue after `lease_secs` seconds.
5. **DLQ permanence** — DLQ entries are not automatically deleted; they must be explicitly requeued or purged.
6. **Tenant isolation** — all operations include `tenant_id`; the C layer enforces it at the storage key level.
