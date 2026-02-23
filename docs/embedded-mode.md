# HookLine — Embedded Mode (service_token)

Embedded mode lets Mashgate's `webhook-delivery` service use HookLine as its delivery data-plane without managing per-tenant API keys. Authentication is a single shared secret; tenant identity flows through the `X-Tenant-Id` request header.

## How it works

```
Mashgate webhook-delivery
  │
  │  Authorization: Bearer <HL_SERVICE_TOKEN>
  │  X-Tenant-Id: <tenant-uuid>
  │  POST /v1/events   (publish)
  │  POST /v1/endpoints (create endpoint)
  ▼
HookLine (HL_AUTH_MODE=service_token)
  ├── Validates Bearer token == HL_SERVICE_TOKEN
  ├── Reads tenant from X-Tenant-Id header (required)
  └── Routes request to tenant-scoped handlers
```

All existing HookLine endpoints (`/v1/events`, `/v1/endpoints`, `/v1/subscriptions`, `/v1/deliveries`, `/v1/replay`, `/v1/dlq`) work identically in embedded mode — the only difference is how tenant identity is established.

## Configuration

### HookLine environment

| Variable | Required | Description |
|----------|----------|-------------|
| `HL_AUTH_MODE` | yes | Set to `service_token` to enable embedded mode |
| `HL_SERVICE_TOKEN` | yes | Shared secret; must match `HOOKLINE_SERVICE_TOKEN` in Mashgate |
| `HL_SINGLE_TENANT` | no | Set to `false` for multi-tenant embedded mode (default: `true`) |
| `HL_WRITER_ID` | no | Erlang node name allowed to accept writes directly (e.g. `hookline@node1`). When set, non-writer nodes proxy event publishes to the writer. Omit in single-node deployments. |
| `HL_PUBLIC_URL` | when clustered | Public HTTP URL of the writer node; used by the leader proxy |

### Mashgate webhook-delivery environment

```env
HOOKLINE_URL=http://hookline:8080
HOOKLINE_AUTH_MODE=service_token
HOOKLINE_SERVICE_TOKEN=<shared-secret>   # must match HL_SERVICE_TOKEN
```

## Quick start

### 1. Create the shared network

```bash
./mashgate/infra/local/scripts/create-network.sh
```

### 2. Start HookLine in embedded mode

```bash
HL_AUTH_MODE=service_token \
HL_SERVICE_TOKEN=super-secret \
HL_SINGLE_TENANT=false \
docker compose -f hookline/docker-compose.yml up -d
```

### 3. Start Mashgate

```bash
HOOKLINE_AUTH_MODE=service_token \
HOOKLINE_SERVICE_TOKEN=super-secret \
docker compose -f mashgate/infra/local/docker-compose.yml up -d
```

### 4. Verify embedded mode is active

```bash
curl http://localhost:8080/v1/health/embedded
# {"embedded":true,"auth_mode":"service_token","writer_id":"","single_tenant":false}
```

## Tenant provisioning

In embedded mode, Mashgate creates HookLine tenants via `POST /v1/tenants` using the service token:

```bash
curl -X POST http://hookline:8080/v1/tenants \
  -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"id": "tenant-uuid", "name": "Acme Corp"}'
```

After creation, HookLine immediately warms the subscription cache and boots delivery actors for the new tenant — no restart required.

## Sending events (Mashgate → HookLine)

```bash
curl -X POST http://hookline:8080/v1/events \
  -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
  -H "X-Tenant-Id: tenant-uuid" \
  -H "Content-Type: application/json" \
  -d '{
    "event_type": "payment.captured",
    "payload": {"payment_id": "pay_123", "amount": 5000}
  }'
```

**Missing `X-Tenant-Id`** returns `400 X_TENANT_ID_REQUIRED` — not `401`. This is intentional: the service token is valid, but the request is malformed.

## Regression: api_key mode is unaffected

When `HL_AUTH_MODE=api_key` (default), the `service_token` path is completely inactive:

- `HL_SERVICE_TOKEN` is ignored
- `X-Tenant-Id` header is ignored for auth (it may still appear as a pass-through)
- All existing API key / ADMIN key behaviour is unchanged

## Security considerations

- **Rotate `HL_SERVICE_TOKEN`** on a schedule. Both HookLine and Mashgate must be updated atomically (rolling restart with the old token still accepted is recommended — add a `HL_SERVICE_TOKEN_OLD` grace period if needed).
- The `mashnet` Docker network isolates HookLine from the public internet. Port `8080` should **not** be published in production — only `mashgate-webhook-delivery` needs network access to HookLine.
- In `service_token` mode, **any caller with the token** can write to **any tenant** by changing `X-Tenant-Id`. The network boundary is the primary defence.

## Single-writer mode (`HL_WRITER_ID`)

In a multi-node HookLine cluster, only one node should append to the C store at a time to prevent log corruption. Set `HL_WRITER_ID` to the Erlang node name of the designated writer:

```env
HL_WRITER_ID=hookline@node1
```

Non-writer nodes that receive write requests proxy them to the leader via `hl_leader_proxy:forward/2`. The proxy uses the same service token for the forwarded request. Read requests (deliveries, DLQ, SSE) can be served by any node.

To check whether the current node is the designated writer:

```erlang
hl_leader_proxy:is_writer().
% → true | false
```
