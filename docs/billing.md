# HookLine Billing

## Plans

| Plan       | Events/month | Endpoints  | Retention | WebSocket | Price/month |
|------------|-------------|------------|-----------|-----------|-------------|
| free       | 10,000       | 3          | 3 days    | No        | $0          |
| starter    | 100,000      | 10         | 7 days    | No        | $29         |
| growth     | 1,000,000    | unlimited  | 30 days   | Yes       | $99         |
| business   | 10,000,000   | unlimited  | 90 days   | Yes       | $399        |
| enterprise | unlimited    | unlimited  | custom    | Yes       | custom      |

Overage: **$1 per 10,000 events** above the monthly limit (paid plans only).

---

## Quota Enforcement

Quota enforcement is opt-in via the `HL_BILLING_ENABLED=true` environment variable (default: `false`, i.e. dev-mode passthrough — all requests allowed).

### Event quota (429 / 402)

`hl_billing:check_quota/1` is called from `hl_core:do_publish/3` before every event write.

| Status | Code | Meaning |
|--------|------|---------|
| `{error, quota_exceeded}` | **429** | Monthly event limit reached |
| `{error, subscription_inactive}` | **402** | Subscription is cancelled or past_due |
| `ok` | — | Event allowed |

### Endpoint quota (403)

`hl_billing:check_endpoint_quota/1` is called before endpoint creation.

Returns `{error, endpoint_limit_reached}` → **HTTP 403**.

### WebSocket feature gate (403)

`hl_billing:plan_feature(TenantId, websocket)` is checked in `hl_ws_handler:init/2`.

WebSocket is only available on the **growth** plan and above.

---

## Response Headers

Every successful event publish (`POST /v1/events → 201`) includes:

| Header | Description |
|--------|-------------|
| `X-RateLimit-Remaining` | Events remaining this billing period |
| `X-RateLimit-Reset` | Unix timestamp (ms) when the period resets |
| `X-Quota-Warning` | Present when ≥90% of quota consumed |

---

## Billing API

All endpoints require `Authorization: Bearer <token>` with `billing.read` (GET) or `billing.write` (POST/DELETE) scope.

### Subscription

```
GET /v1/billing/subscription
```

Response:
```json
{
  "plan": "starter",
  "status": "active",
  "current_period_end": 1740787200000,
  "trial_ends_at": null,
  "next_invoice_usd": 29
}
```

### Usage

```
GET /v1/billing/usage
```

Response:
```json
{
  "period": 1738281600000,
  "plan": "starter",
  "events": {
    "published": 42000,
    "delivered": 41800,
    "failed": 200,
    "limit": 100000,
    "remaining": 58000,
    "percent": 42,
    "overage": 0
  },
  "endpoints": { "limit": 10 },
  "websocket": false
}
```

### Invoices

```
GET  /v1/billing/invoices           → { "items": [...] }
GET  /v1/billing/invoices/:id       → single invoice
```

Invoice fields: `id`, `period_start`, `period_end`, `plan`, `base_amount_usd`, `overage_amount_usd`, `total_amount_usd`, `status`, `mg_checkout_url`, `paid_at`.

### Upgrade plan

```
POST /v1/billing/upgrade
{ "plan": "growth" }
```

Response (free plan):
```json
{ "status": "upgraded" }
```

Response (paid plan):
```json
{ "checkout_url": "https://pay.mashgate.io/checkout/abc123" }
```

### Payment methods

```
GET    /v1/billing/payment-methods        → { "items": [...] }
POST   /v1/billing/payment-methods        → add token
DELETE /v1/billing/payment-methods/:id    → remove
```

---

## mgPay Webhook Setup

1. In the Mashgate dashboard, add a webhook pointing to:
   ```
   POST https://your-hookline.example.com/v1/billing/webhooks/mgpay
   ```

2. Set `HL_MASHGATE_WEBHOOK_SECRET` to the secret shown in the dashboard.

3. The endpoint is **unauthenticated** — signature is verified via HMAC-SHA256.

   Signature header: `X-MgPay-Signature: v1=<hex_hmac_sha256>`

### Webhook events

| Event type | Action |
|------------|--------|
| `payment.captured` | Invoice marked `paid`, subscription remains `active` |
| `payment.failed` | Invoice marked `uncollectible`, subscription set to `past_due` |

---

## Admin API

All admin endpoints require `Authorization: Bearer <HL_ADMIN_KEY>`.

### List tenants with billing info

```
GET /v1/admin/billing/tenants
```

### Force plan change (no payment)

```
POST /v1/admin/billing/tenants/:id/plan
{ "plan": "growth" }
```

### Void invoice

```
POST /v1/admin/billing/invoices/:id/void
{ "tenant_id": "acme" }
```

### Grant trial

```
POST /v1/admin/billing/tenants/:id/trial
{ "plan": "starter", "trial_days": 14 }
```

---

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `HL_BILLING_ENABLED` | `false` | Enable quota enforcement |
| `HL_MASHGATE_URL` | `http://localhost:7000` | mgPay API base URL |
| `HL_MASHGATE_API_KEY` | — | mgPay API key |
| `HL_MASHGATE_WEBHOOK_SECRET` | — | Webhook HMAC secret |
| `HL_BILLING_RETURN_URL` | `http://localhost:8080` | Redirect after checkout |

---

## Internal Architecture

```
hl_billing.erl          Public API (check_quota, plan_feature, etc.)
hl_billing_plans.erl    Static plan config (no DB)
hl_billing_counters.erl ETS counters + 60s flush to C store
hl_billing_period.erl   Monthly close gen_server + invoice generation
hl_billing_store.erl    billing.* C store protocol wrappers
hl_billing_metrics.erl  Prometheus metrics
hl_mgpay_client.erl     gun HTTP client for mgPay checkout API
```

### Counter flush

ETS counters are flushed to the C store every 60 seconds (configurable via `counter_flush_interval_ms`). The hot path is `hl_billing_counters:track_event/2` → `ets:update_counter/3` — sub-microsecond, no gen_server roundtrip.

### Period close

The `hl_billing_period` gen_server calculates `time_until_period_end` on startup and fires `period_close` at the correct UTC month boundary. On close:

1. Flush ETS counters → C store
2. Calculate overage: `max(0, published - plan_limit)`
3. Write usage history entry
4. Create invoice (draft)
5. If payment method on file → call mgPay, update invoice to `open`
6. Roll subscription to new period
7. Reset ETS counters for new period
