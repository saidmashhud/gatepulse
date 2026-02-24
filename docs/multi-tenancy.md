# HookLine — Multi-Tenancy

HookLine supports multi-tenant deployments where each tenant has isolated endpoints, subscriptions, events, and delivery actors.

## Enabling Multi-Tenant Mode

```bash
docker run \
  -e HL_ADMIN_KEY=<admin-secret> \
  -e HL_SINGLE_TENANT=false \
  -e HL_DATA_DIR=/var/lib/hookline \
  -v hookline_data:/var/lib/hookline \
  -p 8080:8080 \
  hookline:latest
```

| Variable | Default | Description |
|----------|---------|-------------|
| `HL_SINGLE_TENANT` | `true` | Set to `false` to enable multi-tenant mode |
| `HL_ADMIN_KEY` | — | Admin API key — required for tenant management |
| `HL_API_KEY` | — | Optional global key (single-tenant backward compat) |

In single-tenant mode (`HL_SINGLE_TENANT=true`), the tenant is always `HL_TENANT_ID` (default: `"default"`). In multi-tenant mode, each API key is scoped to a tenant.

## Managing Tenants

All tenant management operations require the `admin` scope (use `HL_ADMIN_KEY`).

### Create a tenant

```bash
curl -X POST http://localhost:8080/v1/tenants \
  -H "Authorization: Bearer <admin-secret>" \
  -H "Content-Type: application/json" \
  -d '{"id": "acme", "name": "ACME Corp"}'
```

Response:
```json
{
  "tenant_id": "acme",
  "name": "ACME Corp",
  "api_key": "hl_live_abc123..."
}
```

Store the `api_key` — it is only returned on creation.

### Issue an additional API key

```bash
curl -X POST http://localhost:8080/v1/tenants/acme/api-keys \
  -H "Authorization: Bearer <admin-secret>" \
  -H "Content-Type: application/json" \
  -d '{"label": "production", "scopes": ["events.publish", "deliveries.read"]}'
```

### List tenants

```bash
curl http://localhost:8080/v1/tenants \
  -H "Authorization: Bearer <admin-secret>"
```

### Delete a tenant

Purges all endpoints, subscriptions, events, DLQ entries, and API keys for the tenant.

```bash
curl -X DELETE http://localhost:8080/v1/tenants/acme \
  -H "Authorization: Bearer <admin-secret>"
```

## Using a Tenant API Key

All data-plane operations include the tenant context in the API key — no `X-Tenant-Id` header is needed:

```bash
# Publish an event as tenant "acme"
curl -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer hl_live_abc123..." \
  -H "Content-Type: application/json" \
  -d '{"topic": "orders.created", "payload": {"order_id": "123"}}'
```

Tenant data is fully isolated — one tenant cannot read or write another tenant's data.

## Integrating a Control-Plane Service

When a service (e.g. Mashgate `mg-events`) manages webhooks on behalf of your application:

1. Create a tenant for the service:
   ```bash
   curl -X POST http://localhost:8080/v1/tenants \
     -H "Authorization: Bearer <admin-secret>" \
     -d '{"id": "mashgate", "name": "Mashgate Platform"}'
   # → { "api_key": "hl_live_..." }
   ```

2. Store the API key in the service's environment:
   ```
   HOOKLINE_API_KEY=hl_live_...
   ```

3. Restrict admin API access at the network layer (Docker network, Kubernetes NetworkPolicy) so only the admin can reach `/v1/tenants` and `/v1/admin`. The tenant API key alone cannot access those endpoints (it lacks the `admin` scope).

## Migrating from Embedded Mode

Prior versions of HookLine supported `HL_AUTH_MODE=service_token` + `HL_EMBEDDED_MODE=true`. This has been removed.

**Migration steps:**

1. If you were using `service_token` auth, switch to `api_key` auth:
   - Remove `HL_AUTH_MODE`, `HL_SERVICE_TOKEN`, `HL_EMBEDDED_MODE` from your environment
   - Add `HL_ADMIN_KEY` for tenant management
   - Create a tenant for your control-plane service and store its API key

2. Update your control-plane to pass `Authorization: Bearer <tenant-api-key>` instead of `Authorization: Bearer <service-token>` + `X-Tenant-Id` header. The tenant is now encoded in the key — no separate header is needed.

3. Apply network-level isolation to protect admin endpoints instead of relying on `HL_EMBEDDED_MODE` scope restriction.

## Startup Behavior

When `HL_SINGLE_TENANT=false`, HookLine loads all active tenants from the store at startup:
- Subscription cache warmed for all tenants
- Delivery actors booted for all tenants' enabled endpoints

New tenants created via `POST /v1/tenants` are warmed immediately — no restart needed.
