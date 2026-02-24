# Changelog

All notable changes to HookLine will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Full release notes for each version are in [`docs/releases/`](docs/releases/).

## [Unreleased]

### Removed
- `HL_EMBEDDED_MODE`, `HL_SERVICE_TOKEN`, `HL_AUTH_MODE=service_token` — embedded mode removed; isolation is now handled at the network layer. See [migration guide](docs/multi-tenancy.md#migrating-from-embedded-mode).
- `/v1/health/embedded` endpoint — no longer needed.
- `test/embedded-mode.sh` — test suite for removed feature.
- `docs/embedded-mode.md` — documentation for removed feature.

### Changed
- Multi-tenant startup (`HL_SINGLE_TENANT=false`) now uses `HL_SINGLE_TENANT` to determine cache warm strategy instead of `HL_AUTH_MODE`.

## [2.1.0]

### Added
- W3C Trace Context propagation (`traceparent`, `tracestate`) stored and forwarded on delivery
- `_trace` metadata field on events
- Integration test: `test/trace-propagation.sh`

## [2.0.0]

> **Breaking changes** — see [migration guide](docs/releases/v2.0.md#migration-guide).
> PostgreSQL backend and end-to-end secrets-at-rest rotation are not yet GA.

### Added
- Scopes / RBAC: API keys carry scopes (`events:write`, `events:read`, `endpoints:manage`, etc.)
- Audit log: all mutating operations logged; queryable via `GET /v1/admin/audit`
- Event filter DSL per subscription (`eq/neq/gt/gte/lt/lte/contains/and/or/not`)
- Transform DSL per subscription (`rename/add/remove/set` fields at delivery time)
- Web console at `/console/` — full CRUD UI, no build step
- `HL_MASTER_KEY` + `POST /v1/admin/rotate-secrets` API surface (encryption workflow in progress)
- `HL_STORE_BACKEND` config key (`c` or `postgres`; Postgres backend non-GA)
- `HL_LEGACY_KEY_GRACE` for backward-compatible pre-v2 keys

### Breaking
- API keys without scopes treated as full-scope during grace period (`HL_LEGACY_KEY_GRACE=true`)
- Endpoint `secret` field is write-only on GET responses (returned as `"***"`)

## [1.3.0]

### Added
- Leader election via Erlang `global` — only leader runs delivery poller + worker pool
- Job lease ownership (`node_id`, `lease_until`); orphaned jobs reaped automatically
- Helm chart (`helm/hookline/`) — API deployment + store StatefulSet with PVC, HPA, configmap
- `docs/production/ha-deployment.md`
- `HL_NODE_NAME`, `HL_COOKIE`, `HL_JOB_LEASE_SECS` env vars

## [1.2.0]

### Added
- SSE cursor & resume: `Last-Event-ID` + `?from_cursor=SEG:OFFSET`
- Historical backfill on SSE connect (`?from_cursor=0:0`)
- Delivery status stream: all attempts published to `deliveries.status` SSE topic
- Dev Inbox UI at `/v1/dev/inbox/ui?token=TOKEN`
- Stream metrics: `hookline_stream_clients`, `hookline_stream_events_sent_total`
- `HL_STREAM_BACKFILL_MAX_EVENTS`, `HL_STREAM_BACKFILL_MAX_AGE_DAYS` env vars

## [1.1.0]

### Added
- Backpressure: 413 on payloads > 512 KB, 429 + `Retry-After` on queue overflow
- Admin API: `GET /v1/admin/stats`, `POST /v1/admin/compact`, pause/resume job claiming
- Backup CLI: `gp store snapshot create` / `gp store snapshot restore`
- Grafana dashboard (`grafana/dashboards/gatepulse.json`) — 11 panels
- Prometheus alert rules (`prometheus/alerts.yml`)
- CLI: `gp init`, `gp doctor`, `gp tail`
- SDKs: JavaScript (`sdk/js/`) and Python (`sdk/python/`)
- `HL_MAX_PAYLOAD_BYTES`, `HL_MAX_QUEUE_DEPTH`, `HL_MAX_INFLIGHT_GLOBAL`, `HL_COMPACTION_INTERVAL_SECS` env vars

## [0.1.0]

### Added
- Initial release: C store backend, REST API, retries, DLQ, replay, SSE, and SDKs.
