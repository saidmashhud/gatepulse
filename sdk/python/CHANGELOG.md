# Changelog

## [0.2.0] — 2026-02-25

### Added
- Admin client (`HookLineClient.admin`) with `Tenants` sub-client: create, list, get, delete
- WebSocket client (`HookLineWS`) — asyncio-based, auto-reconnect with exponential backoff
- `websockets>=12.0` runtime dependency

### Removed
- `service_token` auth mode (use tenant-scoped API keys instead)

## [0.1.0] — initial release

### Added
- Initial SDK: Events, Endpoints, Subscriptions, DLQ, webhook verification
