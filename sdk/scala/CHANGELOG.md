# Changelog

## [0.2.0] — 2026-02-25

### Added
- Admin client (`HookLineClient.admin`) with `TenantsAdmin` sub-client: create, list, get, delete
- WebSocket client (`HookLineWS`) — ZIO-based, ZStream-backed event and presence streams
- `HookLineClient.connectWS()` factory for scoped WebSocket connections

### Removed
- `service_token` auth mode (use tenant-scoped API keys instead)

## [0.1.0] — initial release

### Added
- Initial SDK: Events, Endpoints, Subscriptions, Deliveries, DLQ, API Keys
- ZIO HTTP-based client with typed error channel (`HookLineError`)
