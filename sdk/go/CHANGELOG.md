# Changelog

## [0.2.0] — 2026-02-25

### Added
- Admin client (`Client.Admin`) with `Tenants` sub-client: Create, List, Get, Delete
- WebSocket client already present from v0.1.0

### Removed
- `service_token` auth mode (use tenant-scoped API keys instead)

## [0.1.0] — initial release

### Added
- Initial SDK: Events, Endpoints, Subscriptions, DLQ, webhook verification
- WebSocket client (`Client.Connect`, `Client.ConnectWS`)
