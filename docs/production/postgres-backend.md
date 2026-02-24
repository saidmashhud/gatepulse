# PostgreSQL Backend

## Current status

PostgreSQL backend is **not production-ready** in the current HookLine runtime.

`HL_STORE_BACKEND=postgres` currently routes calls through a compatibility stub and
does not provide a native PostgreSQL storage implementation yet.

Reference implementation status:

- `/Users/saidmashhud/Projects/personal/hookline/apps/hl_store_client/src/hl_store_pg.erl`

## What this means for operators

- Use the default C store backend for production (`HL_STORE_BACKEND` unset / `c`).
- Do not plan HA/backup/migration strategy around PostgreSQL backend yet.
- If you set `HL_STORE_BACKEND=postgres` today, treat it as non-GA behavior.

## Why keep this page

This page remains as a roadmap placeholder for teams that need:

- SQL-native operational tooling
- PostgreSQL-centric backup/restore workflows
- DB-level introspection for webhook data

Until native implementation is delivered, these are **future goals**, not current capabilities.
