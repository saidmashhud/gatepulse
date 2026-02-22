# PostgreSQL Backend

HookLine v2.0 supports PostgreSQL as an alternative to the embedded C store daemon.
Use this when you need stronger consistency guarantees, existing PostgreSQL infrastructure,
or want to use standard database tooling for backups and observability.

## Prerequisites

- PostgreSQL 14+
- A dedicated database (e.g. `hookline`)
- A user with `CREATE TABLE`, `INSERT`, `UPDATE`, `DELETE`, `SELECT` on that database

## Setup

### 1. Create the database

```sql
CREATE DATABASE hookline;
CREATE USER hookline_user WITH PASSWORD 'your-password';
GRANT ALL PRIVILEGES ON DATABASE hookline TO hookline_user;
```

### 2. Run migrations

```bash
psql postgres://hookline_user:your-password@localhost:5432/hookline \
  -f priv/sql/001_initial.sql
```

The migration creates the following tables:
`events`, `endpoints`, `subscriptions`, `jobs`, `attempts`, `dlq`, `api_keys`

### 3. Configure HookLine

```bash
export HL_STORE_BACKEND=postgres
export HL_POSTGRES_URL=postgres://hookline_user:your-password@localhost:5432/hookline
```

Or in `docker-compose.yml`:

```yaml
environment:
  HL_STORE_BACKEND: postgres
  HL_POSTGRES_URL: postgres://hookline_user:your-password@db:5432/hookline
```

## Switching from C Store to PostgreSQL

1. Stop HookLine
2. Take a final snapshot: `gp store snapshot create --dest /tmp/final-snapshot`
3. Run the migration script to import snapshot data into Postgres:
   ```bash
   # (manual migration — export events/endpoints/subscriptions via API and re-import)
   ```
4. Set `HL_STORE_BACKEND=postgres` and start HookLine
5. Verify with `gp doctor`

> **Note:** There is no automated migration tool from the C store format to PostgreSQL.
> For production migrations, stop ingestion, export data via the REST API, and replay.

## Differences from C Store

| Feature | C Store | PostgreSQL |
|---------|---------|-----------|
| Backup | Pause/rsync/resume | `pg_dump` / streaming replication |
| Scalability | Single node | Connection pooling (PgBouncer) |
| Query flexibility | Cursor-based scan | Full SQL |
| Operational complexity | Zero deps | Requires Postgres |
| Max throughput | ~50k events/s | ~10k events/s (single PG) |

## Connection Pooling

For production, use PgBouncer in transaction mode:

```ini
[databases]
hookline = host=localhost port=5432 dbname=hookline

[pgbouncer]
pool_mode = transaction
max_client_conn = 1000
default_pool_size = 50
```

Set `HL_POSTGRES_URL` to point at PgBouncer instead of PostgreSQL directly.

## Monitoring

Key queries for observability:

```sql
-- Queue depth by tenant
SELECT tenant_id, COUNT(*) FROM jobs WHERE status = 'pending' GROUP BY tenant_id;

-- DLQ size
SELECT COUNT(*) FROM dlq;

-- Failed deliveries in last hour
SELECT COUNT(*) FROM attempts
WHERE status = 'failure' AND started_at > NOW() - INTERVAL '1 hour';

-- P99 latency (milliseconds)
SELECT PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY duration_ms) AS p99
FROM attempts
WHERE started_at > NOW() - INTERVAL '5 minutes';
```
