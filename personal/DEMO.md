# Demo: Mashgate + HookLine + Zist — Full Stack

This demo runs the complete integration stack locally using Docker Compose.

## What's included

| Service | URL | Description |
|---------|-----|-------------|
| **Zist** | http://localhost:8000 | Rental marketplace (main entry point) |
| **Mashgate** | http://localhost:9660 | Payment platform frontend |
| **Mashgate API** | http://localhost:9661 | REST/gRPC gateway |
| **HookLine** | http://localhost:8080 | Webhook delivery service |

## Quick Start

### Step 1 — Start all services

```bash
docker compose -f docker-compose.demo.yml up -d
```

Wait ~60 seconds for all services to become healthy:

```bash
docker compose -f docker-compose.demo.yml ps
```

All services should show `healthy` or `running`.

### Step 2 — Provision Zist in Mashgate

```bash
MGID_URL=http://localhost:9661 \
MGID_ADMIN_TOKEN=<your-admin-token> \
./zist/scripts/provision-mashgate-dev.sh
```

This will:
- Create the Zist tenant in Mashgate
- Register the `zist-local` OAuth client
- Register all Zist app scopes (listings, bookings, payments, webhooks)
- Create `zist_admin`, `zist_operator`, `zist_viewer` roles

Copy the printed `.env` values and set them (or add to a `.env` file at the project root).

### Step 3 — Open the app

```bash
open http://localhost:8000
```

## Full Demo Flow

1. **Login** — Sign in with your Zist credentials via mgID OIDC
2. **Browse listings** — View available rentals
3. **Book** — Create a booking (status: `pending`)
4. **Pay** — Complete checkout via Mashgate hosted payment page
5. **Webhook** — Mashgate fires `payment.captured` → Zist confirms booking (status: `confirmed`)
6. **Admin** — View webhook delivery log at `/admin/webhooks`

## Environment Variables

Create a `.env` file in the project root:

```env
# Mashgate
JWT_SECRET=your-jwt-secret
MASHGATE_API_KEY=mg_test_your_key
MASHGATE_WEBHOOK_SECRET=your-webhook-hmac-secret

# HookLine
HOOKLINE_API_KEY=dev-secret

# Zist ↔ Mashgate OIDC (printed by provision script)
MGID_CLIENT_ID=zist-local
MGID_CLIENT_SECRET=your-client-secret
MGID_REDIRECT_URI=http://localhost:8000/api/auth/callback
MGID_ADMIN_TOKEN=your-admin-token

# Session
SESSION_SECRET=random-32-char-string
```

## Stopping

```bash
docker compose -f docker-compose.demo.yml down
```

To also remove all data volumes:

```bash
docker compose -f docker-compose.demo.yml down -v
```

## Architecture

```
Browser
  │
  ▼
zist-gateway :8000
  ├── /api/auth/*     → OIDC flow (mgID at mashgate-envoy:8080)
  ├── /api/listings/* → zist-listings:8001
  ├── /api/bookings/* → zist-bookings:8002
  ├── /api/payments/* → zist-payments:8003
  ├── /api/admin/webhooks/* → hookline:8080
  └── /*              → zist-web:3000 (SvelteKit SSR)

zist-payments
  └── POST /checkout → mashgate-envoy:8080/v1/checkout/sessions
  └── POST /webhooks/mashgate ← Mashgate fires events here

mashgate-webhook-delivery
  └── Kafka consumer → delivers events → hookline:8080
      └── HookLine → forwards to registered endpoints (e.g. zist-payments/webhooks/mashgate)
```
