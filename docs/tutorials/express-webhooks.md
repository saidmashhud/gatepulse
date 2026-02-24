# Add Webhooks to Express in 15 Minutes

By the end of this tutorial you will have:
- HookLine running locally delivering webhooks
- An Express app receiving and verifying signed events
- A working end-to-end flow for the `orders.created` event

**Prerequisites:** Docker, Node.js 18+, curl.

---

## Step 1 — Start HookLine (2 min)

```bash
curl -o docker-compose.yml https://raw.githubusercontent.com/hookline/hookline/main/docker-compose.yml
docker compose up -d
```

Verify it is running:

```bash
curl http://localhost:8080/healthz
# → {"status":"ok"}
```

Open the console: http://localhost:8080/console — enter `dev-secret` as the API key.

---

## Step 2 — Create your Express receiver (5 min)

```bash
mkdir my-webhook-receiver && cd my-webhook-receiver
npm init -y
npm install express hookline
```

Create `index.js`:

```javascript
const express = require("express");
const { verifyWebhook, InvalidSignatureError } = require("hookline");

const SECRET = process.env.WEBHOOK_SECRET || "my-webhook-secret";
const app = express();

// IMPORTANT: raw body parser — do not use express.json() here
app.post("/webhook", express.raw({ type: "application/json" }), (req, res) => {
  try {
    const event = verifyWebhook(req.body, req.headers, SECRET);

    console.log("✓ Webhook received:");
    console.log("  topic:  ", event.topic);
    console.log("  id:     ", event.id);
    console.log("  payload:", JSON.stringify(event.payload, null, 2));

    // Acknowledge immediately — process async if needed
    res.sendStatus(200);
  } catch (err) {
    if (err instanceof InvalidSignatureError) {
      console.error("✗ Bad signature — rejecting");
      return res.status(401).send("Unauthorized");
    }
    console.error("Handler error:", err);
    res.status(500).send("Error");
  }
});

app.listen(3001, () => console.log("Receiver on http://localhost:3001/webhook"));
```

Start it:

```bash
WEBHOOK_SECRET=my-webhook-secret node index.js
```

---

## Step 3 — Register the endpoint in HookLine (3 min)

```bash
export HL_API_KEY=dev-secret

# Create endpoint pointing at your receiver
EP=$(curl -s -X POST http://localhost:8080/v1/endpoints \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "url": "http://host.docker.internal:3001/webhook",
    "name": "My Express app",
    "secret": "my-webhook-secret"
  }' | jq -r '.endpoint_id')

echo "Endpoint: $EP"

# Subscribe to orders.created events
curl -s -X POST http://localhost:8080/v1/subscriptions \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d "{\"endpoint_id\": \"$EP\", \"topic_pattern\": \"orders.*\"}" | jq .
```

> **macOS / Linux:** Use `http://host.docker.internal:3001` so the HookLine container can reach your localhost.

---

## Step 4 — Publish an event (1 min)

```bash
curl -s -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "topic": "orders.created",
    "payload": {
      "orderId": "ord_001",
      "customer": "alice@example.com",
      "total": 99.99
    }
  }' | jq .
```

You should see in your Express terminal:

```
✓ Webhook received:
  topic:   orders.created
  id:      evt_xxxxxxxxxxxxxxxx
  payload: {
    "orderId": "ord_001",
    "customer": "alice@example.com",
    "total": 99.99
  }
```

---

## Step 5 — Inspect in the Console (2 min)

Open http://localhost:8080/console:

- **Events** tab: see the event with its full payload (click View)
- **Deliveries** tab: see the attempt — HTTP 200, latency in ms
- **Endpoints** tab: click Test to send another event

---

## What happens on failure?

If your receiver returns a 5xx or times out, HookLine retries with exponential backoff (1s, 2s, 4s, …, up to 1 hour). After 10 attempts the event moves to the **DLQ**.

From the DLQ you can requeue with one click in the Console, or via:

```bash
curl -X POST http://localhost:8080/v1/dlq/<job_id>/requeue \
  -H "Authorization: Bearer $HL_API_KEY"
```

---

## Production checklist

- [ ] Set `WEBHOOK_SECRET` to a strong random secret (32+ bytes)
- [ ] Replace `http://host.docker.internal` with your actual domain
- [ ] Set `HL_API_KEY` to a strong random key
- [ ] Mount a persistent volume for `/var/lib/hookline`
- [ ] Add a reverse proxy (nginx) in front of HookLine
- [ ] Set up Prometheus scraping from `:8080/metrics`

---

## Next steps

- [FastAPI tutorial](./fastapi-webhooks.md)
- [Production checklist](../production/checklist.md)
- [API reference](http://localhost:8080/openapi.yaml)
