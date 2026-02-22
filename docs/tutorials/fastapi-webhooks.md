# Add Webhooks to FastAPI in 15 Minutes

By the end of this tutorial your FastAPI app will receive and verify signed HookLine webhook events.

**Prerequisites:** Docker, Python 3.11+, pip.

---

## Step 1 — Start HookLine

```bash
curl -o docker-compose.yml https://raw.githubusercontent.com/hookline/hookline/main/docker-compose.yml
docker compose up -d
curl http://localhost:8080/healthz
```

---

## Step 2 — Create the FastAPI receiver

```bash
pip install fastapi uvicorn hookline
```

Create `receiver.py`:

```python
import logging
import os
from fastapi import FastAPI, HTTPException, Request
from hookline import verify_webhook, InvalidSignatureError

SECRET = os.getenv("WEBHOOK_SECRET", "my-webhook-secret")
logging.basicConfig(level=logging.INFO)
log = logging.getLogger(__name__)

app = FastAPI()

@app.post("/webhook")
async def webhook(request: Request):
    # Read raw bytes — do NOT use request.json() or body parsing middleware
    body = await request.body()
    try:
        event = verify_webhook(body, dict(request.headers), SECRET)
    except InvalidSignatureError:
        raise HTTPException(status_code=401, detail="Invalid signature")

    log.info("Event: topic=%s id=%s payload=%s",
             event["topic"], event["id"], event["payload"])
    return {"ok": True}

@app.get("/healthz")
async def healthz():
    return "ok"
```

Start it:

```bash
WEBHOOK_SECRET=my-webhook-secret uvicorn receiver:app --port 3001
```

---

## Step 3 — Register endpoint and subscribe

```bash
export HL_API_KEY=dev-secret

EP=$(curl -s -X POST http://localhost:8080/v1/endpoints \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "url": "http://host.docker.internal:3001/webhook",
    "name": "FastAPI app",
    "secret": "my-webhook-secret"
  }' | python3 -c "import sys,json; print(json.load(sys.stdin)['endpoint_id'])")

curl -s -X POST http://localhost:8080/v1/subscriptions \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d "{\"endpoint_id\": \"$EP\", \"topic_pattern\": \"orders.*\"}"
```

---

## Step 4 — Publish and observe

```bash
curl -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"topic":"orders.created","payload":{"orderId":"001","total":49.99}}'
```

Your FastAPI terminal should print the event within ~1 second.

Check the Console at http://localhost:8080/console → Deliveries for the full attempt timeline.

---

## Background task pattern

For heavy processing, acknowledge immediately and dispatch to a background task:

```python
from fastapi import BackgroundTasks

@app.post("/webhook")
async def webhook(request: Request, background_tasks: BackgroundTasks):
    body = await request.body()
    try:
        event = verify_webhook(body, dict(request.headers), SECRET)
    except InvalidSignatureError:
        raise HTTPException(status_code=401)

    background_tasks.add_task(process_event, event)
    return {"ok": True}   # ← respond immediately; HookLine sees 200

async def process_event(event: dict):
    # Heavy work here — runs after the response is sent
    await asyncio.sleep(0)  # yield to event loop
    log.info("Processing %s", event["topic"])
```

HookLine marks the delivery successful when it receives HTTP 200 — return that before doing any slow work.
