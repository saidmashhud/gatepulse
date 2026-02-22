# HookLine + FastAPI Example

A minimal webhook receiver built with Python and FastAPI.

## Quick start

```bash
pip install fastapi uvicorn hookline
WEBHOOK_SECRET=my-secret uvicorn receiver:app --port 3001
```

Publish a test event:

```bash
export HL_API_KEY=dev-secret
gp events publish --topic orders.created --payload '{"orderId":"123"}'
```

## Key pattern

```python
from hookline import verify_webhook, InvalidSignatureError

@app.post("/webhook")
async def webhook(request: Request):
    body = await request.body()
    try:
        event = verify_webhook(body, dict(request.headers), SECRET)
    except InvalidSignatureError:
        raise HTTPException(status_code=401)
    # event["topic"], event["payload"]
    return {"ok": True}
```

**Note:** Use `await request.body()` (not `request.json()`) to get raw bytes for HMAC verification.
