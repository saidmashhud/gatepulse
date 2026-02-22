"""
HookLine webhook receiver — Python + FastAPI

Start: uvicorn receiver:app --port 3001
Then publish: gp events publish --topic orders.created --payload '{"orderId":"123"}'
"""

import logging
import os
from fastapi import FastAPI, HTTPException, Request
from hookline import verify_webhook, InvalidSignatureError

SECRET = os.getenv("WEBHOOK_SECRET", "")
logging.basicConfig(level=logging.INFO)
log = logging.getLogger(__name__)

app = FastAPI(title="HookLine webhook receiver")


@app.post("/webhook")
async def webhook(request: Request):
    body = await request.body()
    headers = dict(request.headers)
    try:
        event = verify_webhook(body, headers, SECRET)
    except InvalidSignatureError as e:
        log.warning("Bad signature: %s", e)
        raise HTTPException(status_code=401, detail="Invalid signature")

    log.info(
        "Received event: topic=%s id=%s payload=%s",
        event.get("topic"),
        event.get("id"),
        event.get("payload"),
    )
    return {"ok": True}


@app.get("/healthz")
async def healthz():
    return "ok"
