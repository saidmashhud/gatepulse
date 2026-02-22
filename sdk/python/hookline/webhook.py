"""HookLine webhook verification helpers.

Usage (FastAPI):
    from hookline import verify_webhook, InvalidSignatureError

    @app.post("/webhook")
    async def webhook(request: Request):
        body = await request.body()
        try:
            event = verify_webhook(body, dict(request.headers), secret)
        except InvalidSignatureError:
            raise HTTPException(status_code=401)
        print(event["topic"], event["payload"])

Usage (Flask):
    from hookline import verify_webhook, InvalidSignatureError

    @app.post("/webhook")
    def webhook():
        try:
            event = verify_webhook(request.get_data(), dict(request.headers), secret)
        except InvalidSignatureError:
            abort(401)
        return "", 200
"""

from __future__ import annotations

import hashlib
import hmac
import json
from typing import Any


class InvalidSignatureError(ValueError):
    """Raised when webhook signature verification fails."""


def verify_webhook(
    body: bytes | str,
    headers: dict[str, str],
    secret: str,
) -> dict[str, Any]:
    """Verify a HookLine webhook request and return the parsed event.

    Args:
        body:    Raw request body (bytes or str).
        headers: Request headers (case-insensitive dict is fine).
        secret:  Endpoint secret configured in HookLine.

    Returns:
        Parsed event dict with keys: id, topic, payload, occurred_at, etc.

    Raises:
        InvalidSignatureError: Signature missing or does not match.
    """
    # Normalise headers to lowercase keys
    norm = {k.lower(): v for k, v in headers.items()}
    sig = norm.get("x-gp-signature", "")
    if not sig:
        raise InvalidSignatureError("Missing x-gp-signature header")
    ts = norm.get("x-gp-timestamp", "")
    if not ts:
        raise InvalidSignatureError("Missing x-gp-timestamp header")

    if isinstance(body, str):
        body = body.encode()

    # Signing input: "{timestamp_ms}.{body}"  (matches gp_core_signature.erl)
    signing_input = ts.encode() + b"." + body
    sig_hex = sig.removeprefix("v1=")
    expected = hmac.new(secret.encode(), signing_input, hashlib.sha256).hexdigest()
    if not hmac.compare_digest(expected, sig_hex):
        raise InvalidSignatureError("Signature mismatch")

    return json.loads(body)


def make_flask_handler(secret: str, on_event):
    """Return a Flask view function that verifies and dispatches events.

    Usage:
        from flask import Flask
        from hookline.webhook import make_flask_handler

        app = Flask(__name__)
        app.add_url_rule(
            "/webhook", "webhook",
            make_flask_handler(SECRET, lambda e: print(e)),
            methods=["POST"],
        )
    """
    def _handler():
        from flask import request, abort  # type: ignore
        try:
            event = verify_webhook(request.get_data(), dict(request.headers), secret)
        except InvalidSignatureError:
            abort(401)
            return
        on_event(event)
        return "", 200

    _handler.__name__ = "hookline_webhook"
    return _handler


def make_fastapi_handler(secret: str, on_event):
    """Return a FastAPI route coroutine that verifies and dispatches events.

    Usage:
        from fastapi import FastAPI
        from hookline.webhook import make_fastapi_handler

        app = FastAPI()
        app.post("/webhook")(make_fastapi_handler(SECRET, lambda e: print(e)))
    """
    import asyncio
    import inspect

    async def _handler(request):  # type: ignore
        from fastapi import HTTPException  # type: ignore
        body = await request.body()
        try:
            event = verify_webhook(body, dict(request.headers), secret)
        except InvalidSignatureError:
            raise HTTPException(status_code=401, detail="Invalid signature")

        if asyncio.iscoroutinefunction(on_event):
            await on_event(event)
        else:
            await asyncio.get_event_loop().run_in_executor(None, on_event, event)
        return {"ok": True}

    # Inject Request type hint so FastAPI resolves it correctly
    try:
        from fastapi import Request  # type: ignore
        _handler.__annotations__["request"] = Request
    except ImportError:
        pass

    _handler.__name__ = "hookline_webhook"
    return _handler
