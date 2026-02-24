"""HookLine asyncio WebSocket client with auto-reconnect."""

from __future__ import annotations

import asyncio
import json
import logging
from collections.abc import Callable, Awaitable
from typing import Any

logger = logging.getLogger(__name__)

_RECONNECT_BASE = 1.0
_RECONNECT_MAX = 60.0
_RECONNECT_FACTOR = 2.0


class HookLineWS:
    """Asyncio WebSocket client for HookLine real-time events.

    Usage::

        async with HookLineWS("ws://localhost:8080", "my-api-key") as ws:
            await ws.subscribe("orders.*")
            ws.on_event(handle_event)
            await asyncio.sleep(3600)

    Or manage the lifecycle manually::

        ws = HookLineWS("ws://localhost:8080", "my-api-key")
        await ws.connect()
        try:
            await ws.subscribe("orders.*")
            ...
        finally:
            await ws.close()
    """

    def __init__(self, base_url: str, api_key: str) -> None:
        base = base_url.rstrip("/").replace("http://", "ws://").replace("https://", "wss://")
        self._ws_url = f"{base}/v1/ws?token={api_key}"
        self._api_key = api_key
        self._ws: Any = None
        self._running = False
        self._reconnect_delay = _RECONNECT_BASE
        self._event_callbacks: list[Callable[[dict], Awaitable[None] | None]] = []
        self._presence_callbacks: list[Callable[[dict], Awaitable[None] | None]] = []
        self._read_task: asyncio.Task | None = None

    # ── Context manager ───────────────────────────────────────────────────

    async def __aenter__(self) -> "HookLineWS":
        await self.connect()
        return self

    async def __aexit__(self, *_: Any) -> None:
        await self.close()

    # ── Public API ────────────────────────────────────────────────────────

    async def connect(self) -> None:
        """Open the WebSocket connection and start the read loop."""
        self._running = True
        await self._do_connect()
        self._read_task = asyncio.create_task(self._read_loop())

    async def close(self) -> None:
        """Close the WebSocket connection and stop the read loop."""
        self._running = False
        if self._read_task is not None:
            self._read_task.cancel()
            try:
                await self._read_task
            except asyncio.CancelledError:
                pass
        if self._ws is not None:
            await self._ws.close()
            self._ws = None

    async def subscribe(self, topic: str) -> None:
        """Subscribe to a topic pattern (e.g. ``orders.*``)."""
        await self._send({"type": "subscribe", "topic": topic})

    async def unsubscribe(self, topic: str) -> None:
        """Unsubscribe from a topic pattern."""
        await self._send({"type": "unsubscribe", "topic": topic})

    async def publish(self, topic: str, payload: dict | None = None) -> None:
        """Publish an event over the WebSocket connection."""
        await self._send({"type": "publish", "topic": topic, "payload": payload or {}})

    async def mark_read(self, event_id: str) -> None:
        """Acknowledge receipt of an event."""
        await self._send({"type": "mark_read", "event_id": event_id})

    def on_event(self, callback: Callable[[dict], Awaitable[None] | None]) -> None:
        """Register a callback invoked for each incoming event frame."""
        self._event_callbacks.append(callback)

    def on_presence(self, callback: Callable[[dict], Awaitable[None] | None]) -> None:
        """Register a callback invoked for each presence update frame."""
        self._presence_callbacks.append(callback)

    # ── Internals ─────────────────────────────────────────────────────────

    async def _do_connect(self) -> None:
        try:
            import websockets  # type: ignore[import]
        except ImportError as exc:
            raise ImportError(
                "The 'websockets' package is required for HookLineWS. "
                "Install it with: pip install websockets"
            ) from exc

        self._ws = await websockets.connect(self._ws_url)
        self._reconnect_delay = _RECONNECT_BASE
        logger.debug("HookLineWS connected to %s", self._ws_url)

    async def _read_loop(self) -> None:
        while self._running:
            try:
                assert self._ws is not None
                raw = await self._ws.recv()
                await self._dispatch(json.loads(raw))
            except asyncio.CancelledError:
                return
            except Exception as exc:
                if not self._running:
                    return
                logger.warning("HookLineWS error: %s — reconnecting in %.1fs", exc, self._reconnect_delay)
                await asyncio.sleep(self._reconnect_delay)
                self._reconnect_delay = min(self._reconnect_delay * _RECONNECT_FACTOR, _RECONNECT_MAX)
                try:
                    await self._do_connect()
                except Exception as conn_exc:
                    logger.error("HookLineWS reconnect failed: %s", conn_exc)

    async def _dispatch(self, frame: dict) -> None:
        ftype = frame.get("type")
        if ftype == "event":
            for cb in self._event_callbacks:
                result = cb(frame)
                if asyncio.iscoroutine(result):
                    await result
        elif ftype == "presence":
            for cb in self._presence_callbacks:
                result = cb(frame)
                if asyncio.iscoroutine(result):
                    await result

    async def _send(self, msg: dict) -> None:
        if self._ws is None:
            raise RuntimeError("HookLineWS is not connected")
        await self._ws.send(json.dumps(msg))
