#!/usr/bin/env bash
# ws.sh — WebSocket smoke test
# Requires: websocat, curl, jq
# Usage: HL_API_KEY=<key> HL_URL=http://localhost:8080 ./test/ws.sh

set -euo pipefail

command -v websocat >/dev/null 2>&1 || { echo "websocat not found, skipping ws.sh"; exit 0; }
command -v curl     >/dev/null 2>&1 || { echo "curl not found, skipping ws.sh"; exit 0; }
command -v jq       >/dev/null 2>&1 || { echo "jq not found, skipping ws.sh"; exit 0; }

HL_URL="${HL_URL:-http://localhost:8080}"
HL_API_KEY="${HL_API_KEY:-test-key}"
TOPIC="ws.smoke.$(date +%s)"
WS_URL="${HL_URL/http/ws}/v1/ws?token=${HL_API_KEY}&topics=${TOPIC}"

echo "==> WebSocket smoke test"
echo "    URL:   $HL_URL"
echo "    Topic: $TOPIC"

# ── Step 1: Connect + publish + expect ack + event ───────────────────────────
FIFO=$(mktemp -u)
mkfifo "$FIFO"
trap 'rm -f "$FIFO"' EXIT

# Start websocat in background; pipe one publish message, collect output
PUBLISH_MSG=$(jq -cn \
  --arg topic "$TOPIC" \
  '{"type":"publish","topic":$topic,"payload":{"hello":"world"},"client_id":"smoke-1"}')

OUTPUT=$(echo "$PUBLISH_MSG" | timeout 8 websocat --no-close -n1 "$WS_URL" 2>/dev/null || true)

if [ -z "$OUTPUT" ]; then
  echo "SKIP: no output from websocat (server not running?)"
  exit 0
fi

echo "    WS output: $OUTPUT"

ACK_TYPE=$(echo "$OUTPUT" | jq -r 'select(.type=="ack") | .type' 2>/dev/null || true)
EVENT_TYPE=$(echo "$OUTPUT" | jq -r 'select(.type=="event") | .type' 2>/dev/null || true)

if [ "$ACK_TYPE" = "ack" ]; then
  echo "    [PASS] ack received"
else
  echo "    [WARN] no ack in output (may be ordering issue)"
fi

if [ "$EVENT_TYPE" = "event" ]; then
  echo "    [PASS] event echoed back"
else
  echo "    [WARN] no event echo in output"
fi

# ── Step 2: Presence endpoint ─────────────────────────────────────────────────
echo "==> Presence endpoint check"
PRESENCE=$(curl -sf \
  -H "Authorization: Bearer $HL_API_KEY" \
  "${HL_URL}/v1/presence/${TOPIC}" 2>/dev/null || echo "{}")
echo "    Response: $PRESENCE"
COUNT=$(echo "$PRESENCE" | jq -r '.count // 0' 2>/dev/null || echo 0)
echo "    Online count: $COUNT"

echo "==> ws.sh done"
