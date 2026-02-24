# HookLine WebSocket

HookLine exposes a bidirectional WebSocket endpoint that complements the existing SSE stream. Both transports receive events from the same internal pubsub bus (`hl_stream_pubsub`), so a single `publish_event` call fans out to all SSE and WS subscribers simultaneously.

---

## Connection URL

```
ws://your-hookline-host/v1/ws?token=<API_KEY>[&topics=<pattern1,pattern2>][&user_id=<uid>]
```

| Query param | Required | Default | Description |
|-------------|----------|---------|-------------|
| `token`     | yes      | —       | API key (browsers cannot set `Authorization` headers on WS upgrade) |
| `topics`    | no       | `#`     | Comma-separated topic patterns (MQTT-style wildcards: `+` single, `#` multi) |
| `user_id`   | no       | —       | User identifier; enables presence tracking and offline queue flush |

### Example (JS)
```js
const ws = new WebSocket(
  "ws://localhost:8080/v1/ws?token=my-key&topics=rooms.general,dm.alice"
);
```

---

## Message Types

All messages are JSON objects with a `"type"` field.

### Client → Server

#### `publish`
Publish an event. Server responds with an `ack`.

```json
{
  "type": "publish",
  "topic": "rooms.general",
  "payload": { "text": "Hello!" },
  "client_id": "msg-001"
}
```

#### `subscribe`
Add topics to the server-side filter for this connection.

```json
{ "type": "subscribe", "topics": ["rooms.lobby", "rooms.#"] }
```

#### `unsubscribe`
Remove topics from the filter.

```json
{ "type": "unsubscribe", "topics": ["rooms.lobby"] }
```

#### `read`
Send a read receipt for an event (stored as a delivery attempt with `status=read`).

```json
{ "type": "read", "event_id": "evt_01HXYZ..." }
```

---

### Server → Client

#### `event`
An event matching the connection's topic filter.

```json
{
  "type": "event",
  "id": "evt_01HXYZ...",
  "topic": "rooms.general",
  "payload": { "text": "Hello!" },
  "timestamp": 1714000000000
}
```

#### `ack`
Acknowledgement for a client-initiated `publish`.

```json
{
  "type": "ack",
  "client_id": "msg-001",
  "event_id": "evt_01HXYZ..."
}
```

#### `presence`
A user came online or went offline.

```json
{
  "type": "presence",
  "event": "online",
  "user_id": "alice",
  "topics": ["rooms.general"],
  "ts": 1714000000000
}
```

---

## Presence API

### `GET /v1/presence/{topic}`

Returns users currently online and subscribed to a topic pattern.

```bash
curl http://localhost:8080/v1/presence/rooms.general \
  -H "Authorization: Bearer $HL_API_KEY"
```

```json
{
  "online": [
    { "user_id": "alice", "joined_at": 1714000000000, "topics": ["rooms.general"] }
  ],
  "count": 1
}
```

Scope required: `presence.read`

---

## Offline Queue (Direct Messages)

When you publish an event with a `target_user_id` field and that user is currently offline, HookLine automatically enqueues the event in the user's DLQ. On their next WebSocket connection (with matching `user_id=`), the buffered messages are flushed before the connection enters the normal event loop.

```bash
# Publish a direct message to alice
curl -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"topic":"dm.alice","payload":{"text":"Hey!"},"target_user_id":"alice"}'
```

---

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `HL_WS_ENABLED` | `true` | Enable/disable WebSocket endpoint |
| `HL_WS_MAX_CONNECTIONS` | `10000` | Max concurrent WS connections |
| `HL_PRESENCE_ENABLED` | `true` | Enable/disable presence tracking |
| `HL_PRESENCE_TTL_SECS` | `300` | Stale presence record TTL |
| `HL_UPLOADS_ENABLED` | `false` | Enable file uploads (Stage 4, currently 501) |

---

## JavaScript SDK

```ts
import { HookLineClient } from "@hookline/sdk";

const client = new HookLineClient({
  baseUrl: "http://localhost:8080",
  apiKey: "my-key",
});

// Open WebSocket connection
const ws = client.connect({
  topics: ["rooms.general"],
  onEvent: (evt) => console.log("event:", evt),
  onPresence: (p)  => console.log("presence:", p),
  onAck: (ack)     => console.log("ack:", ack),
  onReconnect: ()  => console.log("reconnected"),
});

// Publish a message
ws.publish("rooms.general", { text: "Hello from JS!" }, "client-id-1");

// Subscribe to more topics at runtime
ws.subscribe(["rooms.lobby"]);

// Mark an event as read
ws.markRead("evt_01HXYZ...");

// Disconnect (no auto-reconnect)
ws.disconnect();
```

You can also use `HooklineWS` directly:

```ts
import { HooklineWS } from "@hookline/sdk";

const ws = new HooklineWS(
  "ws://localhost:8080/v1/ws?token=my-key&topics=rooms.#",
  { onEvent: console.log }
).connect();
```

---

## Go SDK

```go
package main

import (
    "fmt"
    hookline "github.com/hookline/hookline-go/hookline"
)

func main() {
    client := hookline.New("http://localhost:8080", "my-key")

    wsc, err := client.ConnectWS(hookline.WSOptions{
        Topics: []string{"rooms.general"},
        OnEvent: func(e hookline.WSEvent) {
            fmt.Printf("event: %s %+v\n", e.Topic, e.Payload)
        },
        OnPresence: func(p hookline.PresenceUpdate) {
            fmt.Printf("presence: %s %s\n", p.Event, p.UserID)
        },
        OnAck: func(a hookline.AckMessage) {
            fmt.Printf("ack: %s\n", a.EventID)
        },
    })
    if err != nil {
        panic(err)
    }

    // Publish
    _ = wsc.Publish("rooms.general", map[string]any{"text": "Hello from Go!"}, "go-1")

    // Disconnect when done
    // wsc.Disconnect()
}
```

---

## Manual Testing

Requires [websocat](https://github.com/vi/websocat).

```bash
# Connect and subscribe to a topic
websocat "ws://localhost:8080/v1/ws?token=$HL_API_KEY&topics=rooms.general"

# In the terminal, type:
{"type":"publish","topic":"rooms.general","payload":{"text":"hi"},"client_id":"1"}

# Expected ack:
{"type":"ack","client_id":"1","event_id":"evt_..."}

# Expected echo event (same connection sees its own publishes):
{"type":"event","id":"evt_...","topic":"rooms.general","payload":{"text":"hi"},...}
```

Run the automated smoke test (requires websocat + curl + jq):

```bash
HL_API_KEY=my-key HL_URL=http://localhost:8080 ./test/ws.sh
```
