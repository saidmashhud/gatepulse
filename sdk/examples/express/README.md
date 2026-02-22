# HookLine + Express Example

A minimal webhook receiver built with Node.js and Express.

## Quick start

```bash
npm install
WEBHOOK_SECRET=my-secret node receiver.js
```

In another terminal, publish a test event:

```bash
export HL_API_KEY=dev-secret
gp events publish --topic orders.created --payload '{"orderId":"123","total":99.99}'
```

You should see the event printed in the receiver terminal within a second.

## Key pattern

```js
const { verifyWebhook, InvalidSignatureError } = require("hookline");

app.post("/webhook", express.raw({ type: "application/json" }), (req, res) => {
  try {
    const event = verifyWebhook(req.body, req.headers, process.env.WEBHOOK_SECRET);
    // event.topic, event.payload, event.id
    res.sendStatus(200);
  } catch (err) {
    if (err instanceof InvalidSignatureError) return res.status(401).send("Unauthorized");
    res.status(500).send("Error");
  }
});
```

**Critical:** use `express.raw()` not `express.json()` so the raw body is available for HMAC verification.
