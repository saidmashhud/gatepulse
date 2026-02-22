/**
 * HookLine webhook receiver — Node.js + Express
 *
 * Start: node receiver.js
 * Then publish: gp events publish --topic orders.created --payload '{"orderId":"123"}'
 */
const express = require("express");
const { verifyWebhook, InvalidSignatureError } = require("hookline");

const SECRET = process.env.WEBHOOK_SECRET || "";
const PORT = process.env.PORT || 3001;

const app = express();

// IMPORTANT: use raw body parser so the HMAC signature can be verified
app.post(
  "/webhook",
  express.raw({ type: "application/json" }),
  (req, res) => {
    try {
      const event = verifyWebhook(req.body, req.headers, SECRET);
      console.log(`[${new Date().toISOString()}] Received event:`);
      console.log(`  topic:   ${event.topic}`);
      console.log(`  id:      ${event.id}`);
      console.log(`  payload: ${JSON.stringify(event.payload)}`);
      res.sendStatus(200);
    } catch (err) {
      if (err instanceof InvalidSignatureError) {
        console.error("Bad signature:", err.message);
        return res.status(401).send("Unauthorized");
      }
      console.error("Handler error:", err);
      res.status(500).send("Internal Server Error");
    }
  }
);

app.get("/healthz", (_req, res) => res.send("ok"));

app.listen(PORT, () => {
  console.log(`HookLine receiver listening on http://localhost:${PORT}/webhook`);
  if (!SECRET) {
    console.warn("Warning: WEBHOOK_SECRET not set — signature verification skipped");
  }
});
