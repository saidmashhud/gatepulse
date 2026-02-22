/**
 * HookLine webhook verification utilities.
 *
 * Usage (Express):
 *   import { verifyWebhook, createWebhookHandler } from 'hookline';
 *   app.post('/webhook', express.raw({ type: 'application/json' }), (req, res) => {
 *     const event = verifyWebhook(req.body, req.headers, process.env.WEBHOOK_SECRET!);
 *     console.log(event.topic, event.payload);
 *     res.sendStatus(200);
 *   });
 */

export class InvalidSignatureError extends Error {
  constructor(message = "Invalid webhook signature") {
    super(message);
    this.name = "InvalidSignatureError";
  }
}

export interface WebhookEvent {
  id: string;
  topic: string;
  payload: Record<string, unknown>;
  occurred_at: number;
  [key: string]: unknown;
}

type HeadersLike = Record<string, string | string[] | undefined> | Headers;

function getHeader(headers: HeadersLike, name: string): string | undefined {
  if (headers instanceof Headers) {
    return headers.get(name) ?? undefined;
  }
  const val = (headers as Record<string, string | string[] | undefined>)[name];
  return Array.isArray(val) ? val[0] : val;
}

/**
 * Verify a HookLine webhook request synchronously (Node.js crypto).
 * Throws InvalidSignatureError if signature is missing or invalid.
 * Returns the parsed event.
 *
 * @param body    Raw request body (Buffer or string)
 * @param headers Request headers (Express req.headers or similar)
 * @param secret  Endpoint secret
 */
export function verifyWebhook(
  body: Buffer | string,
  headers: HeadersLike,
  secret: string
): WebhookEvent {
  const sig = getHeader(headers, "x-gp-signature");
  if (!sig) throw new InvalidSignatureError("Missing x-gp-signature header");
  const ts = getHeader(headers, "x-gp-timestamp");
  if (!ts) throw new InvalidSignatureError("Missing x-gp-timestamp header");

  // Node.js path
  if (typeof require !== "undefined") {
    // eslint-disable-next-line @typescript-eslint/no-var-requires
    const crypto = require("crypto") as typeof import("crypto");
    const raw = typeof body === "string" ? Buffer.from(body) : body;
    // Signing input: "{timestamp_ms}.{body}"  (matches gp_core_signature.erl)
    const input = Buffer.concat([Buffer.from(ts + "."), raw]);
    const expected =
      "v1=" +
      crypto.createHmac("sha256", secret).update(input).digest("hex");
    const sigBuf = Buffer.from(sig);
    const expBuf = Buffer.from(expected);
    if (
      sigBuf.length !== expBuf.length ||
      !crypto.timingSafeEqual(sigBuf, expBuf)
    ) {
      throw new InvalidSignatureError();
    }
    const text = typeof body === "string" ? body : body.toString("utf8");
    return JSON.parse(text) as WebhookEvent;
  }

  throw new Error(
    "Synchronous verifyWebhook requires a Node.js environment. " +
      "Use verifyWebhookAsync for browser/edge environments."
  );
}

/**
 * Verify a HookLine webhook request using the Web Crypto API (async).
 * Works in browser, Deno, Bun, Cloudflare Workers, Node 18+.
 */
export async function verifyWebhookAsync(
  body: BufferSource | string,
  headers: HeadersLike,
  secret: string
): Promise<WebhookEvent> {
  const sig = getHeader(headers, "x-gp-signature");
  if (!sig) throw new InvalidSignatureError("Missing x-gp-signature header");
  const ts = getHeader(headers, "x-gp-timestamp");
  if (!ts) throw new InvalidSignatureError("Missing x-gp-timestamp header");

  const enc = new TextEncoder();
  const rawBody = typeof body === "string" ? enc.encode(body) : new Uint8Array(body instanceof ArrayBuffer ? body : (body as ArrayBufferView).buffer);
  // Signing input: "{timestamp_ms}.{body}"  (matches gp_core_signature.erl)
  const prefix = enc.encode(ts + ".");
  const input = new Uint8Array(prefix.length + rawBody.length);
  input.set(prefix);
  input.set(rawBody, prefix.length);

  const key = await crypto.subtle.importKey(
    "raw",
    enc.encode(secret),
    { name: "HMAC", hash: "SHA-256" },
    false,
    ["sign"]
  );
  const sigBytes = await crypto.subtle.sign("HMAC", key, input);
  const hex = Array.from(new Uint8Array(sigBytes))
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
  const expected = "v1=" + hex;

  if (expected.length !== sig.length || expected !== sig) {
    throw new InvalidSignatureError();
  }

  const text = typeof body === "string" ? body : new TextDecoder().decode(body);
  return JSON.parse(text) as WebhookEvent;
}

export interface WebhookHandlerConfig {
  /** Endpoint secret for signature verification */
  secret: string;
  /** Called with the verified event */
  onEvent: (event: WebhookEvent) => Promise<void> | void;
  /** Called when verification fails or onEvent throws (optional) */
  onError?: (error: Error) => void;
}

/**
 * Build an Express/Connect-style middleware that verifies and dispatches events.
 * Mount on a raw-body route: app.post('/webhook', express.raw({type:'*\/*'}), handler)
 */
export function createWebhookHandler(config: WebhookHandlerConfig) {
  return async function webhookMiddleware(
    req: {
      body: Buffer | string;
      headers: Record<string, string | string[] | undefined>;
    },
    res: { sendStatus: (n: number) => unknown; status: (n: number) => { send: (s: string) => unknown } },
    next?: (err?: unknown) => void
  ) {
    try {
      const event = verifyWebhook(req.body, req.headers, config.secret);
      await config.onEvent(event);
      res.sendStatus(200);
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));
      if (config.onError) config.onError(error);
      if (err instanceof InvalidSignatureError) {
        res.status(401).send("Unauthorized");
      } else {
        if (next) next(err);
        else res.status(500).send("Internal Server Error");
      }
    }
  };
}
