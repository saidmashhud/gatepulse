# Why HookLine?

You need to send webhooks to your users. Here are your options.

## The comparison

| | DIY Postgres queue | HookLine (self-hosted) | Svix | Hookdeck |
|---|---|---|---|---|
| **Setup** | Write it yourself (weeks) | `docker compose up` (minutes) | Managed SaaS | Managed SaaS |
| **Cost at scale** | DB + infra only | DB + infra only | ~$0.0025/delivery | ~$0.005/delivery |
| **Data residency** | Your infra ✓ | Your infra ✓ | US/EU regions | US/EU regions |
| **Retries** | Custom | Exponential backoff, DLQ ✓ | ✓ | ✓ |
| **Signature verification** | Custom | HMAC-SHA256, SDKs ✓ | ✓ | ✓ |
| **Live delivery stream** | No | SSE ✓ | ✓ | ✓ |
| **Web UI** | No | Built-in ✓ | ✓ | ✓ |
| **Open source** | N/A | MIT ✓ | No | No |
| **Audit log** | Custom | Built-in ✓ | ✓ | ✓ |
| **Multi-tenant** | Custom | Roadmap | ✓ | ✓ |

## When HookLine makes sense

**Compliance & data residency.** Your legal team says event data cannot leave your cloud region. With HookLine you deploy it inside your own VPC. Event payloads never touch a third-party server.

**Cost at scale.** Svix charges ~$0.0025 per delivery. At 10M deliveries/month that is $25,000/month. HookLine on a $50 VPS costs $50/month. The breakeven is around 50,000 deliveries/month.

**Already have Erlang/OTP.** HookLine is a native OTP application. If you run Erlang, you can embed it directly in your umbrella.

**You want to own the code.** MIT license, no vendor lock-in, no pricing changes, no sunset risk.

## When HookLine does NOT make sense

**You need it working in 20 minutes and never want to think about ops.** Use Svix or Hookdeck. They are excellent products and the time you save on setup is worth the cost at low volumes.

**You need multi-tenant webhooks today.** HookLine v1.x is single-tenant per deployment. Multi-tenancy (shared infrastructure, per-customer API keys, usage metering) is on the v2.0 roadmap. If you need it now, use a managed service.

**Your team does not know Erlang.** HookLine is operational simplicity once deployed, but debugging requires reading Erlang logs. Consider whether your on-call team can handle that.

**You send fewer than 1,000 deliveries/month.** At that volume, a DIY solution (a cron job hitting a Postgres table) is probably fine and has zero new infrastructure to operate.

## Why not just use Postgres?

A Postgres job queue can work, and many companies ship it. The problems show up as you scale:

1. **Polling latency.** Every poll is a SELECT + row lock. At 100ms polling you have up to 100ms delivery latency. HookLine delivers in microseconds via actor message-passing.

2. **Retry complexity.** Exponential backoff, max attempts, DLQ, requeue — these are 500 lines of SQL and application code. HookLine has them built in.

3. **Fan-out.** One event → N endpoints × M subscriptions. Coordinating this across multiple app servers without duplicate delivery or missed delivery is genuinely hard. HookLine handles it.

4. **Signature verification.** Every consumer of your webhooks needs to verify HMAC-SHA256. HookLine generates the signature header automatically and provides SDK helpers.

5. **Observability.** When a webhook fails, you need to know: which attempt, what HTTP status, what response body. With Postgres you build this yourself. HookLine has a UI for it.

The Postgres queue is not wrong. It is just a foundation, not a finished product.
