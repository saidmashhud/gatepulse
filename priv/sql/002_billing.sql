-- HookLine Billing Schema
-- Convention: gp_ prefix, BIGINT millisecond timestamps, composite indexes.
-- This file is the spec for the C daemon's billing.* store commands.

-- Tenant subscriptions: one active row per tenant
CREATE TABLE gp_tenant_subscriptions (
    tenant_id       TEXT    PRIMARY KEY,
    plan            TEXT    NOT NULL DEFAULT 'free',
    status          TEXT    NOT NULL DEFAULT 'active',
                            -- active | past_due | cancelled | trialing
    period_start    BIGINT  NOT NULL,
    period_end      BIGINT  NOT NULL,
    trial_ends_at   BIGINT,
    created_at      BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    updated_at      BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);

-- Usage counters: one row per (tenant, billing period)
-- Primary key is the composite (tenant_id, period_start).
-- Updated in real-time via billing.increment_usage; periodically flushed from ETS.
CREATE TABLE gp_usage_counters (
    tenant_id           TEXT    NOT NULL,
    period_start        BIGINT  NOT NULL,
    events_published    BIGINT  NOT NULL DEFAULT 0,
    events_delivered    BIGINT  NOT NULL DEFAULT 0,
    events_failed       BIGINT  NOT NULL DEFAULT 0,
    ws_connections_peak BIGINT  NOT NULL DEFAULT 0,
    endpoints_count     BIGINT  NOT NULL DEFAULT 0,
    updated_at          BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    PRIMARY KEY (tenant_id, period_start)
);
CREATE INDEX idx_gp_usage_counters_tenant ON gp_usage_counters (tenant_id, period_start DESC);

-- Usage history: closed billing periods (immutable after close)
CREATE TABLE gp_usage_history (
    id                  TEXT    PRIMARY KEY,
    tenant_id           TEXT    NOT NULL,
    plan                TEXT    NOT NULL,
    period_start        BIGINT  NOT NULL,
    period_end          BIGINT  NOT NULL,
    events_published    BIGINT  NOT NULL DEFAULT 0,
    events_delivered    BIGINT  NOT NULL DEFAULT 0,
    events_failed       BIGINT  NOT NULL DEFAULT 0,
    ws_connections_peak BIGINT  NOT NULL DEFAULT 0,
    endpoints_count     BIGINT  NOT NULL DEFAULT 0,
    overage_events      BIGINT  NOT NULL DEFAULT 0,
    overage_amount_usd  NUMERIC(10,2) NOT NULL DEFAULT 0,
    closed_at           BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);
CREATE INDEX idx_gp_usage_history_tenant ON gp_usage_history (tenant_id, period_start DESC);

-- Invoices: one per closed billing period
CREATE TABLE gp_invoices (
    id                  TEXT    PRIMARY KEY,
    tenant_id           TEXT    NOT NULL,
    period_start        BIGINT  NOT NULL,
    period_end          BIGINT  NOT NULL,
    plan                TEXT    NOT NULL,
    base_amount_usd     NUMERIC(10,2) NOT NULL DEFAULT 0,
    overage_amount_usd  NUMERIC(10,2) NOT NULL DEFAULT 0,
    total_amount_usd    NUMERIC(10,2) NOT NULL DEFAULT 0,
    status              TEXT    NOT NULL DEFAULT 'draft',
                                -- draft | open | paid | void | uncollectible
    mg_payment_id       TEXT,
    mg_checkout_url     TEXT,
    paid_at             BIGINT,
    created_at          BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    updated_at          BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);
CREATE INDEX idx_gp_invoices_tenant_status ON gp_invoices (tenant_id, status);
CREATE INDEX idx_gp_invoices_open ON gp_invoices (status) WHERE status = 'open';

-- Payment methods: mgPay card tokens per tenant
CREATE TABLE gp_payment_methods (
    id              TEXT    PRIMARY KEY,
    tenant_id       TEXT    NOT NULL,
    mg_token        TEXT    NOT NULL,
    card_last4      TEXT,
    card_brand      TEXT,
    is_default      BOOLEAN NOT NULL DEFAULT false,
    created_at      BIGINT  NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    deleted_at      BIGINT
);
CREATE INDEX idx_gp_payment_methods_tenant ON gp_payment_methods (tenant_id)
    WHERE deleted_at IS NULL;
