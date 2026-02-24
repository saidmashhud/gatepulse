%% Prometheus metrics for HookLine billing.
%% Registered once on startup by hl_billing_sup.
-module(hl_billing_metrics).

-export([init/0,
         inc_events_published/2,
         inc_quota_exceeded/1,
         set_active_subscriptions/2,
         observe_invoice_amount/3,
         inc_payment_failures/1]).

init() ->
    prometheus_counter:declare([
        {name, hl_billing_events_published_total},
        {labels, [tenant_id, plan]},
        {help, "Total events published per tenant"}
    ]),
    prometheus_counter:declare([
        {name, hl_billing_quota_exceeded_total},
        {labels, [tenant_id]},
        {help, "Number of times quota was exceeded per tenant"}
    ]),
    prometheus_gauge:declare([
        {name, hl_billing_active_subscriptions},
        {labels, [plan]},
        {help, "Number of active subscriptions per plan"}
    ]),
    prometheus_gauge:declare([
        {name, hl_billing_invoice_amount_usd},
        {labels, [tenant_id, status]},
        {help, "Invoice amount in USD per tenant"}
    ]),
    prometheus_counter:declare([
        {name, hl_billing_payment_failures_total},
        {labels, [tenant_id]},
        {help, "Total payment failures per tenant"}
    ]),
    ok.

inc_events_published(TenantId, Plan) ->
    catch prometheus_counter:inc(hl_billing_events_published_total,
                                 [TenantId, Plan]).

inc_quota_exceeded(TenantId) ->
    catch prometheus_counter:inc(hl_billing_quota_exceeded_total, [TenantId]).

set_active_subscriptions(Plan, Count) ->
    catch prometheus_gauge:set(hl_billing_active_subscriptions, [Plan], Count).

observe_invoice_amount(TenantId, Status, AmountUsd) ->
    catch prometheus_gauge:set(hl_billing_invoice_amount_usd,
                               [TenantId, Status], AmountUsd).

inc_payment_failures(TenantId) ->
    catch prometheus_counter:inc(hl_billing_payment_failures_total, [TenantId]).
