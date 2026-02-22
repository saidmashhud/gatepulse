%% Prometheus-based delivery metrics.
%% Call init/0 once at application start.
-module(hl_delivery_metrics).
-export([init/0,
         inc_published/2,
         inc_delivered/2,
         inc_failed/2,
         inc_retried/2,
         inc_dlq/2,
         inc_rate_limited/1,
         observe_latency/2,
         set_inflight/2,
         inc_store_op/2,
         inc_overload_drop/2,
         inc_stream_clients/0,
         dec_stream_clients/0,
         inc_stream_events/2]).

init() ->
    Metrics = [
        {counter, hookline_events_published_total,   [tenant, topic],
         "Total events published"},
        {counter, hookline_deliveries_total,         [tenant, endpoint, status],
         "Total delivery attempts by status"},
        {counter, hookline_dlq_total,                [tenant, endpoint],
         "Total events moved to DLQ"},
        {counter, hookline_rate_limited_total,       [endpoint],
         "Total deliveries dropped due to rate limiting"},
        {counter, hookline_store_ops_total,          [op, status],
         "Total store IPC operations"},
        {histogram, hookline_delivery_latency_ms,    [endpoint],
         "Webhook delivery latency in milliseconds",
         [10, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000]},
        {gauge, hookline_queue_depth,                [tenant],
         "Current pending jobs in queue"},
        {gauge, hookline_inflight,                   [endpoint],
         "Current in-flight deliveries per endpoint"},
        {counter, hookline_overload_drops_total,     [tenant, reason],
         "Total events dropped due to overload"},
        {gauge, hookline_stream_clients,             [],
         "Current SSE stream clients"},
        {counter, hookline_stream_events_total,      [tenant, topic],
         "Total events sent via SSE stream"}
    ],
    lists:foreach(fun register_metric/1, Metrics).

register_metric({counter, Name, Labels, Help}) ->
    catch prometheus_counter:new([{name, Name}, {labels, Labels}, {help, Help}]);
register_metric({histogram, Name, Labels, Help, Buckets}) ->
    catch prometheus_histogram:new([{name, Name}, {labels, Labels},
                                    {help, Help}, {buckets, Buckets}]);
register_metric({gauge, Name, Labels, Help}) ->
    catch prometheus_gauge:new([{name, Name}, {labels, Labels}, {help, Help}]).

inc_published(TenantId, Topic) ->
    catch prometheus_counter:inc(hookline_events_published_total,
                                 [TenantId, Topic]).

inc_delivered(TenantId, EndpointId) ->
    catch prometheus_counter:inc(hookline_deliveries_total,
                                 [TenantId, EndpointId, <<"success">>]).

inc_failed(TenantId, EndpointId) ->
    catch prometheus_counter:inc(hookline_deliveries_total,
                                 [TenantId, EndpointId, <<"failed">>]).

inc_retried(TenantId, EndpointId) ->
    catch prometheus_counter:inc(hookline_deliveries_total,
                                 [TenantId, EndpointId, <<"retried">>]).

inc_dlq(TenantId, EndpointId) ->
    catch prometheus_counter:inc(hookline_dlq_total, [TenantId, EndpointId]).

inc_rate_limited(EndpointId) ->
    catch prometheus_counter:inc(hookline_rate_limited_total, [EndpointId]).

observe_latency(EndpointId, LatencyMs) ->
    catch prometheus_histogram:observe(hookline_delivery_latency_ms,
                                       [EndpointId], LatencyMs).

set_inflight(EndpointId, N) ->
    catch prometheus_gauge:set(hookline_inflight, [EndpointId], N).

inc_store_op(Op, Status) ->
    catch prometheus_counter:inc(hookline_store_ops_total, [Op, Status]).

inc_overload_drop(TenantId, Reason) ->
    catch prometheus_counter:inc(hookline_overload_drops_total,
                                 [TenantId, Reason]).

inc_stream_clients() ->
    catch prometheus_gauge:inc(hookline_stream_clients, []).

dec_stream_clients() ->
    catch prometheus_gauge:dec(hookline_stream_clients, []).

inc_stream_events(TenantId, Topic) ->
    catch prometheus_counter:inc(hookline_stream_events_total,
                                 [TenantId, Topic]).
