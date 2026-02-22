-module(gp_core).

-export([
    validate_event/2,
    route_event/2,
    create_delivery_jobs/3,
    publish_event/2,
    check_queue_depth/1
]).

-define(DEFAULT_MAX_QUEUE_DEPTH, 100000).

%% Check if queue depth is within limits for a tenant.
check_queue_depth(TenantId) ->
    MaxDepth = gp_config:get_int("GP_MAX_QUEUE_DEPTH", ?DEFAULT_MAX_QUEUE_DEPTH),
    case gp_store_client:queue_stats(TenantId) of
        {ok, #{<<"pending">> := Pending}} when Pending >= MaxDepth ->
            gp_delivery_metrics:inc_overload_drop(TenantId, <<"queue_full">>),
            {error, overloaded};
        _ ->
            ok
    end.

%% Validate and enrich an incoming event payload map.
validate_event(TenantId, RawMap) when is_binary(TenantId), is_map(RawMap) ->
    Map = RawMap#{<<"tenant_id">> => TenantId},
    case gp_core_schema:validate_event(Map) of
        ok ->
            EventId  = gp_core_uuid:generate_str(),
            OccurredAt = maps:get(<<"occurred_at">>,
                                  Map,
                                  erlang:system_time(millisecond)),
            Enriched = Map#{
                <<"id">>          => EventId,
                <<"event_id">>    => EventId,
                <<"created_at">>  => erlang:system_time(millisecond),
                <<"occurred_at">> => OccurredAt
            },
            {ok, Enriched};
        {error, _} = E -> E
    end.

%% Find all subscriptions matching the event topic for a tenant.
%% Uses ETS subscription cache — zero C store round-trips.
route_event(TenantId, EventMap) ->
    Topic   = maps:get(<<"topic">>, EventMap),
    Matched = gp_subscription_cache:match(TenantId, Topic),
    Filtered = [S || S <- Matched,
                     gp_core_filter:matches(
                         maps:get(<<"filter">>, S, undefined), EventMap)],
    {ok, Filtered}.

%% Create delivery jobs for a list of matching subscriptions.
%% Hot path: casts to in-memory endpoint actor (ETS lookup, no C store reads).
%% Fallback: enqueues to C store if actor not found (poller will pick up).
create_delivery_jobs(EventMap, Subs, _TenantId) ->
    EventId  = maps:get(<<"id">>, EventMap),
    TenantId = maps:get(<<"tenant_id">>, EventMap),
    Results  = lists:map(fun(Sub) ->
        JobId      = gp_core_uuid:generate_str(),
        EndpointId = maps:get(<<"endpoint_id">>, Sub),
        case gp_endpoint_registry:lookup(EndpointId) of
            {ok, Pid} ->
                %% Actor path: zero C store reads, delivery in microseconds
                gen_server:cast(Pid, {enqueue, EventMap, Sub, JobId}),
                {ok, JobId};
            not_found ->
                %% Fallback: C store job queue (poller picks up)
                MaxAtt    = gp_core_retry:max_attempts(),
                Transform = maps:get(<<"transform">>, Sub, null),
                Job = #{
                    <<"job_id">>       => JobId,
                    <<"event_id">>     => EventId,
                    <<"endpoint_id">>  => EndpointId,
                    <<"tenant_id">>    => TenantId,
                    <<"max_attempts">> => MaxAtt,
                    <<"transform">>    => Transform
                },
                logger:info(#{event => job_enqueued_fallback, job_id => JobId,
                              event_id => EventId, endpoint_id => EndpointId,
                              tenant_id => TenantId}),
                case gp_store_client:enqueue_job(Job) of
                    {ok, _}    -> {ok, JobId};
                    {error, R} -> {error, R}
                end
        end
    end, Subs),
    Ids = [Id || {ok, Id} <- Results],
    {ok, Ids}.

%% Full publish pipeline: idempotency → validate → store → route → jobs → SSE
publish_event(TenantId, RawMap) ->
    IdempotencyKey = maps:get(<<"idempotency_key">>, RawMap, undefined),
    case check_idempotency(TenantId, IdempotencyKey) of
        {duplicate, ExistingEvent} ->
            logger:info(#{event => event_duplicate,
                          tenant_id => TenantId,
                          idempotency_key => IdempotencyKey}),
            {ok, ExistingEvent, deduped};

        proceed ->
            case validate_event(TenantId, RawMap) of
                {ok, Event} ->
                    do_publish(TenantId, Event, IdempotencyKey);
                {error, _} = E -> E
            end
    end.

do_publish(TenantId, Event, IdempotencyKey) ->
    Payload = jsx:encode(Event),
    EventId = maps:get(<<"id">>, Event),
    Topic   = maps:get(<<"topic">>, Event),

    case gp_store_client:append_event(EventId, TenantId, Payload) of
        {ok, _} ->
            case IdempotencyKey of
                undefined -> ok;
                K -> gp_core_idempotency:store(TenantId, K, EventId)
            end,

            logger:info(#{event => event_published,
                          event_id => EventId, tenant_id => TenantId,
                          topic => Topic}),
            gp_delivery_metrics:inc_published(TenantId, Topic),

            case route_event(TenantId, Event) of
                {ok, Subs} ->
                    {ok, JobIds} = create_delivery_jobs(Event, Subs, TenantId),
                    gp_stream_pubsub:publish(TenantId, Event),
                    {ok, Event#{<<"job_ids">> => JobIds}};
                {error, _} = E -> E
            end;

        {error, _} = E -> E
    end.

check_idempotency(_TenantId, undefined) ->
    proceed;
check_idempotency(TenantId, Key) ->
    case gp_core_idempotency:check(TenantId, Key) of
        {exists, EventId} -> {duplicate, #{<<"id">> => EventId, <<"event_id">> => EventId}};
        not_found         -> proceed
    end.
