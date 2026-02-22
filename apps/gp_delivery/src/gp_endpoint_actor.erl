%% Per-endpoint delivery actor.
%% Owns an in-memory job queue and dispatches up to max_in_flight concurrent
%% deliveries without any C store reads on the hot path.
-module(gp_endpoint_actor).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    endpoint_id :: binary(),
    endpoint    :: map(),
    queue       :: queue:queue(),
    in_flight   :: non_neg_integer()
}).

start_link(Endpoint) ->
    gen_server:start_link(?MODULE, [Endpoint], []).

init([Endpoint]) ->
    EpId = maps:get(<<"endpoint_id">>, Endpoint),
    gp_endpoint_registry:register(EpId, self()),
    {ok, #state{
        endpoint_id = EpId,
        endpoint    = Endpoint,
        queue       = queue:new(),
        in_flight   = 0
    }}.

%% ── Enqueue a new job (from gp_core:create_delivery_jobs) ───────────────────

handle_cast({enqueue, Event, Sub, JobId}, S) ->
    Job = make_job(Event, Sub, JobId),
    {noreply, maybe_dispatch(S#state{queue = queue:in(Job, S#state.queue)})};

%% ── Delivery result from spawned worker ─────────────────────────────────────

handle_cast({delivery_done, _JobId, Result}, S) ->
    S2 = S#state{in_flight = S#state.in_flight - 1},
    S3 = handle_result(Result, S2),
    {noreply, maybe_dispatch(S3)};

%% ── Live config update (PATCH /v1/endpoints/:id) ────────────────────────────

handle_cast({update_config, NewEp}, S) ->
    {noreply, S#state{endpoint = NewEp}};

handle_cast(_Msg, S) -> {noreply, S}.

handle_call(_Req, _From, S) -> {reply, ok, S}.

%% ── Retry timer fired ────────────────────────────────────────────────────────

handle_info({requeue, Job}, S) ->
    {noreply, maybe_dispatch(S#state{queue = queue:in(Job, S#state.queue)})};

handle_info(_Msg, S) -> {noreply, S}.

terminate(_Reason, S) ->
    gp_endpoint_registry:unregister(S#state.endpoint_id),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ── Internal ────────────────────────────────────────────────────────────────

make_job(Event, Sub, JobId) ->
    EndpointId = maps:get(<<"endpoint_id">>, Sub),
    TenantId   = maps:get(<<"tenant_id">>, Sub),
    Transform  = maps:get(<<"transform">>, Sub, null),
    #{
        <<"job_id">>       => JobId,
        <<"event_id">>     => maps:get(<<"id">>, Event,
                                       maps:get(<<"event_id">>, Event, <<>>)),
        <<"endpoint_id">>  => EndpointId,
        <<"tenant_id">>    => TenantId,
        <<"max_attempts">> => gp_core_retry:max_attempts(),
        <<"transform">>    => Transform,
        <<"attempt_n">>    => 1,
        <<"event">>        => Event
    }.

%% Dispatch as many jobs as rate and concurrency limits allow.
maybe_dispatch(S) ->
    Enabled     = maps:get(<<"enabled">>, S#state.endpoint, true),
    MaxInFlight = maps:get(<<"max_in_flight">>, S#state.endpoint, 10),
    RateRPS     = maps:get(<<"rate_limit_rps">>, S#state.endpoint, 100),
    case Enabled andalso
         S#state.in_flight < MaxInFlight andalso
         not queue:is_empty(S#state.queue) andalso
         gp_delivery_rate:check(S#state.endpoint_id, RateRPS) =:= ok of
        true  -> maybe_dispatch(dispatch_next(S));
        false -> S
    end.

dispatch_next(S) ->
    {{value, Job}, Q2} = queue:out(S#state.queue),
    ActorPid = self(),
    spawn(fun() ->
        Result = gp_delivery_worker:deliver_actor(Job, S#state.endpoint),
        gen_server:cast(ActorPid,
                        {delivery_done, maps:get(<<"job_id">>, Job), Result})
    end),
    S#state{queue = Q2, in_flight = S#state.in_flight + 1}.

handle_result({ok, _Code}, S) ->
    gp_delivery_metrics:inc_delivered(
        maps:get(<<"tenant_id">>, S#state.endpoint, <<>>),
        S#state.endpoint_id),
    S;

handle_result({retry, DelaySecs, Job2}, S) ->
    erlang:send_after(DelaySecs * 1000, self(), {requeue, Job2}),
    S;

handle_result({dlq, DlqEntry}, S) ->
    gp_store_client:put_dlq(DlqEntry),
    gp_delivery_metrics:inc_dlq(
        maps:get(<<"tenant_id">>, DlqEntry, <<>>),
        S#state.endpoint_id),
    S.
