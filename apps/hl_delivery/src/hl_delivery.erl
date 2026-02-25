%% High-level delivery facade.
%%
%% Provides a simplified API on top of hl_delivery_worker for callers that
%% already have the Job and Endpoint maps in memory (atom keys).
%%
%% Used by: hl_delivery_test (mocks hl_delivery_http for unit tests).
%%
%% Return values:
%%   {ok, delivered}              — 2xx
%%   {retry, NextAttemptEpoch}    — retryable failure (5xx / network error)
%%   {dlq, Reason}                — non-retryable (4xx / max attempts)
-module(hl_delivery).
-export([deliver/2, consume_rate_token/1, backoff_seconds/1]).

-define(RATE_TABLE, hl_delivery_facade_rate).
-define(BACKOFF_SCHEDULE, [60, 300, 1800, 7200, 28800]).

%% ── deliver/2 ──────────────────────────────────────────────────────────────

deliver(Job, Endpoint) ->
    Url     = maps:get(url, Endpoint),
    Secret  = maps:get(secret, Endpoint, <<>>),
    Payload = maps:get(payload, Job),
    JobId   = maps:get(job_id, Job),
    Attempt = maps:get(attempt_count, Job, 1),
    MaxAtt  = maps:get(max_attempts, Job, 5),

    %% Build headers
    Ts    = erlang:system_time(millisecond),
    TsBin = integer_to_binary(Ts),

    SigHeaders = case Secret of
        <<>> -> [];
        S    ->
            Sig = hl_core_signature:header_value(S, Ts, Payload),
            [{<<"x-gp-signature">>, Sig}]
    end,

    Headers = [
        {<<"content-type">>,     <<"application/json">>},
        {<<"x-gp-timestamp">>,   TsBin}
        | SigHeaders
    ],

    Result = hl_delivery_http:post(Url, Headers, Payload),

    %% Record attempt (best-effort; hl_delivery_store may not be loaded)
    maybe_record(JobId, Result),

    %% Classify response
    classify(Result, Attempt, MaxAtt).

%% ── consume_rate_token/1 ───────────────────────────────────────────────────

consume_rate_token(Endpoint) ->
    EndpointId = maps:get(endpoint_id, Endpoint),
    RateRPS    = maps:get(rate_limit_rps, Endpoint),
    ensure_rate_table(),
    Now   = erlang:system_time(second),
    Key   = {EndpointId, Now},
    Count = ets:update_counter(?RATE_TABLE, Key, {2, 1}, {Key, 0}),
    if
        Count =< RateRPS -> ok;
        true             -> {error, rate_limited}
    end.

%% ── backoff_seconds/1 ─────────────────────────────────────────────────────
%% Deterministic schedule: 1m, 5m, 30m, 2h, 8h — capped at 8h (28 800s).

backoff_seconds(Attempt) when Attempt =< 0 ->
    hd(?BACKOFF_SCHEDULE);
backoff_seconds(Attempt) ->
    Idx = min(Attempt, length(?BACKOFF_SCHEDULE)),
    lists:nth(Idx, ?BACKOFF_SCHEDULE).

%% ── Internal ───────────────────────────────────────────────────────────────

classify({ok, Status, _, _}, _Attempt, _Max) when Status >= 200, Status < 300 ->
    {ok, delivered};
classify({ok, Status, _, _}, _Attempt, _Max) when Status >= 400, Status < 500 ->
    {dlq, <<"http_4xx">>};
classify(_Result, Attempt, Max) when Attempt >= Max ->
    {dlq, <<"max_attempts_exceeded">>};
classify(_Result, Attempt, _Max) ->
    Delay = backoff_seconds(Attempt),
    {retry, erlang:system_time(second) + Delay}.

maybe_record(JobId, Result) ->
    {Status, Code, Body} = case Result of
        {ok, C, _, B}  -> {<<"delivered">>, C, B};
        {error, R}     ->
            Msg = list_to_binary(io_lib:format("~p", [R])),
            {<<"error">>, 0, Msg}
    end,
    try hl_delivery_store:record_attempt(JobId, Status, Code, Body)
    catch _:_ -> ok
    end.

ensure_rate_table() ->
    case ets:info(?RATE_TABLE) of
        undefined ->
            ets:new(?RATE_TABLE, [named_table, public, set]);
        _ -> ok
    end.
