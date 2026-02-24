%% hl_delivery_test.erl
%% Unit tests for the HookLine delivery engine.
%%
%% Delivery flow:
%%   hl_delivery:deliver(Job) → HTTP POST to endpoint URL
%%   On 2xx → ack (delivered)
%%   On 4xx/5xx or network error → nack (schedule retry or DLQ after max attempts)
%%   Rate limiting: delivery respects rate_limit_rps per endpoint
%%
%% Uses meck to stub hl_delivery_http so no real HTTP is made.

-module(hl_delivery_test).
-include_lib("eunit/include/eunit.hrl").

%% ─── Setup / teardown ────────────────────────────────────────────────────────

setup() ->
    meck:new(hl_delivery_http, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(hl_delivery_http).

%% ─── Fixtures ────────────────────────────────────────────────────────────────

-define(ENDPOINT, #{
    endpoint_id  => <<"ep-001">>,
    url          => <<"https://example.com/hook">>,
    secret       => <<"s3cr3t">>,
    rate_limit_rps => 100
}).

-define(JOB, #{
    job_id       => <<"job-001">>,
    event_id     => <<"evt-001">>,
    endpoint_id  => <<"ep-001">>,
    tenant_id    => <<"tenant-a">>,
    payload      => <<"{\"eventType\":\"payment.captured\"}">>,
    attempt_count => 1,
    max_attempts  => 5
}).

%% ─── Test: 200 response → success ────────────────────────────────────────────

delivery_success_200_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         meck:expect(hl_delivery_http, post, fun(_, _, _) ->
             {ok, 200, [], <<"{\"ok\":true}">>}
         end),
         Result = hl_delivery:deliver(?JOB, ?ENDPOINT),
         ?assertMatch({ok, delivered}, Result)
     end}.

%% ─── Test: 500 response → retry scheduled ────────────────────────────────────

delivery_retry_on_500_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         %% First call: 500. Second call: 200.
         meck:sequence(hl_delivery_http, post, 3,
             [{ok, 500, [], <<"error">>},
              {ok, 200, [], <<"ok">>}]),
         Result1 = hl_delivery:deliver(?JOB, ?ENDPOINT),
         ?assertMatch({retry, _NextAttemptAt}, Result1),
         Job2 = ?JOB#{attempt_count := 2},
         Result2 = hl_delivery:deliver(Job2, ?ENDPOINT),
         ?assertMatch({ok, delivered}, Result2)
     end}.

%% ─── Test: 400 client error → DLQ immediately (no retry) ────────────────────

delivery_no_retry_on_400_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         meck:expect(hl_delivery_http, post, fun(_, _, _) ->
             {ok, 400, [], <<"{\"error\":\"bad request\"}">>}
         end),
         Result = hl_delivery:deliver(?JOB, ?ENDPOINT),
         %% 4xx is not retried — goes straight to DLQ
         ?assertMatch({dlq, <<"http_4xx">>}, Result)
     end}.

%% ─── Test: max attempts → DLQ ────────────────────────────────────────────────

delivery_dlq_after_max_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         meck:expect(hl_delivery_http, post, fun(_, _, _) ->
             {ok, 503, [], <<"service unavailable">>}
         end),
         %% Attempt = max_attempts (last possible attempt)
         MaxJob = ?JOB#{attempt_count := 5, max_attempts := 5},
         Result = hl_delivery:deliver(MaxJob, ?ENDPOINT),
         ?assertMatch({dlq, _Reason}, Result)
     end}.

%% ─── Test: network error → retry ─────────────────────────────────────────────

delivery_retry_on_network_error_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         meck:expect(hl_delivery_http, post, fun(_, _, _) ->
             {error, econnrefused}
         end),
         Result = hl_delivery:deliver(?JOB, ?ENDPOINT),
         ?assertMatch({retry, _NextAttemptAt}, Result)
     end}.

%% ─── Test: rate limit respected (tokens consumed) ────────────────────────────

delivery_rate_limit_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         %% Endpoint allows 2 req/s; 3rd request should be throttled
         ThrottledEndpoint = ?ENDPOINT#{rate_limit_rps := 2},
         meck:expect(hl_delivery_http, post, fun(_, _, _) ->
             {ok, 200, [], <<"ok">>}
         end),
         ok = hl_delivery:consume_rate_token(ThrottledEndpoint),
         ok = hl_delivery:consume_rate_token(ThrottledEndpoint),
         Result = hl_delivery:consume_rate_token(ThrottledEndpoint),
         ?assertMatch({error, rate_limited}, Result)
     end}.

%% ─── Test: signature is attached to outgoing request ─────────────────────────

delivery_signature_header_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         Self = self(),
         meck:expect(hl_delivery_http, post, fun(Url, Headers, Body) ->
             Self ! {called, Url, Headers, Body},
             {ok, 200, [], <<"ok">>}
         end),
         hl_delivery:deliver(?JOB, ?ENDPOINT),
         receive
             {called, _Url, Headers, _Body} ->
                 Sig = proplists:get_value(<<"x-gp-signature">>, Headers),
                 Ts  = proplists:get_value(<<"x-gp-timestamp">>, Headers),
                 ?assert(is_binary(Sig)),
                 ?assertMatch(<<"v1=", _/binary>>, Sig),
                 ?assert(is_binary(Ts))
         after 1000 ->
             ?assert(false)
         end
     end}.

%% ─── Test: retry back-off schedule ───────────────────────────────────────────

delivery_backoff_schedule_test_() ->
    %% Verifies that retry delays increase with attempt number.
    %% Back-off formula: 60s * 2^(attempt - 1), capped at 8h.
    [
        ?_assertEqual(60,     hl_delivery:backoff_seconds(1)),
        ?_assertEqual(300,    hl_delivery:backoff_seconds(2)),
        ?_assertEqual(1800,   hl_delivery:backoff_seconds(3)),
        ?_assertEqual(7200,   hl_delivery:backoff_seconds(4)),
        ?_assertEqual(28800,  hl_delivery:backoff_seconds(5)),
        ?_assertEqual(28800,  hl_delivery:backoff_seconds(10))  % capped at 8h
    ].

%% ─── Test: delivery response recorded in DB ──────────────────────────────────

delivery_records_attempt_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(_) ->
         meck:new(hl_delivery_store, [non_strict]),
         meck:expect(hl_delivery_http, post, fun(_, _, _) ->
             {ok, 200, [], <<"ok">>}
         end),
         meck:expect(hl_delivery_store, record_attempt, fun(JobId, _Status, _Code, _Body) ->
             ?assert(is_binary(JobId)),
             ok
         end),
         hl_delivery:deliver(?JOB, ?ENDPOINT),
         ?assert(meck:called(hl_delivery_store, record_attempt, '_')),
         meck:unload(hl_delivery_store)
     end}.
