-module(hl_billing_test).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test generator
%%--------------------------------------------------------------------

billing_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         {"check_quota_ok - under limit returns ok",
          fun check_quota_ok/0},
         {"check_quota_exceeded - at limit returns quota_exceeded",
          fun check_quota_exceeded/0},
         {"check_quota_inactive - cancelled subscription returns subscription_inactive",
          fun check_quota_inactive/0},
         {"check_quota_disabled - HL_BILLING_ENABLED=false always returns ok",
          fun check_quota_disabled/0},
         {"check_endpoint_quota_ok - under limit returns ok",
          fun check_endpoint_quota_ok/0},
         {"check_endpoint_quota_exceeded - at limit returns endpoint_limit_reached",
          fun check_endpoint_quota_exceeded/0},
         {"track_event_ets - ETS counter increments atomically",
          fun track_event_ets/0},
         {"flush_counters_calls_store - flush triggers billing.increment_usage",
          fun flush_counters_calls_store/0},
         {"close_period_creates_invoice - correct amounts in invoice",
          fun close_period_creates_invoice/0},
         {"overage_calculation - 150K events on starter plan",
          fun overage_calculation/0},
         {"plan_feature_websocket - free disabled, growth enabled",
          fun plan_feature_websocket/0},
         {"get_retention_secs - growth plan = 30 days",
          fun get_retention_secs/0}
     ]}.

setup() ->
    %% Enable billing for most tests
    application:set_env(hl_billing, enabled, true),
    application:set_env(hl_billing, default_plan, <<"free">>),
    application:set_env(hl_billing, counter_flush_interval_ms, 999999),
    %% Start ETS table (bypass gen_server for unit tests)
    case ets:info(hl_billing_counters) of
        undefined ->
            ets:new(hl_billing_counters, [named_table, public, set,
                                          {write_concurrency, true},
                                          {read_concurrency, true}]);
        _ -> ok
    end,
    %% Mock store modules
    meck:new(hl_billing_store, [passthrough, no_link]),
    meck:new(hl_billing_counters, [passthrough, no_link]),
    meck:new(hl_store_client, [passthrough, no_link]),
    %% Stub flush/0 (gen_server not running in tests) — delegates to do_flush/0
    meck:expect(hl_billing_counters, flush, fun() ->
        hl_billing_counters:do_flush()
    end),
    ok.

teardown(_) ->
    catch meck:unload(hl_billing_store),
    catch meck:unload(hl_billing_counters),
    catch meck:unload(hl_store_client),
    ok.

%%--------------------------------------------------------------------
%% check_quota tests
%%--------------------------------------------------------------------

check_quota_ok() ->
    TId = <<"tenant_quota_ok">>,
    %% Starter plan, 50k events published (under 100k limit)
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"starter">>,
            <<"status">> => <<"active">>
        }}}
    end),
    %% ETS counter is 0 by default (fresh tenant key)
    ?assertEqual(ok, hl_billing:check_quota(TId)).

check_quota_exceeded() ->
    TId = <<"tenant_quota_exceeded">>,
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"starter">>,
            <<"status">> => <<"active">>
        }}}
    end),
    %% Inject counter at limit (100,000 events = starter limit)
    PeriodStart = hl_billing_counters:current_period_start(),
    Key = {TId, PeriodStart, events_published},
    ets:insert(hl_billing_counters, {Key, 100000}),
    try
        ?assertEqual({error, quota_exceeded}, hl_billing:check_quota(TId))
    after
        ets:delete(hl_billing_counters, Key)
    end.

check_quota_inactive() ->
    TId = <<"tenant_inactive">>,
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"starter">>,
            <<"status">> => <<"cancelled">>
        }}}
    end),
    ?assertEqual({error, subscription_inactive}, hl_billing:check_quota(TId)).

check_quota_disabled() ->
    application:set_env(hl_billing, enabled, false),
    TId = <<"tenant_disabled">>,
    try
        ?assertEqual(ok, hl_billing:check_quota(TId))
    after
        application:set_env(hl_billing, enabled, true)
    end.

%%--------------------------------------------------------------------
%% check_endpoint_quota tests
%%--------------------------------------------------------------------

check_endpoint_quota_ok() ->
    TId = <<"tenant_ep_ok">>,
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"starter">>,
            <<"status">> => <<"active">>
        }}}
    end),
    %% 3 endpoints, limit is 10 for starter
    meck:expect(hl_store_client, list_endpoints, fun(_) ->
        {ok, #{<<"endpoints">> => [ep1, ep2, ep3]}}
    end),
    ?assertEqual(ok, hl_billing:check_endpoint_quota(TId)).

check_endpoint_quota_exceeded() ->
    TId = <<"tenant_ep_exceeded">>,
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"free">>,
            <<"status">> => <<"active">>
        }}}
    end),
    %% 3 endpoints on free plan (limit = 3)
    meck:expect(hl_store_client, list_endpoints, fun(_) ->
        {ok, #{<<"endpoints">> => [ep1, ep2, ep3]}}
    end),
    ?assertEqual({error, endpoint_limit_reached},
                 hl_billing:check_endpoint_quota(TId)).

%%--------------------------------------------------------------------
%% ETS counter tests
%%--------------------------------------------------------------------

track_event_ets() ->
    TId = <<"tenant_ets_track">>,
    PeriodStart = hl_billing_counters:current_period_start(),
    Key = {TId, PeriodStart, events_published},
    ets:delete(hl_billing_counters, Key),
    hl_billing_counters:track_event(TId, published),
    hl_billing_counters:track_event(TId, published),
    hl_billing_counters:track_event(TId, published),
    [{_, N}] = ets:lookup(hl_billing_counters, Key),
    ?assertEqual(3, N),
    ets:delete(hl_billing_counters, Key).

flush_counters_calls_store() ->
    TId = <<"tenant_flush">>,
    PeriodStart = hl_billing_counters:current_period_start(),
    Key = {TId, PeriodStart, events_published},
    ets:insert(hl_billing_counters, {Key, 42}),
    Self = self(),
    meck:expect(hl_billing_store, increment_usage, fun(T, PS, Field, N) ->
        Self ! {increment_usage, T, PS, Field, N},
        {ok, #{<<"ok">> => true}}
    end),
    hl_billing_counters:do_flush(),
    receive
        {increment_usage, TId, PeriodStart, <<"events_published">>, 42} -> ok
    after 1000 ->
        ?assert(false)
    end,
    ets:delete(hl_billing_counters, Key).

%%--------------------------------------------------------------------
%% Period close tests
%%--------------------------------------------------------------------

close_period_creates_invoice() ->
    TId = <<"tenant_close">>,
    Now = erlang:system_time(millisecond),
    PeriodStart = hl_billing_counters:current_period_start(),
    PeriodEnd   = PeriodStart + (30 * 86400 * 1000),
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">>         => <<"growth">>,
            <<"status">>       => <<"active">>,
            <<"period_start">> => PeriodStart,
            <<"period_end">>   => PeriodEnd
        }}}
    end),
    meck:expect(hl_billing_store, put_usage_history, fun(_) -> {ok, #{}} end),
    meck:expect(hl_billing_store, put_invoice, fun(Inv) ->
        ?assertEqual(<<"growth">>, maps:get(<<"plan">>, Inv)),
        ?assertEqual(99, maps:get(<<"base_amount_usd">>, Inv)),
        ?assertEqual(0, maps:get(<<"overage_amount_usd">>, Inv)),
        {ok, #{}}
    end),
    meck:expect(hl_billing_store, list_payment_methods, fun(_) ->
        {ok, #{<<"methods">> => []}}
    end),
    meck:expect(hl_billing_store, put_subscription, fun(_) -> {ok, #{}} end),
    meck:expect(hl_store_client, list_endpoints, fun(_) ->
        {ok, #{<<"endpoints">> => []}}
    end),
    _ = Now,
    {ok, _InvoiceId} = hl_billing_period:do_close_period(TId).

%%--------------------------------------------------------------------
%% Overage calculation tests
%%--------------------------------------------------------------------

overage_calculation() ->
    %% 150K events published on starter plan (limit 100K) → 50K overage → $5
    TId = <<"tenant_overage">>,
    PeriodStart = hl_billing_counters:current_period_start(),
    PeriodEnd   = PeriodStart + (30 * 86400 * 1000),
    Key = {TId, PeriodStart, events_published},
    ets:insert(hl_billing_counters, {Key, 150000}),
    try
        meck:expect(hl_billing_store, get_subscription, fun(_) ->
            {ok, #{<<"subscription">> => #{
                <<"plan">>         => <<"starter">>,
                <<"status">>       => <<"active">>,
                <<"period_start">> => PeriodStart,
                <<"period_end">>   => PeriodEnd
            }}}
        end),
        meck:expect(hl_billing_store, put_usage_history, fun(_) -> {ok, #{}} end),
        meck:expect(hl_billing_store, put_invoice, fun(Inv) ->
            OverageAmt = maps:get(<<"overage_amount_usd">>, Inv),
            %% 50K overage / 10K per dollar = $5
            ?assertEqual(5.0, OverageAmt),
            {ok, #{}}
        end),
        meck:expect(hl_billing_store, list_payment_methods, fun(_) ->
            {ok, #{<<"methods">> => []}}
        end),
        meck:expect(hl_billing_store, put_subscription, fun(_) -> {ok, #{}} end),
        meck:expect(hl_store_client, list_endpoints, fun(_) ->
            {ok, #{<<"endpoints">> => []}}
        end),
        {ok, _} = hl_billing_period:do_close_period(TId)
    after
        ets:delete(hl_billing_counters, Key)
    end.

%%--------------------------------------------------------------------
%% Plan feature tests
%%--------------------------------------------------------------------

plan_feature_websocket() ->
    ?assertEqual(disabled, hl_billing:plan_feature(<<"any">>, websocket)),
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"growth">>,
            <<"status">> => <<"active">>
        }}}
    end),
    %% growth plan has websocket
    ?assertEqual(enabled, hl_billing:plan_feature(<<"growth_tenant">>, websocket)).

get_retention_secs() ->
    meck:expect(hl_billing_store, get_subscription, fun(_) ->
        {ok, #{<<"subscription">> => #{
            <<"plan">> => <<"growth">>,
            <<"status">> => <<"active">>
        }}}
    end),
    %% growth = 30 days
    ?assertEqual(30 * 86400, hl_billing:get_retention_secs(<<"growth_tenant">>)).
