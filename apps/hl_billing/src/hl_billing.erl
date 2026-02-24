%% HookLine Billing — public API.
%% Called from hl_core (quota enforcement) and hl_api handlers (billing info).
-module(hl_billing).

-export([
    %% Quota checks
    check_quota/1,
    check_endpoint_quota/1,
    get_retention_secs/1,
    plan_feature/2,
    %% Usage info
    get_subscription/1,
    get_usage_summary/1,
    get_quota_info/1,
    %% Period management
    close_period/1,
    upgrade_plan/2
]).

-define(DEFAULT_PLAN, <<"free">>).

%%--------------------------------------------------------------------
%% Quota checks
%%--------------------------------------------------------------------

%% Returns ok | {error, quota_exceeded} | {error, subscription_inactive}
%% When HL_BILLING_ENABLED=false (default) always returns ok.
check_quota(TenantId) ->
    case billing_enabled() of
        false -> ok;
        true  -> do_check_quota(TenantId)
    end.

do_check_quota(TenantId) ->
    case get_active_subscription(TenantId) of
        {error, not_found} ->
            %% No subscription record — treat as free plan, still ok
            ok;
        {ok, #{<<"status">> := Status}} when Status =:= <<"cancelled">> ->
            {error, subscription_inactive};
        {ok, #{<<"status">> := Status}} when Status =:= <<"past_due">> ->
            {error, subscription_inactive};
        {ok, Sub} ->
            Plan   = maps:get(<<"plan">>, Sub, ?DEFAULT_PLAN),
            Limit  = hl_billing_plans:event_limit(Plan),
            check_event_limit(TenantId, Plan, Limit)
    end.

check_event_limit(_TenantId, _Plan, unlimited) ->
    ok;
check_event_limit(TenantId, _Plan, Limit) ->
    Published = hl_billing_counters:get_count(TenantId, events_published),
    if
        Published >= Limit ->
            hl_billing_metrics:inc_quota_exceeded(TenantId),
            {error, quota_exceeded};
        true ->
            ok
    end.

%% Returns ok | {error, endpoint_limit_reached}
check_endpoint_quota(TenantId) ->
    case billing_enabled() of
        false -> ok;
        true  -> do_check_endpoint_quota(TenantId)
    end.

do_check_endpoint_quota(TenantId) ->
    Plan  = get_plan(TenantId),
    Limit = hl_billing_plans:endpoint_limit(Plan),
    case Limit of
        unlimited -> ok;
        N ->
            case hl_store_client:list_endpoints(TenantId) of
                {ok, #{<<"endpoints">> := Eps}} when length(Eps) >= N ->
                    {error, endpoint_limit_reached};
                _ ->
                    ok
            end
    end.

%% Returns retention in seconds for the tenant's plan.
get_retention_secs(TenantId) ->
    Plan = get_plan(TenantId),
    Days = hl_billing_plans:retention_days(Plan),
    case Days of
        custom -> 90 * 86400;  % enterprise default
        D      -> D * 86400
    end.

%% Returns enabled | disabled for a plan feature.
plan_feature(TenantId, websocket) ->
    case billing_enabled() of
        false -> enabled;
        true  ->
            Plan = get_plan(TenantId),
            case hl_billing_plans:has_websocket(Plan) of
                true  -> enabled;
                false -> disabled
            end
    end;
plan_feature(_TenantId, _Feature) ->
    enabled.

%%--------------------------------------------------------------------
%% Usage info (for API handlers)
%%--------------------------------------------------------------------

get_subscription(TenantId) ->
    case get_active_subscription(TenantId) of
        {ok, Sub}          -> {ok, Sub};
        {error, not_found} ->
            %% Return a synthetic free-plan subscription
            Now = erlang:system_time(millisecond),
            PeriodStart = current_period_start_ms(),
            PeriodEnd   = next_period_start_ms(PeriodStart),
            {ok, #{
                <<"tenant_id">>    => TenantId,
                <<"plan">>         => ?DEFAULT_PLAN,
                <<"status">>       => <<"active">>,
                <<"period_start">> => PeriodStart,
                <<"period_end">>   => PeriodEnd,
                <<"trial_ends_at">> => null,
                <<"created_at">>   => Now
            }};
        Err -> Err
    end.

get_usage_summary(TenantId) ->
    Plan        = get_plan(TenantId),
    PeriodStart = current_period_start_ms(),
    Published   = hl_billing_counters:get_count(TenantId, events_published),
    Delivered   = hl_billing_counters:get_count(TenantId, events_delivered),
    Failed      = hl_billing_counters:get_count(TenantId, events_failed),
    Limit       = hl_billing_plans:event_limit(Plan),
    {ok, #{
        <<"period_start">>     => PeriodStart,
        <<"plan">>             => Plan,
        <<"events_published">> => Published,
        <<"events_delivered">> => Delivered,
        <<"events_failed">>    => Failed,
        <<"event_limit">>      => case Limit of unlimited -> null; N -> N end
    }}.

get_quota_info(TenantId) ->
    Plan        = get_plan(TenantId),
    PeriodStart = current_period_start_ms(),
    PeriodEnd   = next_period_start_ms(PeriodStart),
    Published   = hl_billing_counters:get_count(TenantId, events_published),
    Limit       = hl_billing_plans:event_limit(Plan),
    {Remaining, Percent} = case Limit of
        unlimited -> {null, 0};
        L ->
            Rem  = max(0, L - Published),
            Pct  = round((Published / max(1, L)) * 100),
            {Rem, Pct}
    end,
    {ok, #{
        remaining => Remaining,
        limit     => case Limit of unlimited -> null; N -> N end,
        percent   => Percent,
        reset_at  => PeriodEnd
    }}.

%%--------------------------------------------------------------------
%% Period management
%%--------------------------------------------------------------------

close_period(TenantId) ->
    hl_billing_period:close_period(TenantId).

upgrade_plan(TenantId, NewPlan) ->
    case hl_billing_plans:get(NewPlan) of
        #{price_usd := 0} ->
            %% Downgrade to free — no payment needed
            do_set_plan(TenantId, NewPlan);
        #{price_usd := custom} ->
            {error, contact_sales};
        _Plan ->
            %% Paid plan — create checkout
            PeriodStart = current_period_start_ms(),
            PeriodEnd   = next_period_start_ms(PeriodStart),
            InvoiceId   = generate_id(<<"inv_">>),
            BaseAmount  = maps:get(price_usd, hl_billing_plans:get(NewPlan)),
            Invoice = #{
                <<"id">>               => InvoiceId,
                <<"tenant_id">>        => TenantId,
                <<"period_start">>     => PeriodStart,
                <<"period_end">>       => PeriodEnd,
                <<"plan">>             => NewPlan,
                <<"base_amount_usd">>  => BaseAmount,
                <<"overage_amount_usd">> => 0,
                <<"total_amount_usd">> => BaseAmount,
                <<"status">>           => <<"draft">>,
                <<"created_at">>       => erlang:system_time(millisecond)
            },
            case hl_billing_store:put_invoice(Invoice) of
                {ok, _} ->
                    ReturnUrl = list_to_binary(
                        hl_config:get_str("HL_BILLING_RETURN_URL", "http://localhost:8080")),
                    case hl_mgpay_client:create_checkout(TenantId, BaseAmount,
                                                         InvoiceId, ReturnUrl) of
                        {ok, #{<<"checkout_url">> := CheckoutUrl,
                               <<"payment_id">>   := PaymentId}} ->
                            hl_billing_store:update_invoice(TenantId, InvoiceId, #{
                                <<"mg_payment_id">>  => PaymentId,
                                <<"mg_checkout_url">> => CheckoutUrl,
                                <<"status">>         => <<"open">>
                            }),
                            {ok, CheckoutUrl};
                        {error, _} = E -> E
                    end;
                {error, _} = E -> E
            end
    end.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

billing_enabled() ->
    case hl_config:get_str("HL_BILLING_ENABLED", "false") of
        "true"  -> true;
        "1"     -> true;
        _       ->
            application:get_env(hl_billing, enabled, false)
    end.

get_plan(TenantId) ->
    case get_active_subscription(TenantId) of
        {ok, #{<<"plan">> := Plan}} -> Plan;
        _                           ->
            DefaultPlan = application:get_env(hl_billing, default_plan, ?DEFAULT_PLAN),
            DefaultPlan
    end.

get_active_subscription(TenantId) ->
    case hl_billing_store:get_subscription(TenantId) of
        {ok, #{<<"subscription">> := SubRaw}} ->
            Sub = case SubRaw of
                B when is_binary(B) -> jsx:decode(B, [return_maps]);
                M when is_map(M)    -> M
            end,
            {ok, Sub};
        {error, <<"not found">>} -> {error, not_found};
        {error, _} = E           -> E
    end.

do_set_plan(TenantId, Plan) ->
    Now         = erlang:system_time(millisecond),
    PeriodStart = current_period_start_ms(),
    PeriodEnd   = next_period_start_ms(PeriodStart),
    Sub = #{
        <<"tenant_id">>    => TenantId,
        <<"plan">>         => Plan,
        <<"status">>       => <<"active">>,
        <<"period_start">> => PeriodStart,
        <<"period_end">>   => PeriodEnd,
        <<"updated_at">>   => Now
    },
    case hl_billing_store:put_subscription(Sub) of
        {ok, _} -> {ok, no_checkout};
        Err     -> Err
    end.

current_period_start_ms() ->
    hl_billing_counters:current_period_start().

next_period_start_ms(PeriodStart) ->
    %% Add one month
    StartSecs = PeriodStart div 1000,
    {{Y, M, _D}, _} = calendar:gregorian_seconds_to_datetime(
        StartSecs + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
    {NextY, NextM} = case M of
        12 -> {Y + 1, 1};
        _  -> {Y, M + 1}
    end,
    Secs = calendar:datetime_to_gregorian_seconds({{NextY, NextM, 1}, {0,0,0}})
           - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Secs * 1000.

generate_id(Prefix) ->
    <<Prefix/binary, (hl_core_uuid:generate_str())/binary>>.
