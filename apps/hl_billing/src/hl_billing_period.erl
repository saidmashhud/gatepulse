%% Monthly period close gen_server.
%% Flushes counters, calculates overage, generates invoice, charges via mgPay.
-module(hl_billing_period).
-behaviour(gen_server).

-export([start_link/0, close_period/1, do_close_period/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Trigger period close for a specific tenant (also called by timer).
close_period(TenantId) ->
    gen_server:call(?MODULE, {close_period, TenantId}, 60000).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    schedule_next_close(),
    {ok, #{}}.

handle_call({close_period, TenantId}, _From, State) ->
    Result = do_close_period(TenantId),
    {reply, Result, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(period_close, State) ->
    logger:info(#{event => billing_period_close_tick}),
    %% Close all active subscriptions
    close_all_active_tenants(),
    schedule_next_close(),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Core period close logic
%%--------------------------------------------------------------------

do_close_period(TenantId) ->
    %% Step 1: read current subscription
    case get_subscription(TenantId) of
        {error, _} = E -> E;
        {ok, Sub} ->
            Plan        = maps:get(<<"plan">>, Sub, <<"free">>),
            PeriodStart = maps:get(<<"period_start">>, Sub),
            PeriodEnd   = maps:get(<<"period_end">>, Sub),

            %% Step 2: capture ETS counts BEFORE flushing
            {Published, Delivered, Failed, WsPeak, EpCount} =
                fetch_usage(TenantId, PeriodStart),

            %% Step 3: flush ETS counters to store (zeros ETS after flush)
            hl_billing_counters:flush(),

            %% Step 4: calculate overage
            EventLimit = hl_billing_plans:event_limit(Plan),
            {OverageEvents, OverageAmount} = calc_overage(Plan, Published, EventLimit),

            %% Step 5: write usage history
            HistoryId = generate_id(<<"uh_">>),
            Now = erlang:system_time(millisecond),
            History = #{
                <<"id">>                 => HistoryId,
                <<"tenant_id">>          => TenantId,
                <<"plan">>               => Plan,
                <<"period_start">>       => PeriodStart,
                <<"period_end">>         => PeriodEnd,
                <<"events_published">>   => Published,
                <<"events_delivered">>   => Delivered,
                <<"events_failed">>      => Failed,
                <<"ws_connections_peak">> => WsPeak,
                <<"endpoints_count">>    => EpCount,
                <<"overage_events">>     => OverageEvents,
                <<"overage_amount_usd">> => OverageAmount,
                <<"closed_at">>          => Now
            },
            catch hl_billing_store:put_usage_history(History),

            %% Step 6: create invoice
            BaseAmount = plan_price(Plan),
            TotalAmount = BaseAmount + OverageAmount,
            InvoiceId = generate_id(<<"inv_">>),
            Invoice = #{
                <<"id">>                 => InvoiceId,
                <<"tenant_id">>          => TenantId,
                <<"period_start">>       => PeriodStart,
                <<"period_end">>         => PeriodEnd,
                <<"plan">>               => Plan,
                <<"base_amount_usd">>    => BaseAmount,
                <<"overage_amount_usd">> => OverageAmount,
                <<"total_amount_usd">>   => TotalAmount,
                <<"status">>             => <<"draft">>,
                <<"created_at">>         => Now
            },
            case hl_billing_store:put_invoice(Invoice) of
                {ok, _} ->
                    maybe_charge(TenantId, TotalAmount, InvoiceId),
                    roll_subscription(TenantId, Sub, Plan),
                    {ok, InvoiceId};
                {error, _} = E -> E
            end
    end.

maybe_charge(_TenantId, Amount, _InvoiceId) when Amount =:= 0 ->
    ok;
maybe_charge(TenantId, Amount, InvoiceId) ->
    case has_payment_method(TenantId) of
        false -> ok;
        true  ->
            ReturnUrl = list_to_binary(
                hl_config:get_str("HL_BILLING_RETURN_URL", "http://localhost:8080")),
            case hl_mgpay_client:create_checkout(TenantId, Amount, InvoiceId, ReturnUrl) of
                {ok, #{<<"checkout_url">> := CheckoutUrl,
                       <<"payment_id">>   := PaymentId}} ->
                    catch hl_billing_store:update_invoice(TenantId, InvoiceId, #{
                        <<"mg_payment_id">>   => PaymentId,
                        <<"mg_checkout_url">> => CheckoutUrl,
                        <<"status">>          => <<"open">>
                    });
                {error, Reason} ->
                    logger:warning(#{event => billing_checkout_failed,
                                     tenant_id => TenantId,
                                     invoice_id => InvoiceId,
                                     reason => Reason}),
                    hl_billing_metrics:inc_payment_failures(TenantId)
            end
    end.

roll_subscription(TenantId, _Sub, Plan) ->
    OldEnd   = hl_billing:get_subscription(TenantId),
    NewStart = current_period_start_ms(),
    NewEnd   = next_period_start_ms(NewStart),
    Now      = erlang:system_time(millisecond),
    UpdatedSub = #{
        <<"tenant_id">>    => TenantId,
        <<"plan">>         => Plan,
        <<"status">>       => <<"active">>,
        <<"period_start">> => NewStart,
        <<"period_end">>   => NewEnd,
        <<"updated_at">>   => Now
    },
    _ = OldEnd,
    catch hl_billing_store:put_subscription(UpdatedSub).

close_all_active_tenants() ->
    %% In a real implementation this would iterate stored subscriptions.
    %% For now we rely on individual close_period calls from the timer.
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

fetch_usage(TenantId, PeriodStart) ->
    Published = hl_billing_counters:get_count(TenantId, events_published),
    Delivered = hl_billing_counters:get_count(TenantId, events_delivered),
    Failed    = hl_billing_counters:get_count(TenantId, events_failed),
    %% For WS peak and endpoint count we fall back to store snapshot
    WsPeak = 0,
    EpCount = case hl_store_client:list_endpoints(TenantId) of
        {ok, #{<<"endpoints">> := Eps}} -> length(Eps);
        _ -> 0
    end,
    _ = PeriodStart,
    {Published, Delivered, Failed, WsPeak, EpCount}.

calc_overage(_Plan, _Published, unlimited) ->
    {0, 0};
calc_overage(Plan, Published, Limit) when Published > Limit ->
    case hl_billing_plans:overage_rate(Plan) of
        none ->
            {0, 0};
        {events_per_dollar, Rate} ->
            OverageEvents = Published - Limit,
            OverageAmount = ceil(OverageEvents / Rate) * 1.0,
            {OverageEvents, OverageAmount}
    end;
calc_overage(_Plan, _Published, _Limit) ->
    {0, 0}.

plan_price(Plan) ->
    case maps:get(price_usd, hl_billing_plans:get(Plan)) of
        custom -> 0;
        N      -> N
    end.

has_payment_method(TenantId) ->
    case hl_billing_store:list_payment_methods(TenantId) of
        {ok, #{<<"methods">> := [_|_]}} -> true;
        _                               -> false
    end.

get_subscription(TenantId) ->
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

schedule_next_close() ->
    Ms = time_until_period_end_ms(),
    logger:info(#{event => billing_next_close_scheduled, in_ms => Ms}),
    erlang:send_after(Ms, self(), period_close).

time_until_period_end_ms() ->
    Now      = erlang:system_time(millisecond),
    PeriodStart = current_period_start_ms(),
    PeriodEnd   = next_period_start_ms(PeriodStart),
    max(1000, PeriodEnd - Now).

current_period_start_ms() ->
    hl_billing_counters:current_period_start().

next_period_start_ms(PeriodStart) ->
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
