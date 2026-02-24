%% Admin billing API handler.
-module(hl_api_h_billing_admin).
-export([init/2]).

init(Req0, #{action := Action} = Opts) ->
    Method = cowboy_req:method(Req0),
    handle(Action, Method, Req0, Opts).

%%--------------------------------------------------------------------
%% GET /v1/admin/billing/tenants
%%--------------------------------------------------------------------
handle(tenants, <<"GET">>, Req, Opts) ->
    %% In a full implementation this would enumerate all subscriptions from store.
    %% For now return a placeholder; the store command is billing.list_subscriptions.
    reply_json(200, #{<<"items">> => []}, Req, Opts);

%%--------------------------------------------------------------------
%% POST /v1/admin/billing/tenants/:id/plan
%%--------------------------------------------------------------------
handle(set_plan, <<"POST">>, Req, Opts) ->
    TenantId = cowboy_req:binding(id, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        #{<<"plan">> := Plan} ->
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
                {ok, _} ->
                    reply_json(200, #{<<"tenant_id">> => TenantId,
                                      <<"plan">>      => Plan}, Req1, Opts);
                {error, Reason} ->
                    hl_api_error:reply(Req1, 500, store_error,
                        list_to_binary(io_lib:format("~p", [Reason]))),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req, 400, validation_error, <<"plan field required">>),
            {ok, Req, Opts}
    end;

%%--------------------------------------------------------------------
%% POST /v1/admin/billing/invoices/:id/void
%%--------------------------------------------------------------------
handle(void_invoice, <<"POST">>, Req, Opts) ->
    InvoiceId = cowboy_req:binding(id, Req),
    %% tenant_id required to update — require it in body
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        #{<<"tenant_id">> := TenantId} ->
            case hl_billing_store:update_invoice(TenantId, InvoiceId,
                                                 #{<<"status">> => <<"void">>}) of
                {ok, _} ->
                    reply_json(200, #{<<"invoice_id">> => InvoiceId,
                                      <<"status">> => <<"void">>}, Req1, Opts);
                {error, Reason} ->
                    hl_api_error:reply(Req1, 500, store_error,
                        list_to_binary(io_lib:format("~p", [Reason]))),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req, 400, validation_error, <<"tenant_id required">>),
            {ok, Req, Opts}
    end;

%%--------------------------------------------------------------------
%% POST /v1/admin/billing/tenants/:id/trial
%%--------------------------------------------------------------------
handle(grant_trial, <<"POST">>, Req, Opts) ->
    TenantId = cowboy_req:binding(id, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        Map when is_map(Map) ->
            TrialDays   = maps:get(<<"trial_days">>, Map, 14),
            Now         = erlang:system_time(millisecond),
            TrialEndsAt = Now + (TrialDays * 86400 * 1000),
            PeriodStart = current_period_start_ms(),
            PeriodEnd   = next_period_start_ms(PeriodStart),
            Plan        = maps:get(<<"plan">>, Map, <<"starter">>),
            Sub = #{
                <<"tenant_id">>    => TenantId,
                <<"plan">>         => Plan,
                <<"status">>       => <<"trialing">>,
                <<"period_start">> => PeriodStart,
                <<"period_end">>   => PeriodEnd,
                <<"trial_ends_at">> => TrialEndsAt,
                <<"updated_at">>   => Now
            },
            case hl_billing_store:put_subscription(Sub) of
                {ok, _} ->
                    reply_json(200, #{<<"tenant_id">>    => TenantId,
                                      <<"status">>       => <<"trialing">>,
                                      <<"trial_ends_at">> => TrialEndsAt}, Req1, Opts);
                {error, Reason} ->
                    hl_api_error:reply(Req1, 500, store_error,
                        list_to_binary(io_lib:format("~p", [Reason]))),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req, 400, invalid_json, <<>>),
            {ok, Req, Opts}
    end;

handle(_Action, _Method, Req, Opts) ->
    Resp = cowboy_req:reply(405, #{}, <<>>, Req),
    {ok, Resp, Opts}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

current_period_start_ms() ->
    NowSecs = erlang:system_time(second),
    {{Y, M, _D}, _} = calendar:gregorian_seconds_to_datetime(
        NowSecs + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
    Secs = calendar:datetime_to_gregorian_seconds({{Y, M, 1}, {0,0,0}})
           - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Secs * 1000.

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

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
