%% Tenant self-service billing API handler.
%% Dispatches on the 'action' opt from the router.
-module(hl_api_h_billing).
-export([init/2]).

init(Req0, #{action := Action} = Opts) ->
    Method   = cowboy_req:method(Req0),
    TenantId = get_tenant(Req0),
    handle(Action, Method, TenantId, Req0, Opts).

%%--------------------------------------------------------------------
%% GET /v1/billing/subscription
%%--------------------------------------------------------------------
handle(subscription, <<"GET">>, TId, Req, Opts) ->
    case hl_billing:get_subscription(TId) of
        {ok, Sub} ->
            Plan   = maps:get(<<"plan">>, Sub, <<"free">>),
            Status = maps:get(<<"status">>, Sub, <<"active">>),
            PeriodEnd = maps:get(<<"period_end">>, Sub, null),
            TrialEnd  = maps:get(<<"trial_ends_at">>, Sub, null),
            %% Best-effort next invoice amount (base plan price)
            PlanMap   = hl_billing_plans:get(Plan),
            NextInvoice = case maps:get(price_usd, PlanMap) of
                0       -> 0;
                custom  -> null;
                Price   -> Price
            end,
            reply_json(200, #{
                <<"plan">>               => Plan,
                <<"status">>             => Status,
                <<"current_period_end">> => PeriodEnd,
                <<"trial_ends_at">>      => TrialEnd,
                <<"next_invoice_usd">>   => NextInvoice
            }, Req, Opts);
        {error, Reason} ->
            hl_api_error:reply(Req, 500, internal_error,
                list_to_binary(io_lib:format("~p", [Reason]))),
            {ok, Req, Opts}
    end;

%%--------------------------------------------------------------------
%% GET /v1/billing/usage
%%--------------------------------------------------------------------
handle(usage, <<"GET">>, TId, Req, Opts) ->
    {ok, Summary}   = hl_billing:get_usage_summary(TId),
    {ok, QuotaInfo} = hl_billing:get_quota_info(TId),
    Plan      = maps:get(<<"plan">>, Summary),
    Published = maps:get(<<"events_published">>, Summary, 0),
    Limit     = maps:get(limit, QuotaInfo),
    Remaining = maps:get(remaining, QuotaInfo),
    Percent   = maps:get(percent, QuotaInfo),
    Overage   = case Limit of
        null -> 0;
        L    -> max(0, Published - L)
    end,
    reply_json(200, #{
        <<"period">>     => maps:get(<<"period_start">>, Summary),
        <<"plan">>       => Plan,
        <<"events">>     => #{
            <<"published">> => Published,
            <<"delivered">> => maps:get(<<"events_delivered">>, Summary, 0),
            <<"failed">>    => maps:get(<<"events_failed">>, Summary, 0),
            <<"limit">>     => Limit,
            <<"remaining">> => Remaining,
            <<"percent">>   => Percent,
            <<"overage">>   => Overage
        },
        <<"endpoints">>  => #{
            <<"limit">> => case hl_billing_plans:endpoint_limit(Plan) of
                unlimited -> null;
                N -> N
            end
        },
        <<"websocket">>  => hl_billing_plans:has_websocket(Plan)
    }, Req, Opts);

%%--------------------------------------------------------------------
%% GET /v1/billing/invoices
%%--------------------------------------------------------------------
handle(invoices, <<"GET">>, TId, Req, Opts) ->
    case hl_billing_store:list_invoices(TId) of
        {ok, #{<<"invoices">> := Items}} ->
            reply_json(200, #{<<"items">> => Items}, Req, Opts);
        {ok, _} ->
            reply_json(200, #{<<"items">> => []}, Req, Opts);
        {error, Reason} ->
            hl_api_error:reply(Req, 500, store_error,
                list_to_binary(io_lib:format("~p", [Reason]))),
            {ok, Req, Opts}
    end;

%%--------------------------------------------------------------------
%% GET /v1/billing/invoices/:id
%%--------------------------------------------------------------------
handle(invoice, <<"GET">>, TId, Req, Opts) ->
    InvoiceId = cowboy_req:binding(id, Req),
    case hl_billing_store:get_invoice(TId, InvoiceId) of
        {ok, #{<<"invoice">> := InvRaw}} ->
            Inv = decode_json(InvRaw),
            reply_json(200, Inv, Req, Opts);
        {error, <<"not found">>} ->
            hl_api_error:reply(Req, 404, not_found, <<"Invoice not found">>),
            {ok, Req, Opts};
        {error, Reason} ->
            hl_api_error:reply(Req, 500, store_error,
                list_to_binary(io_lib:format("~p", [Reason]))),
            {ok, Req, Opts}
    end;

%%--------------------------------------------------------------------
%% POST /v1/billing/upgrade
%%--------------------------------------------------------------------
handle(upgrade, <<"POST">>, TId, Req, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        #{<<"plan">> := Plan} ->
            case hl_billing:upgrade_plan(TId, Plan) of
                {ok, no_checkout} ->
                    reply_json(200, #{<<"status">> => <<"upgraded">>}, Req1, Opts);
                {ok, CheckoutUrl} ->
                    reply_json(200, #{<<"checkout_url">> => CheckoutUrl}, Req1, Opts);
                {error, contact_sales} ->
                    hl_api_error:reply(Req1, 400, contact_sales,
                        <<"Enterprise plans require contacting sales">>),
                    {ok, Req1, Opts};
                {error, Reason} ->
                    hl_api_error:reply(Req1, 500, internal_error,
                        list_to_binary(io_lib:format("~p", [Reason]))),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req1, 400, validation_error, <<"plan field required">>),
            {ok, Req1, Opts}
    end;

%%--------------------------------------------------------------------
%% GET|POST /v1/billing/payment-methods
%%--------------------------------------------------------------------
handle(payment_methods, <<"GET">>, TId, Req, Opts) ->
    case hl_billing_store:list_payment_methods(TId) of
        {ok, #{<<"methods">> := Methods}} ->
            reply_json(200, #{<<"items">> => Methods}, Req, Opts);
        {ok, _} ->
            reply_json(200, #{<<"items">> => []}, Req, Opts);
        {error, Reason} ->
            hl_api_error:reply(Req, 500, store_error,
                list_to_binary(io_lib:format("~p", [Reason]))),
            {ok, Req, Opts}
    end;

handle(payment_methods, <<"POST">>, TId, Req, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        Map when is_map(Map) ->
            MethodId = hl_core_uuid:generate_str(),
            Method = Map#{
                <<"id">>         => MethodId,
                <<"tenant_id">>  => TId,
                <<"created_at">> => erlang:system_time(millisecond)
            },
            case hl_billing_store:put_payment_method(Method) of
                {ok, _} ->
                    reply_json(201, Method, Req1, Opts);
                {error, Reason} ->
                    hl_api_error:reply(Req1, 500, store_error,
                        list_to_binary(io_lib:format("~p", [Reason]))),
                    {ok, Req1, Opts}
            end;
        _ ->
            hl_api_error:reply(Req1, 400, invalid_json, <<>>),
            {ok, Req1, Opts}
    end;

%%--------------------------------------------------------------------
%% DELETE /v1/billing/payment-methods/:id
%%--------------------------------------------------------------------
handle(payment_method, <<"DELETE">>, TId, Req, Opts) ->
    MethodId = cowboy_req:binding(id, Req),
    case hl_billing_store:delete_payment_method(TId, MethodId) of
        {ok, _} ->
            Resp = cowboy_req:reply(204, #{}, <<>>, Req),
            {ok, Resp, Opts};
        {error, Reason} ->
            hl_api_error:reply(Req, 500, store_error,
                list_to_binary(io_lib:format("~p", [Reason]))),
            {ok, Req, Opts}
    end;

handle(_Action, _Method, _TId, Req, Opts) ->
    Resp = cowboy_req:reply(405, #{}, <<>>, Req),
    {ok, Resp, Opts}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_tenant(Req) ->
    maps:get(tenant_id, Req,
             list_to_binary(hl_config:get_str("HL_TENANT_ID", "default"))).

decode_json(B) when is_binary(B) -> jsx:decode(B, [return_maps]);
decode_json(M) when is_map(M)    -> M.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
