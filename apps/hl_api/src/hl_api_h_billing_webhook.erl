%% mgPay webhook handler — unauthenticated; signature verified inline.
-module(hl_api_h_billing_webhook).
-export([init/2]).

init(Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Sig    = cowboy_req:header(<<"x-mgpay-signature">>, Req1, <<>>),
    Secret = list_to_binary(
        hl_config:get_str("HL_MASHGATE_WEBHOOK_SECRET", "")),
    case hl_mgpay_client:verify_webhook_signature(Body, Sig, Secret) of
        {error, invalid} ->
            logger:warning(#{event => billing_webhook_invalid_sig,
                             sig => Sig}),
            Resp = cowboy_req:reply(401,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"invalid_signature\"}">>, Req1),
            {ok, Resp, Opts};
        ok ->
            handle_event(Body, Req1, Opts)
    end.

handle_event(Body, Req, Opts) ->
    case catch jsx:decode(Body, [return_maps]) of
        #{<<"type">> := <<"payment.captured">>} = Event ->
            handle_payment_captured(Event),
            reply_ok(Req, Opts);
        #{<<"type">> := <<"payment.failed">>} = Event ->
            handle_payment_failed(Event),
            reply_ok(Req, Opts);
        _ ->
            %% Unknown event type — acknowledge to prevent retries
            reply_ok(Req, Opts)
    end.

handle_payment_captured(Event) ->
    TenantId  = maps:get(<<"tenant_id">>, Event, undefined),
    InvoiceId = maps:get(<<"invoice_id">>, Event, undefined),
    PaymentId = maps:get(<<"payment_id">>, Event, undefined),
    Now       = erlang:system_time(millisecond),
    logger:info(#{event => billing_payment_captured,
                  tenant_id => TenantId, invoice_id => InvoiceId}),
    case TenantId =/= undefined andalso InvoiceId =/= undefined of
        true ->
            catch hl_billing_store:update_invoice(TenantId, InvoiceId, #{
                <<"status">>        => <<"paid">>,
                <<"mg_payment_id">> => PaymentId,
                <<"paid_at">>       => Now
            }),
            %% Ensure subscription is active
            catch hl_billing_store:get_subscription(TenantId);
        false ->
            ok
    end.

handle_payment_failed(Event) ->
    TenantId  = maps:get(<<"tenant_id">>, Event, undefined),
    InvoiceId = maps:get(<<"invoice_id">>, Event, undefined),
    logger:warning(#{event => billing_payment_failed,
                     tenant_id => TenantId, invoice_id => InvoiceId}),
    case TenantId =/= undefined andalso InvoiceId =/= undefined of
        true ->
            hl_billing_metrics:inc_payment_failures(TenantId),
            catch hl_billing_store:update_invoice(TenantId, InvoiceId, #{
                <<"status">> => <<"uncollectible">>
            }),
            %% Mark subscription past_due
            case hl_billing_store:get_subscription(TenantId) of
                {ok, #{<<"subscription">> := SubRaw}} ->
                    Sub = case SubRaw of
                        B when is_binary(B) -> jsx:decode(B, [return_maps]);
                        M when is_map(M)    -> M
                    end,
                    catch hl_billing_store:put_subscription(
                        Sub#{<<"status">> => <<"past_due">>,
                             <<"updated_at">> => erlang:system_time(millisecond)});
                _ -> ok
            end;
        false ->
            ok
    end.

reply_ok(Req, Opts) ->
    Resp = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        <<"{\"ok\":true}">>, Req),
    {ok, Resp, Opts}.
