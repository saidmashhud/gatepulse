%% Billing store protocol wrappers.
%% Follows the exact hl_store_client.erl call pattern.
-module(hl_billing_store).

-export([
    get_subscription/1,
    put_subscription/1,
    get_usage/2,
    increment_usage/4,
    put_usage_history/1,
    list_invoices/1,
    get_invoice/2,
    put_invoice/1,
    update_invoice/3,
    list_payment_methods/1,
    put_payment_method/1,
    delete_payment_method/2
]).

call(Cmd, Args) ->
    Frame = hl_store_protocol:encode(Cmd, Args),
    case hl_store_pool:call(Frame) of
        #{<<"ok">> := true}  = Resp -> {ok, Resp};
        #{<<"ok">> := false} = Resp ->
            Err = maps:get(<<"error">>, Resp, <<"unknown">>),
            logger:warning(#{event => billing_store_error, cmd => Cmd, error => Err}),
            {error, Err};
        {error, _} = E ->
            logger:error(#{event => billing_store_error, cmd => Cmd, reason => E}),
            E
    end.

get_subscription(TenantId) ->
    call(<<"billing.get_subscription">>, #{<<"tenant_id">> => TenantId}).

put_subscription(Sub) when is_map(Sub) ->
    call(<<"billing.put_subscription">>, Sub).

get_usage(TenantId, PeriodStart) ->
    call(<<"billing.get_usage">>, #{
        <<"tenant_id">>    => TenantId,
        <<"period_start">> => PeriodStart
    }).

increment_usage(TenantId, PeriodStart, Field, N) when is_binary(Field), is_integer(N) ->
    call(<<"billing.increment_usage">>, #{
        <<"tenant_id">>    => TenantId,
        <<"period_start">> => PeriodStart,
        <<"field">>        => Field,
        <<"n">>            => N
    }).

put_usage_history(Entry) when is_map(Entry) ->
    call(<<"billing.put_usage_history">>, Entry).

list_invoices(TenantId) ->
    call(<<"billing.list_invoices">>, #{<<"tenant_id">> => TenantId}).

get_invoice(TenantId, InvoiceId) ->
    call(<<"billing.get_invoice">>, #{
        <<"tenant_id">>  => TenantId,
        <<"invoice_id">> => InvoiceId
    }).

put_invoice(Invoice) when is_map(Invoice) ->
    call(<<"billing.put_invoice">>, Invoice).

update_invoice(TenantId, InvoiceId, Fields) when is_map(Fields) ->
    call(<<"billing.update_invoice">>, Fields#{
        <<"tenant_id">>  => TenantId,
        <<"invoice_id">> => InvoiceId
    }).

list_payment_methods(TenantId) ->
    call(<<"billing.list_payment_methods">>, #{<<"tenant_id">> => TenantId}).

put_payment_method(Method) when is_map(Method) ->
    call(<<"billing.put_payment_method">>, Method).

delete_payment_method(TenantId, MethodId) ->
    call(<<"billing.delete_payment_method">>, #{
        <<"tenant_id">> => TenantId,
        <<"method_id">> => MethodId
    }).
