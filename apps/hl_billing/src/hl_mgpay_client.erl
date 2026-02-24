%% mgPay HTTP client using gun — same pattern as hl_delivery_worker.erl.
%% Config via env: HL_MASHGATE_URL, HL_MASHGATE_API_KEY,
%%                 HL_MASHGATE_WEBHOOK_SECRET, HL_BILLING_RETURN_URL.
-module(hl_mgpay_client).

-export([create_checkout/4, verify_webhook_signature/3]).

-define(GUN_TIMEOUT, 30000).

%% Create a mgPay checkout session.
%% Returns {ok, #{<<"payment_id">> => _, <<"checkout_url">> => _}} | {error, reason}
create_checkout(TenantId, AmountUsd, InvoiceId, ReturnUrl) ->
    BaseUrl = list_to_binary(hl_config:get_str("HL_MASHGATE_URL", "http://localhost:7000")),
    ApiKey  = list_to_binary(hl_config:get_str("HL_MASHGATE_API_KEY", "")),
    Path    = <<"/v1/checkouts">>,
    Body    = jsx:encode(#{
        <<"tenant_id">>  => TenantId,
        <<"amount_usd">> => AmountUsd,
        <<"invoice_id">> => InvoiceId,
        <<"return_url">> => ReturnUrl,
        <<"currency">>   => <<"USD">>
    }),
    Headers = [
        {<<"content-type">>,  <<"application/json">>},
        {<<"authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"user-agent">>,    <<"HookLine/0.1.0">>}
    ],
    {Scheme, Host, Port, _} = parse_url(binary_to_list(<<BaseUrl/binary, Path/binary>>)),
    FullPath = binary_to_list(Path),
    case http_post(Scheme, Host, Port, FullPath, Body, Headers, ?GUN_TIMEOUT) of
        {ok, Status, RespBody} when Status >= 200, Status < 300 ->
            case catch jsx:decode(RespBody, [return_maps]) of
                Map when is_map(Map) -> {ok, Map};
                _                   -> {error, invalid_response}
            end;
        {ok, Status, RespBody} ->
            logger:warning(#{event => mgpay_checkout_failed,
                             status => Status, body => RespBody,
                             tenant_id => TenantId}),
            {error, {http_error, Status}};
        {error, _} = E ->
            logger:error(#{event => mgpay_checkout_error,
                           reason => E, tenant_id => TenantId}),
            E
    end.

%% Verify mgPay webhook HMAC-SHA256 signature.
%% Signature header format: "v1=<hex_hmac>"
verify_webhook_signature(Body, Sig, Secret) ->
    case Sig of
        <<"v1=", HexMac/binary>> ->
            Expected = crypto:mac(hmac, sha256, Secret, Body),
            ExpectedHex = binary:encode_hex(Expected, lowercase),
            case crypto:equal_time_compare(HexMac, ExpectedHex) of
                true  -> ok;
                false -> {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

http_post(Scheme, Host, Port, Path, Body, Headers, TimeoutMs) ->
    ConnOpts = case Scheme of
        https -> #{transport => tls};
        _     -> #{}
    end,
    ConnTimeout = min(TimeoutMs, 10000),
    case gun:open(Host, Port, ConnOpts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, ConnTimeout) of
                {ok, _} ->
                    StreamRef = gun:post(ConnPid, Path, Headers, Body),
                    Resp = case gun:await(ConnPid, StreamRef, TimeoutMs) of
                        {response, fin, Status, _RH} ->
                            {ok, Status, <<>>};
                        {response, nofin, Status, _RH} ->
                            {ok, RB} = gun:await_body(ConnPid, StreamRef, TimeoutMs),
                            {ok, Status, RB};
                        {error, R} ->
                            {error, R}
                    end,
                    gun:close(ConnPid),
                    Resp;
                {error, R} ->
                    gun:close(ConnPid),
                    {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

parse_url(URL) ->
    Map    = uri_string:parse(URL),
    Scheme = list_to_atom(maps:get(scheme, Map, "http")),
    Host   = maps:get(host, Map, "localhost"),
    Port   = case maps:get(port, Map, 0) of
        0 -> case Scheme of https -> 443; _ -> 80 end;
        P -> P
    end,
    Path   = case maps:get(path, Map, "") of
        ""  -> "/";
        P2  -> P2
    end,
    {Scheme, Host, Port, Path}.
