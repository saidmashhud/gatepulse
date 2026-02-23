-module(hl_api_h_health).
-export([init/2]).

init(Req0, #{check := liveness} = Opts) ->
    Body = jsx:encode(#{<<"status">> => <<"ok">>}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, Opts};

init(Req0, #{check := readiness} = Opts) ->
    %% Check store connectivity
    Status = case hl_store_client:claim_jobs(0, 1) of
        {ok, _}    -> 200;
        {error, _} -> 503
    end,
    Body = case Status of
        200 -> jsx:encode(#{<<"status">> => <<"ready">>});
        503 -> jsx:encode(#{<<"status">> => <<"unavailable">>,
                            <<"reason">> => <<"store unreachable">>})
    end,
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, Opts};

init(Req0, #{check := embedded} = Opts) ->
    %% Returns current auth/embedding configuration.
    %% Useful for Mashgate webhook-delivery to verify service_token mode is active
    %% before attempting to publish.  Public path — no auth required.
    AuthMode     = list_to_binary(hl_config:get_str("HL_AUTH_MODE", "api_key")),
    WriterId     = list_to_binary(hl_config:get_str("HL_WRITER_ID", "")),
    SingleTenant = list_to_binary(hl_config:get_str("HL_SINGLE_TENANT", "true")),
    Body = jsx:encode(#{
        <<"embedded">>      => AuthMode =:= <<"service_token">>,
        <<"auth_mode">>     => AuthMode,
        <<"writer_id">>     => WriterId,
        <<"single_tenant">> => SingleTenant =:= <<"true">>
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, Opts}.
