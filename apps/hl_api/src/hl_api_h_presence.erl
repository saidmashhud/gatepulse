-module(hl_api_h_presence).
-export([init/2]).

init(Req, Opts) ->
    %% Topic pattern extracted from trailing path segments, e.g.
    %% GET /v1/presence/rooms.general  → Topic = <<"rooms.general">>
    %% GET /v1/presence                → Topic = <<"#">> (all)
    RawPath = cowboy_req:path_info(Req),
    Topic = case RawPath of
        undefined -> <<"#">>;
        []        -> <<"#">>;
        Parts     -> iolist_to_binary(lists:join(<<"/">>, Parts))
    end,
    TenantId = maps:get(tenant_id, Req),
    Online   = hl_presence:list(TenantId, Topic),
    Body     = jsx:encode(#{<<"online">> => Online,
                            <<"count">>  => length(Online)}),
    Req1 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {ok, Req1, Opts}.
