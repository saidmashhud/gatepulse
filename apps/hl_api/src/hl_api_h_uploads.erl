-module(hl_api_h_uploads).
-export([init/2]).

%% Stage 4 skeleton — returns 501 when uploads are disabled (default).
%% Full S3 presigned URL generation is deferred.
init(Req, Opts) ->
    Enabled = hl_config:get_str("HL_UPLOADS_ENABLED", "false"),
    case Enabled of
        "true" ->
            Body = jsx:encode(#{<<"error">> => <<"not_implemented">>,
                                <<"message">> => <<"uploads not yet configured">>}),
            Req1 = cowboy_req:reply(501,
                #{<<"content-type">> => <<"application/json">>},
                Body, Req),
            {ok, Req1, Opts};
        _ ->
            Body = jsx:encode(#{<<"error">> => <<"uploads_disabled">>}),
            Req1 = cowboy_req:reply(501,
                #{<<"content-type">> => <<"application/json">>},
                Body, Req),
            {ok, Req1, Opts}
    end.
