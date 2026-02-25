%% Serves the management console UI with config injection and optional Basic Auth.
%% All /console/** paths return the same SPA index.html.
-module(hl_api_h_console).
-export([init/2]).

init(Req0, Opts) ->
    case is_enabled() of
        false ->
            Body = jsx:encode(#{<<"error">> => <<"console_disabled">>,
                                <<"message">> => <<"Console is disabled. Set HL_CONSOLE_ENABLED=true to enable.">>}),
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                Body, Req0),
            {ok, Req, Opts};
        true ->
            case check_auth(Req0) of
                ok ->
                    serve_console(Req0, Opts);
                {challenge, Req1} ->
                    {ok, Req1, Opts}
            end
    end.

%% ── Console enabled check ────────────────────────────────────────────────────

is_enabled() ->
    case hl_config:get_str("HL_CONSOLE_ENABLED", "true") of
        "true"  -> true;
        "1"     -> true;
        _       -> false
    end.

%% ── Basic Auth ───────────────────────────────────────────────────────────────

check_auth(Req) ->
    case hl_config:get_str("HL_CONSOLE_PASS", "") of
        "" ->
            %% No password configured — allow access
            ok;
        Password ->
            PasswordBin = list_to_binary(Password),
            case cowboy_req:parse_header(<<"authorization">>, Req) of
                {basic, <<"admin">>, PasswordBin} ->
                    ok;
                _ ->
                    Body = <<"Unauthorized">>,
                    Req2 = cowboy_req:reply(401,
                        #{<<"content-type">> => <<"text/plain">>,
                          <<"www-authenticate">> => <<"Basic realm=\"HookLine Console\"">>},
                        Body, Req),
                    {challenge, Req2}
            end
    end.

%% ── Serve HTML with config injection ─────────────────────────────────────────

serve_console(Req0, Opts) ->
    PrivDir  = code:priv_dir(hl_api),
    FilePath = filename:join([PrivDir, "console", "index.html"]),
    HTML = case file:read_file(FilePath) of
        {ok, Bin} -> inject_config(Bin);
        {error, _} -> fallback_html()
    end,
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html; charset=utf-8">>,
          <<"cache-control">> => <<"no-cache">>},
        HTML, Req0),
    {ok, Req, Opts}.

inject_config(HTML) ->
    Config = jsx:encode(#{
        <<"apiUrl">>   => list_to_binary(hl_config:get_str("HL_API_URL", "")),
        <<"tenantId">> => list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
        <<"version">>  => <<"2.1.0">>
    }),
    Script = <<"<script>window.__HL_CONFIG__=", Config/binary, ";</script>">>,
    binary:replace(HTML, <<"</head>">>, <<Script/binary, "</head>">>).

fallback_html() ->
    <<"<!DOCTYPE html><html><head><title>HookLine Console</title></head>",
      "<body><h1>HookLine Console</h1>",
      "<p>Console assets not found. Run <code>make assets</code> to build.</p>",
      "</body></html>">>.
