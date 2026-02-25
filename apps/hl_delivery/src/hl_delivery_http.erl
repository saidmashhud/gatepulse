%% Mockable HTTP boundary for the delivery facade.
%%
%% In production the delivery worker (hl_delivery_worker) uses gun directly.
%% This thin module exists so tests can mock HTTP calls via meck without
%% touching the gun process tree.
%%
%% API contract (matched by hl_delivery_test):
%%   post(Url, Headers, Body) -> {ok, Status, RespHeaders, RespBody} | {error, Reason}
-module(hl_delivery_http).
-export([post/3]).

-spec post(binary(), [{binary(), binary()}], binary()) ->
    {ok, non_neg_integer(), list(), binary()} | {error, term()}.
post(URL, Headers, Body) ->
    URLStr = binary_to_list(URL),
    {Scheme, Host, Port, Path} = parse_url(URLStr),
    ConnOpts = case Scheme of
        https -> #{transport => tls};
        _     -> #{}
    end,
    case gun:open(Host, Port, ConnOpts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, 10000) of
                {ok, _} ->
                    StreamRef = gun:post(ConnPid, Path, Headers, Body),
                    Resp = case gun:await(ConnPid, StreamRef, 30000) of
                        {response, fin, Status, RH} ->
                            {ok, Status, RH, <<>>};
                        {response, nofin, Status, RH} ->
                            {ok, RB} = gun:await_body(ConnPid, StreamRef, 30000),
                            {ok, Status, RH, RB};
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
    Path = case maps:get(path, Map, "") of
        ""  -> "/";
        P2  -> P2
    end,
    {Scheme, Host, Port, Path}.
