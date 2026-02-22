-module(hl_api_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hl_api_h_apikeys:init_table(),
    hl_api_h_replay:init_table(),
    Port = hl_config:get_int(<<"HL_PORT">>, 8080),
    ListenAddr = hl_config:get_str(<<"HL_LISTEN_ADDR">>, "0.0.0.0"),
    Dispatch = hl_api_router:dispatch(),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}, {ip, parse_addr(ListenAddr)}],
        #{env => #{dispatch => Dispatch},
          middlewares => [cowboy_router, hl_api_auth, cowboy_handler]}
    ),
    logger:info("HookLine API listening on ~s:~p", [ListenAddr, Port]),
    hl_api_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.

parse_addr(Addr) ->
    {ok, IP} = inet:parse_address(Addr),
    IP.
