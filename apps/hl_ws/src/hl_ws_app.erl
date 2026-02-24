-module(hl_ws_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hl_ws_sup:start_link().

stop(_State) ->
    ok.
