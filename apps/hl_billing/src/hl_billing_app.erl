-module(hl_billing_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hl_billing_sup:start_link().

stop(_State) ->
    ok.
