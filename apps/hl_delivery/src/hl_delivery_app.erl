-module(hl_delivery_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Init ETS tables used by rate limiter and semaphore
    hl_delivery_rate:init(),
    hl_delivery_sem:init(),
    %% Register Prometheus metrics
    hl_delivery_metrics:init(),
    hl_delivery_sup:start_link().

stop(_State) ->
    ok.
