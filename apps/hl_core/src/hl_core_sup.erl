-module(hl_core_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    SubCache = #{
        id      => hl_subscription_cache,
        start   => {hl_subscription_cache, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type    => worker
    },
    IdempotencySpec = #{
        id      => hl_core_idempotency,
        start   => {hl_core_idempotency, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type    => worker
    },
    {ok, {SupFlags, [SubCache, IdempotencySpec]}}.
