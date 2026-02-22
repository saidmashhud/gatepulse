%% simple_one_for_one supervisor for per-endpoint delivery actors.
-module(hl_delivery_actor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy  => simple_one_for_one,
            intensity => 10,
            period    => 60},
          [#{id       => hl_endpoint_actor,
             start    => {hl_endpoint_actor, start_link, []},
             restart  => transient,
             shutdown => 5000,
             type     => worker}]}}.
