-module(hl_cluster_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => hl_leader,
          start => {hl_leader, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [hl_leader]}
    ],
    {ok, {#{strategy => one_for_one, intensity => 3, period => 5}, Children}}.
