-module(hl_presence_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{id => hl_presence_pubsub,
          start => {hl_presence_pubsub, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [hl_presence_pubsub]},
        #{id => hl_presence,
          start => {hl_presence, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [hl_presence]}
    ],
    {ok, {SupFlags, Children}}.
