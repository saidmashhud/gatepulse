-module(hl_stream_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    PubSub = #{id => hl_stream_pubsub,
               start => {hl_stream_pubsub, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker},
    {ok, {SupFlags, [PubSub]}}.
