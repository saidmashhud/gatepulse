-module(hl_store_client_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},
    PoolSize = hl_config:get_int(<<"HL_STORE_POOL_SIZE">>, 8),
    PoolSpec = #{id => hl_store_pool,
                 start => {hl_store_pool, start_link, [PoolSize]},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker},
    {ok, {SupFlags, [PoolSpec]}}.
