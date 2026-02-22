-module(hl_delivery_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_workers/0, stop_workers/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},

    Workers = hl_config:get_int(<<"HL_DELIVERY_WORKERS">>, 16),

    Poller = #{id      => hl_delivery_poller,
               start   => {hl_delivery_poller, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type    => worker},

    WorkerPool = #{id      => hl_delivery_worker_pool_sup,
                   start   => {hl_delivery_worker_pool_sup, start_link, [Workers]},
                   restart => permanent,
                   shutdown => infinity,
                   type    => supervisor},

    Compaction = #{id      => hl_compaction,
                   start   => {hl_compaction, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type    => worker},

    Registry = #{id      => hl_endpoint_registry,
                 start   => {hl_endpoint_registry, start_link, []},
                 restart => permanent,
                 shutdown => 5000,
                 type    => worker},

    ActorSup = #{id      => hl_delivery_actor_sup,
                 start   => {hl_delivery_actor_sup, start_link, []},
                 restart => permanent,
                 shutdown => infinity,
                 type    => supervisor},

    TenantMgr = #{id      => hl_tenant_manager,
                  start   => {hl_tenant_manager, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type    => worker},

    {ok, {SupFlags, [Poller, WorkerPool, Compaction,
                     Registry, ActorSup, TenantMgr]}}.

%% Start delivery poller and worker pool (called by leader node).
start_workers() ->
    case supervisor:start_child(?MODULE, #{
        id      => hl_delivery_poller,
        start   => {hl_delivery_poller, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type    => worker}) of
        {ok, _} -> ok;
        {error, already_present} ->
            supervisor:restart_child(?MODULE, hl_delivery_poller);
        {error, {already_started, _}} -> ok;
        Err -> Err
    end.

%% Stop delivery poller and worker pool (called when losing leadership).
stop_workers() ->
    _ = supervisor:terminate_child(?MODULE, hl_delivery_poller),
    _ = supervisor:delete_child(?MODULE, hl_delivery_poller),
    ok.
