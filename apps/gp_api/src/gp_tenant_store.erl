%% Persistent tenant metadata store.
%%
%% ETS is the hot-path (read_concurrency); DETS backs to disk.
%%
%% Record schema (4-tuple):
%%   {TenantId, Name, Status, CreatedAt}
%%     TenantId  :: binary()         — caller-supplied or generated UUID
%%     Name      :: binary()
%%     Status    :: active | deleted
%%     CreatedAt :: integer()        — unix ms
-module(gp_tenant_store).
-behaviour(gen_server).

-export([start_link/0]).
-export([create/2, get/1, list/0, delete/1, exists/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(ETS,  gp_tenants_ets).
-define(DETS, gp_tenants_dets).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Create a new tenant. Returns {ok, TenantId} | {error, already_exists}.
create(TenantId, Name) ->
    gen_server:call(?MODULE, {create, TenantId, Name}).

%% Returns {ok, Map} | {error, not_found}.
%% Direct ETS read.
get(TenantId) ->
    case ets:lookup(?ETS, TenantId) of
        [{TenantId, Name, Status, CreatedAt}] ->
            {ok, #{<<"tenant_id">>  => TenantId,
                   <<"name">>       => Name,
                   <<"status">>     => atom_to_binary(Status, utf8),
                   <<"created_at">> => CreatedAt}};
        [] ->
            {error, not_found}
    end.

%% List all active tenants. Direct ETS read.
list() ->
    ets:foldl(fun
        ({TenantId, Name, active, CreatedAt}, Acc) ->
            [#{<<"tenant_id">>  => TenantId,
               <<"name">>       => Name,
               <<"created_at">> => CreatedAt} | Acc];
        (_, Acc) ->
            Acc
    end, [], ?ETS).

%% Returns true if tenant exists and is active.
exists(TenantId) ->
    case ets:lookup(?ETS, TenantId) of
        [{_, _, active, _}] -> true;
        _                   -> false
    end.

%% Soft-delete: mark as deleted in ETS and DETS.
%% Caller is responsible for stopping actors and purging C store data.
delete(TenantId) ->
    gen_server:call(?MODULE, {delete, TenantId}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    DataDir  = gp_config:get_str("GP_DATA_DIR", "/tmp/gp_data"),
    DetsFile = filename:join(DataDir, "tenants.dets"),
    ok = filelib:ensure_dir(DetsFile),
    {ok, _} = dets:open_file(?DETS, [{file, DetsFile}, {type, set}]),
    ets:new(?ETS, [named_table, public, set, {read_concurrency, true}]),
    dets:foldl(fun(Record, ok) -> ets:insert(?ETS, Record), ok end, ok, ?DETS),
    Count = ets:info(?ETS, size),
    logger:info(#{event => tenant_store_loaded, tenants => Count}),
    {ok, #{}}.

handle_call({create, TenantId, Name}, _From, State) ->
    case ets:lookup(?ETS, TenantId) of
        [{_, _, active, _}] ->
            {reply, {error, already_exists}, State};
        _ ->
            Now    = erlang:system_time(millisecond),
            Record = {TenantId, Name, active, Now},
            ets:insert(?ETS, Record),
            ok = dets:insert(?DETS, Record),
            logger:info(#{event => tenant_created,
                          tenant_id => TenantId, name => Name}),
            {reply, {ok, TenantId}, State}
    end;

handle_call({delete, TenantId}, _From, State) ->
    case ets:lookup(?ETS, TenantId) of
        [] ->
            {reply, {error, not_found}, State};
        [{TenantId, Name, _Status, CreatedAt}] ->
            Record = {TenantId, Name, deleted, CreatedAt},
            ets:insert(?ETS, Record),
            ok = dets:insert(?DETS, Record),
            logger:info(#{event => tenant_deleted, tenant_id => TenantId}),
            {reply, ok, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    catch dets:close(?DETS),
    ok.
