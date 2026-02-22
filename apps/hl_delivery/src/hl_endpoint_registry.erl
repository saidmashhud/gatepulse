%% ETS registry mapping endpoint_id -> actor pid.
%% Monitors pids so entries are removed automatically on actor crash/stop.
-module(hl_endpoint_registry).
-behaviour(gen_server).

-export([start_link/0, register/2, unregister/1, lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(EpId, Pid) ->
    gen_server:call(?MODULE, {register, EpId, Pid}).

unregister(EpId) ->
    gen_server:call(?MODULE, {unregister, EpId}).

%% ETS read — no gen_server round-trip on lookup.
lookup(EpId) ->
    case ets:lookup(?TABLE, EpId) of
        [{_, Pid}] -> {ok, Pid};
        []         -> not_found
    end.

%% ── gen_server ───────────────────────────────────────────────────────────────
%% State: #{MonRef => EpId}

init([]) ->
    ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    {ok, #{}}.

handle_call({register, EpId, Pid}, _From, State) ->
    MonRef = erlang:monitor(process, Pid),
    ets:insert(?TABLE, {EpId, Pid}),
    {reply, ok, State#{MonRef => EpId}};

handle_call({unregister, EpId}, _From, State) ->
    ets:delete(?TABLE, EpId),
    NewState = maps:filter(fun(_Ref, Id) -> Id =/= EpId end, State),
    {reply, ok, NewState};

handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    case maps:get(Ref, State, undefined) of
        undefined ->
            {noreply, State};
        EpId ->
            ets:delete(?TABLE, EpId),
            {noreply, maps:remove(Ref, State)}
    end;

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
