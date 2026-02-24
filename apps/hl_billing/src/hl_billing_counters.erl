%% ETS-backed event counters with periodic flush to the C store.
%% Hot path: ets:update_counter/3 — sub-microsecond, no gen_server roundtrip.
-module(hl_billing_counters).
-behaviour(gen_server).

-export([start_link/0,
         track_event/2,
         flush/0,
         do_flush/0,
         get_count/2,
         current_period_start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, hl_billing_counters).
%% ETS key: {TenantId, field}
%% Fields: events_published | events_delivered | events_failed

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Hot path — called from hl_core on every publish/deliver/fail.
track_event(TenantId, Type) when Type =:= published;
                                  Type =:= delivered;
                                  Type =:= failed ->
    Field = field_key(Type),
    PeriodStart = current_period_start(),
    Key = {TenantId, PeriodStart, Field},
    ets:update_counter(?TAB, Key, 1, {Key, 0}),
    ok.

%% Return current in-memory count (for quota checks).
get_count(TenantId, Field) ->
    PeriodStart = current_period_start(),
    Key = {TenantId, PeriodStart, Field},
    case ets:lookup(?TAB, Key) of
        [{_, N}] -> N;
        []       -> 0
    end.

%% Manual flush (called before period close).
flush() ->
    gen_server:call(?MODULE, flush, 30000).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    ?TAB = ets:new(?TAB, [named_table, public, set,
                           {write_concurrency, true},
                           {read_concurrency, true}]),
    FlushMs = application:get_env(hl_billing, counter_flush_interval_ms, 60000),
    schedule_flush(FlushMs),
    {ok, #{flush_interval_ms => FlushMs}}.

handle_call(flush, _From, State) ->
    do_flush(),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State) ->
    do_flush(),
    schedule_flush(maps:get(flush_interval_ms, State, 60000)),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

do_flush() ->
    %% Snapshot and drain all counter entries.
    Entries = ets:tab2list(?TAB),
    lists:foreach(fun({{TenantId, PeriodStart, Field}, Count}) ->
        FieldBin = atom_to_binary(Field, utf8),
        catch hl_billing_store:increment_usage(TenantId, PeriodStart, FieldBin, Count)
    end, Entries),
    %% Reset counters to zero after flush (subtract what we just flushed).
    lists:foreach(fun({{TenantId, PeriodStart, Field}, Count}) ->
        Key = {TenantId, PeriodStart, Field},
        ets:update_counter(?TAB, Key, -Count, {Key, 0})
    end, Entries).

schedule_flush(Ms) ->
    erlang:send_after(Ms, self(), flush).

current_period_start() ->
    %% Start of current UTC month in milliseconds.
    NowSecs = erlang:system_time(second),
    {{Y, M, _D}, _} = calendar:gregorian_seconds_to_datetime(
        NowSecs + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
    Secs = calendar:datetime_to_gregorian_seconds({{Y, M, 1}, {0,0,0}})
           - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Secs * 1000.

field_key(published) -> events_published;
field_key(delivered) -> events_delivered;
field_key(failed)    -> events_failed.
