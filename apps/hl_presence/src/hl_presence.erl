-module(hl_presence).
-behaviour(gen_server).

-export([start_link/0,
         join/4, leave/2, is_online/2, list/2, subscribe/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, hl_presence_records).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Join presence. Key: {TenantId, UserId, Pid}.
join(TenantId, UserId, Topics, Pid) ->
    gen_server:call(?MODULE, {join, TenantId, UserId, Topics, Pid}).

%% Leave presence for the calling process.
leave(TenantId, UserId) ->
    gen_server:cast(?MODULE, {leave, TenantId, UserId, self()}).

is_online(TenantId, UserId) ->
    ets:select_count(?TAB, [{{{TenantId, UserId, '_'}, '_'}, [], [true]}]) > 0.

list(TenantId, TopicPattern) ->
    All = ets:select(?TAB, [{{{TenantId, '_', '_'}, '$1'}, [], ['$1']}]),
    filter_by_topic(All, TopicPattern).

subscribe(TenantId, _Pattern, Pid) ->
    hl_presence_pubsub:subscribe(TenantId, Pid).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    ets:new(?TAB, [named_table, public, bag,
                   {read_concurrency, true},
                   {keypos, 1}]),
    {ok, #{monitors => #{}}}.

handle_call({join, TenantId, UserId, Topics, Pid}, _From,
            #{monitors := Monitors} = State) ->
    Key  = {TenantId, UserId, Pid},
    Meta = #{<<"user_id">>   => UserId,
             <<"topics">>    => Topics,
             <<"joined_at">> => erlang:system_time(millisecond),
             <<"meta">>      => #{}},
    ets:insert(?TAB, {Key, Meta}),
    Ref  = erlang:monitor(process, Pid),
    Event = #{<<"type">>      => <<"presence">>,
              <<"event">>     => <<"online">>,
              <<"tenant_id">> => TenantId,
              <<"user_id">>   => UserId,
              <<"topics">>    => Topics,
              <<"ts">>        => erlang:system_time(millisecond)},
    hl_presence_pubsub:publish(TenantId, Event),
    {reply, ok, State#{monitors := Monitors#{Ref => {TenantId, UserId, Pid}}}};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({leave, TenantId, UserId, Pid}, State) ->
    do_leave(TenantId, UserId, Pid, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason},
            #{monitors := Monitors} = State) ->
    case maps:get(Ref, Monitors, undefined) of
        {TenantId, UserId, Pid} ->
            {noreply, NewState} = do_leave(TenantId, UserId, Pid,
                                            State#{monitors := maps:remove(Ref, Monitors)}),
            {noreply, NewState};
        undefined ->
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

do_leave(TenantId, UserId, Pid, #{monitors := Monitors} = State) ->
    Key = {TenantId, UserId, Pid},
    ets:delete(?TAB, Key),
    %% Remove matching monitor ref
    NewMonitors = maps:filter(fun(_Ref, {T, U, P}) ->
        not (T =:= TenantId andalso U =:= UserId andalso P =:= Pid)
    end, Monitors),
    Event = #{<<"type">>      => <<"presence">>,
              <<"event">>     => <<"offline">>,
              <<"tenant_id">> => TenantId,
              <<"user_id">>   => UserId,
              <<"ts">>        => erlang:system_time(millisecond)},
    hl_presence_pubsub:publish(TenantId, Event),
    {noreply, State#{monitors := NewMonitors}}.

filter_by_topic(Metas, <<"#">>) ->
    [M || {_Key, M} <- Metas];
filter_by_topic(Metas, Pattern) ->
    [M || {_Key, #{<<"topics">> := Topics} = M} <- Metas,
          lists:any(fun(T) -> hl_core_topic:matches(Pattern, T) end, Topics)].
