-module(hl_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%%--------------------------------------------------------------------
%% cowboy_websocket callbacks
%%--------------------------------------------------------------------

init(Req, _Opts) ->
    TenantId = maps:get(tenant_id, Req, <<"default">>),
    UserId   = maps:get(user_id,   Req, undefined),
    QS       = cowboy_req:parse_qs(Req),
    Topics   = parse_topics(proplists:get_value(<<"topics">>, QS, <<"#">>)),
    State    = #{tenant_id => TenantId, user_id => UserId, topics => Topics},
    case hl_billing:plan_feature(TenantId, websocket) of
        enabled ->
            {cowboy_websocket, Req, State,
             #{idle_timeout => 60_000, compress => true}};
        disabled ->
            Body = jsx:encode(#{<<"error">> =>
                                <<"websocket requires growth plan or higher">>}),
            Req1 = cowboy_req:reply(403,
                #{<<"content-type">> => <<"application/json">>}, Body, Req),
            {ok, Req1, State}
    end.

websocket_init(State) ->
    #{tenant_id := TId, user_id := Uid} = State,
    hl_stream_pubsub:subscribe(TId, <<"#">>, self()),
    maybe_join_presence(TId, Uid, maps:get(topics, State), self()),
    erlang:send_after(30_000, self(), keepalive),
    {ok, State}.

websocket_handle({text, Data}, State) ->
    case catch jsx:decode(Data, [return_maps]) of
        #{<<"type">> := <<"publish">>}     = M -> handle_publish(M, State);
        #{<<"type">> := <<"subscribe">>}   = M -> handle_subscribe(M, State);
        #{<<"type">> := <<"unsubscribe">>} = M -> handle_unsubscribe(M, State);
        #{<<"type">> := <<"read">>}        = M -> handle_read(M, State);
        _ -> {ok, State}
    end;
websocket_handle({ping, _}, State) ->
    {reply, {pong, <<>>}, State};
websocket_handle(_, State) ->
    {ok, State}.

websocket_info({hl_stream, _TId, Event}, State) ->
    Topics     = maps:get(topics, State, [<<"#">>]),
    EventTopic = maps:get(<<"topic">>, Event, <<>>),
    TargetUser = maps:get(<<"target_user_id">>, Event, undefined),
    MyUser     = maps:get(user_id, State),
    Deliver    = topic_matches_any(Topics, EventTopic) andalso
                 (TargetUser =:= undefined orelse TargetUser =:= MyUser),
    case Deliver of
        true ->
            Frame = jsx:encode(#{
                <<"type">>      => <<"event">>,
                <<"id">>        => maps:get(<<"id">>, Event, <<>>),
                <<"topic">>     => EventTopic,
                <<"payload">>   => maps:get(<<"payload">>, Event, #{}),
                <<"timestamp">> => maps:get(<<"created_at">>, Event, 0)
            }),
            {reply, {text, Frame}, State};
        false ->
            {ok, State}
    end;
websocket_info({presence_event, PEvt}, State) ->
    {reply, {text, jsx:encode(PEvt)}, State};
websocket_info(keepalive, State) ->
    erlang:send_after(30_000, self(), keepalive),
    {reply, ping, State};
websocket_info(_, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    #{tenant_id := TId, user_id := Uid} = State,
    hl_stream_pubsub:unsubscribe(TId, <<"#">>, self()),
    case Uid of
        undefined -> ok;
        U         -> hl_presence:leave(TId, U)
    end.

%%--------------------------------------------------------------------
%% Message handlers
%%--------------------------------------------------------------------

handle_publish(#{<<"topic">> := Topic, <<"payload">> := Payload} = Msg, State) ->
    #{tenant_id := TId} = State,
    ClientId = maps:get(<<"client_id">>, Msg, undefined),
    RawMap   = #{<<"topic">> => Topic, <<"payload">> => Payload},
    case hl_core:publish_event(TId, RawMap) of
        {ok, Event, _} ->
            EventId = maps:get(<<"id">>, Event, <<>>),
            Ack = jsx:encode(#{<<"type">>      => <<"ack">>,
                               <<"client_id">> => ClientId,
                               <<"event_id">>  => EventId}),
            {reply, {text, Ack}, State};
        {ok, Event} ->
            EventId = maps:get(<<"id">>, Event, <<>>),
            Ack = jsx:encode(#{<<"type">>      => <<"ack">>,
                               <<"client_id">> => ClientId,
                               <<"event_id">>  => EventId}),
            {reply, {text, Ack}, State};
        {error, Reason} ->
            Err = jsx:encode(#{<<"type">>  => <<"error">>,
                               <<"error">> => Reason}),
            {reply, {text, Err}, State}
    end;
handle_publish(_, State) ->
    {ok, State}.

handle_subscribe(#{<<"topics">> := NewTopics}, State) when is_list(NewTopics) ->
    Existing = maps:get(topics, State, []),
    Merged   = lists:usort(Existing ++ NewTopics),
    {ok, State#{topics => Merged}};
handle_subscribe(_, State) ->
    {ok, State}.

handle_unsubscribe(#{<<"topics">> := Remove}, State) when is_list(Remove) ->
    Existing = maps:get(topics, State, []),
    Remaining = [T || T <- Existing, not lists:member(T, Remove)],
    {ok, State#{topics => Remaining}};
handle_unsubscribe(_, State) ->
    {ok, State}.

handle_read(#{<<"event_id">> := EventId}, State) ->
    #{tenant_id := TId} = State,
    hl_store_client:mark_read(TId, EventId),
    {ok, State};
handle_read(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

parse_topics(<<>>) ->
    [<<"#">>];
parse_topics(TopicsBin) when is_binary(TopicsBin) ->
    binary:split(TopicsBin, <<",">>, [global]);
parse_topics(_) ->
    [<<"#">>].

topic_matches_any(Topics, EventTopic) ->
    lists:any(fun(P) -> hl_core_topic:matches(P, EventTopic) end, Topics).

maybe_join_presence(_TId, undefined, _Topics, _Pid) ->
    ok;
maybe_join_presence(TId, Uid, Topics, Pid) ->
    hl_presence:join(TId, Uid, Topics, Pid),
    flush_offline_messages(TId, Uid).

flush_offline_messages(TId, Uid) ->
    case hl_store_client:list_user_dlq(TId, Uid) of
        {ok, Entries} ->
            lists:foreach(fun(Entry) ->
                Event  = maps:get(<<"event">>, Entry, #{}),
                JobId  = maps:get(<<"job_id">>, Entry, <<>>),
                self() ! {hl_stream, TId, Event},
                hl_store_client:delete_dlq(TId, JobId)
            end, Entries);
        _ ->
            ok
    end.
