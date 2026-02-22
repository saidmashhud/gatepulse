%% ETS-backed subscription cache.
%% Keeps {TenantId, SubId} -> subscription map in memory so that
%% hl_core:route_event does zero C store round-trips on the hot path.
-module(hl_subscription_cache).
-behaviour(gen_server).

-export([start_link/0, match/2, add/1, remove/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3]).

-define(TABLE, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ETS read — no C store round-trip on event publish.
%% Returns all subscriptions whose topic_pattern matches Topic.
match(TenantId, Topic) ->
    All = ets:match(?TABLE, {{TenantId, '_'}, '$1'}),
    [Sub || [Sub] <- All,
            hl_core_topic:matches(
                maps:get(<<"topic_pattern">>, Sub, <<"#">>), Topic)].

add(Sub) ->
    ets:insert(?TABLE, {{maps:get(<<"tenant_id">>, Sub),
                         maps:get(<<"subscription_id">>, Sub)}, Sub}).

remove(TenantId, SubId) ->
    ets:delete(?TABLE, {TenantId, SubId}).

%% ── gen_server ───────────────────────────────────────────────────────────────

init([]) ->
    ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    {ok, #{}, {continue, load}}.

handle_continue(load, State) ->
    TenantId = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
    case hl_store_client:list_subscriptions(TenantId) of
        {ok, #{<<"items">> := Subs}} ->
            lists:foreach(fun(Sub) ->
                add(decode_sub(Sub))
            end, Subs);
        _ ->
            ok
    end,
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

decode_sub(Sub) when is_map(Sub) -> Sub;
decode_sub(B) when is_binary(B)  -> jsx:decode(B, [return_maps]).
