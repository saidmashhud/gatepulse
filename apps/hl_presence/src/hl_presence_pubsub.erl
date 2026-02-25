-module(hl_presence_pubsub).
-behaviour(gen_server).

-export([start_link/0, subscribe/2, unsubscribe/2, publish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PG_SCOPE, hl_presence_pg).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    pg:start_link(?PG_SCOPE),
    {ok, #{}}.

subscribe(TenantId, Pid) ->
    pg:join(?PG_SCOPE, TenantId, Pid).

unsubscribe(TenantId, Pid) ->
    pg:leave(?PG_SCOPE, TenantId, Pid).

publish(TenantId, Event) ->
    lists:foreach(fun(P) -> P ! {presence_event, Event} end,
                  pg:get_members(?PG_SCOPE, TenantId)).

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
