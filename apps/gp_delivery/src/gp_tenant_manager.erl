%% Manages the lifecycle of per-endpoint delivery actors.
%% On startup loads all endpoints from C store and boots actors.
%% HTTP handlers call start_actor/stop_actor/update_actor for live changes.
-module(gp_tenant_manager).
-behaviour(gen_server).

-export([start_link/0, start_actor/1, stop_actor/1, update_actor/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_actor(Ep) ->
    supervisor:start_child(gp_delivery_actor_sup, [Ep]).

stop_actor(EpId) ->
    case gp_endpoint_registry:lookup(EpId) of
        {ok, Pid} -> supervisor:terminate_child(gp_delivery_actor_sup, Pid);
        not_found -> ok
    end.

update_actor(Ep) ->
    EpId = maps:get(<<"endpoint_id">>, Ep),
    case gp_endpoint_registry:lookup(EpId) of
        {ok, Pid} -> gen_server:cast(Pid, {update_config, Ep});
        not_found -> start_actor(Ep)
    end.

%% ── gen_server ───────────────────────────────────────────────────────────────

init([]) ->
    {ok, #{}, {continue, load}}.

handle_continue(load, State) ->
    TenantId = list_to_binary(gp_config:get_str("GP_TENANT_ID", "default")),
    case gp_store_client:list_endpoints(TenantId) of
        {ok, #{<<"items">> := Eps}} ->
            lists:foreach(fun(Ep) ->
                Dec = decode_ep(Ep),
                case maps:get(<<"enabled">>, Dec, true) of
                    true -> start_actor(Dec);
                    _    -> ok
                end
            end, Eps);
        _ ->
            ok
    end,
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

decode_ep(Ep) when is_map(Ep) -> Ep;
decode_ep(B) when is_binary(B)  -> jsx:decode(B, [return_maps]).
