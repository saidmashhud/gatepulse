%% Tenant management handler. All operations require admin scope.
%%
%% Routes:
%%   POST   /v1/tenants          — create tenant
%%   GET    /v1/tenants/:id      — get tenant
%%   DELETE /v1/tenants/:id      — delete tenant + purge all data
%%   GET    /v1/tenants/:id/stats— delivery stats for tenant
-module(gp_api_h_tenants).
-export([init/2]).

init(Req0, Opts) ->
    case gp_api_auth:require_scope(Req0, <<"admin">>) of
        ok ->
            Method   = cowboy_req:method(Req0),
            TenantId = cowboy_req:binding(id, Req0),
            Action   = maps:get(action, Opts, undefined),
            handle(Method, TenantId, Action, Req0, Opts);
        {stop, _} ->
            {ok, Req0, Opts}
    end.

%%--------------------------------------------------------------------
%% POST /v1/tenants — create
%%--------------------------------------------------------------------
handle(<<"POST">>, undefined, undefined, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Params   = decode_body(Body),
    Name     = maps:get(<<"name">>, Params, <<"unnamed">>),
    TenantId = case maps:get(<<"id">>, Params, undefined) of
        undefined -> list_to_binary(gp_core_uuid:generate_str());
        Id        -> Id
    end,
    case gp_tenant_store:create(TenantId, Name) of
        {ok, TId} ->
            {ok, Tenant} = gp_tenant_store:get(TId),
            reply_json(201, Tenant, Req1, Opts);
        {error, already_exists} ->
            gp_api_error:reply(Req0, 409, conflict,
                               <<"Tenant already exists">>),
            {ok, Req0, Opts}
    end;

%%--------------------------------------------------------------------
%% GET /v1/tenants/:id — get
%%--------------------------------------------------------------------
handle(<<"GET">>, TenantId, undefined, Req0, Opts) when TenantId =/= undefined ->
    case gp_tenant_store:get(TenantId) of
        {ok, Tenant} ->
            reply_json(200, Tenant, Req0, Opts);
        {error, not_found} ->
            gp_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts}
    end;

%%--------------------------------------------------------------------
%% GET /v1/tenants/:id/stats — stats
%%--------------------------------------------------------------------
handle(<<"GET">>, TenantId, stats, Req0, Opts) when TenantId =/= undefined ->
    case gp_tenant_store:exists(TenantId) of
        false ->
            gp_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts};
        true ->
            Stats = collect_stats(TenantId),
            reply_json(200, Stats, Req0, Opts)
    end;

%%--------------------------------------------------------------------
%% DELETE /v1/tenants/:id — delete + purge
%%--------------------------------------------------------------------
handle(<<"DELETE">>, TenantId, undefined, Req0, Opts) when TenantId =/= undefined ->
    case gp_tenant_store:get(TenantId) of
        {error, not_found} ->
            gp_api_error:reply(Req0, 404, not_found, <<"Tenant not found">>),
            {ok, Req0, Opts};
        {ok, _} ->
            purge_tenant(TenantId),
            Req = cowboy_req:reply(204, #{}, <<>>, Req0),
            {ok, Req, Opts}
    end;

handle(_, _, _, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

%%--------------------------------------------------------------------
%% Purge: stop actors, remove keys, remove endpoints + subscriptions
%%        from C store. Events/attempts stay (compaction handles them).
%%--------------------------------------------------------------------
purge_tenant(TenantId) ->
    %% 1. Stop all endpoint actors
    case gp_store_client:list_endpoints(TenantId) of
        {ok, #{<<"endpoints">> := Endpoints}} ->
            lists:foreach(fun(Ep) ->
                EpId = maps:get(<<"endpoint_id">>, Ep, undefined),
                EpId =/= undefined andalso
                    catch gp_tenant_manager:stop_actor(EpId)
            end, Endpoints);
        _ -> ok
    end,
    %% 2. Remove subscriptions from C store + subscription cache
    case gp_store_client:list_subscriptions(TenantId) of
        {ok, #{<<"subscriptions">> := Subs}} ->
            lists:foreach(fun(Sub) ->
                SubId = maps:get(<<"subscription_id">>, Sub, undefined),
                SubId =/= undefined andalso begin
                    catch gp_subscription_cache:remove(TenantId, SubId),
                    catch gp_store_client:delete_subscription(SubId)
                end
            end, Subs);
        _ -> ok
    end,
    %% 3. Remove endpoints from C store
    case gp_store_client:list_endpoints(TenantId) of
        {ok, #{<<"endpoints">> := Eps2}} ->
            lists:foreach(fun(Ep) ->
                EpId = maps:get(<<"endpoint_id">>, Ep, undefined),
                EpId =/= undefined andalso
                    catch gp_store_client:delete_endpoint(EpId)
            end, Eps2);
        _ -> ok
    end,
    %% 4. Remove all API keys for this tenant
    catch gp_api_key_store:delete_tenant(TenantId),
    %% 5. Mark tenant as deleted
    gp_tenant_store:delete(TenantId),
    logger:info(#{event => tenant_purged, tenant_id => TenantId}).

%%--------------------------------------------------------------------
%% Stats
%%--------------------------------------------------------------------
collect_stats(TenantId) ->
    QueueStats = case gp_store_client:queue_stats(TenantId) of
        {ok, S} -> S;
        _       -> #{}
    end,
    DlqCount = case gp_store_client:list_dlq(TenantId) of
        {ok, #{<<"entries">> := Entries}} -> length(Entries);
        _ -> 0
    end,
    KeyCount  = length(gp_api_key_store:list(TenantId)),
    #{<<"tenant_id">>   => TenantId,
      <<"queue_depth">> => maps:get(<<"queue_depth">>, QueueStats, 0),
      <<"in_flight">>   => maps:get(<<"in_flight">>,   QueueStats, 0),
      <<"dlq_count">>   => DlqCount,
      <<"api_keys">>    => KeyCount}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
decode_body(<<>>) -> #{};
decode_body(B) ->
    case catch jsx:decode(B, [return_maps]) of
        M when is_map(M) -> M;
        _                -> #{}
    end.

reply_json(Status, Body, Req, Opts) ->
    Resp = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req),
    {ok, Resp, Opts}.
