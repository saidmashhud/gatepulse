-module(gp_core_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Sup = gp_core_sup:start_link(),
    %% Deferred: wait 2 s for all apps (including gp_dev_inbox, gp_delivery)
    %% to fully start before checking/seeding.
    erlang:spawn(fun() ->
        timer:sleep(2000),
        first_run()
    end),
    Sup.

stop(_State) ->
    ok.

%% ── First-run detection ──────────────────────────────────────────────────────

first_run() ->
    TenantId = list_to_binary(gp_config:get_str("GP_TENANT_ID", "default")),
    case gp_store_client:list_endpoints(TenantId) of
        {ok, #{<<"items">> := []}} ->
            %% Store is empty — seed demo data and print welcome banner
            Token = seed_demo_data(TenantId),
            print_welcome(Token);
        _ ->
            ok
    end.

seed_demo_data(TenantId) ->
    Token = gp_core_uuid:generate_str(),
    Port  = gp_config:get_int("GP_PORT", 8080),
    InboxURL = iolist_to_binary([
        "http://localhost:", integer_to_binary(Port),
        "/v1/dev/inbox/receive/", Token
    ]),

    %% Register the inbox token (gp_dev_inbox may be in a different app;
    %% use catch so we don't crash if it's not loaded yet).
    catch gp_dev_inbox:create(Token),

    %% Create demo endpoint in the store
    EpId = gp_core_uuid:generate_str(),
    Now  = erlang:system_time(millisecond),
    Ep = #{
        <<"id">>          => EpId,
        <<"endpoint_id">> => EpId,
        <<"tenant_id">>   => TenantId,
        <<"url">>         => InboxURL,
        <<"name">>        => <<"Demo Inbox">>,
        <<"enabled">>     => true,
        <<"created_at">>  => Now
    },
    gp_store_client:put_endpoint(#{
        <<"endpoint_id">> => EpId,
        <<"payload">>     => jsx:encode(Ep),
        <<"tenant_id">>   => TenantId
    }),

    %% Create subscription for all topics
    SubId = gp_core_uuid:generate_str(),
    Sub = #{
        <<"id">>              => SubId,
        <<"subscription_id">> => SubId,
        <<"tenant_id">>       => TenantId,
        <<"endpoint_id">>     => EpId,
        <<"topic_pattern">>   => <<"#">>,
        <<"created_at">>      => Now
    },
    gp_store_client:put_subscription(Sub),

    %% Keep ETS caches consistent (ignore errors if not yet started)
    catch gp_subscription_cache:add(Sub),
    catch gp_tenant_manager:start_actor(Ep),

    Token.

print_welcome(Token) ->
    ApiKey = gp_config:get_str("GP_API_KEY", "dev-secret"),
    Port   = gp_config:get_int("GP_PORT", 8080),
    io:format(
        "~n=======================================================~n"
        "  GatePulse is running!~n~n"
        "  Console:    http://localhost:~B/console~n"
        "  API:        http://localhost:~B/v1~n"
        "  API Key:    ~s~n~n"
        "  Demo inbox ready. Publish your first event:~n~n"
        "  curl -X POST http://localhost:~B/v1/events \\~n"
        "    -H \"Authorization: Bearer ~s\" \\~n"
        "    -H \"Content-Type: application/json\" \\~n"
        "    -d '{\"topic\":\"hello.world\",\"payload\":{\"msg\":\"it works\"}}'~n~n"
        "  View deliveries at: http://localhost:~B/v1/dev/inbox/messages?token=~s~n"
        "  Or open:           http://localhost:~B/console~n"
        "=======================================================~n~n",
        [Port, Port, ApiKey, Port, ApiKey, Port, Token, Port]
    ).
