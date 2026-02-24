%% Static plan definitions for HookLine billing.
%% Plans are fixed in config — not stored in DB.
-module(hl_billing_plans).
-compile({no_auto_import, [get/1]}).

-export([
    get/1,
    event_limit/1,
    endpoint_limit/1,
    retention_days/1,
    has_websocket/1,
    overage_rate/1,
    all/0
]).

-define(PLANS, #{
    <<"free">> => #{
        events            => 10000,
        endpoints         => 3,
        retention_days    => 3,
        websocket         => false,
        price_usd         => 0
    },
    <<"starter">> => #{
        events            => 100000,
        endpoints         => 10,
        retention_days    => 7,
        websocket         => false,
        price_usd         => 29
    },
    <<"growth">> => #{
        events            => 1000000,
        endpoints         => unlimited,
        retention_days    => 30,
        websocket         => true,
        price_usd         => 99
    },
    <<"business">> => #{
        events            => 10000000,
        endpoints         => unlimited,
        retention_days    => 90,
        websocket         => true,
        price_usd         => 399
    },
    <<"enterprise">> => #{
        events            => unlimited,
        endpoints         => unlimited,
        retention_days    => custom,
        websocket         => true,
        price_usd         => custom
    }
}).

get(PlanId) when is_binary(PlanId) ->
    maps:get(PlanId, ?PLANS, maps:get(<<"free">>, ?PLANS)).

event_limit(PlanId) ->
    maps:get(events, get(PlanId)).

endpoint_limit(PlanId) ->
    maps:get(endpoints, get(PlanId)).

retention_days(PlanId) ->
    maps:get(retention_days, get(PlanId)).

has_websocket(PlanId) ->
    maps:get(websocket, get(PlanId)).

%% Overage: $1 per 10,000 events above limit (paid plans only).
overage_rate(PlanId) ->
    Price = maps:get(price_usd, get(PlanId)),
    if
        Price =:= 0    -> none;
        Price =:= custom -> none;
        true           -> {events_per_dollar, 10000}
    end.

all() ->
    maps:keys(?PLANS).
