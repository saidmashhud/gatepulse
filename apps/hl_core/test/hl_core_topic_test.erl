-module(hl_core_topic_test).
-include_lib("eunit/include/eunit.hrl").

exact_match_test() ->
    ?assert(hl_core_topic:matches(<<"orders.created">>, <<"orders.created">>)),
    ?assertNot(hl_core_topic:matches(<<"orders.created">>, <<"orders.updated">>)).

star_wildcard_test() ->
    ?assert(hl_core_topic:matches(<<"orders.*">>, <<"orders.created">>)),
    ?assert(hl_core_topic:matches(<<"orders.*">>, <<"orders.updated">>)),
    ?assertNot(hl_core_topic:matches(<<"orders.*">>, <<"orders.created.v2">>)).

hash_wildcard_test() ->
    ?assert(hl_core_topic:matches(<<"orders.#">>, <<"orders.created">>)),
    ?assert(hl_core_topic:matches(<<"orders.#">>, <<"orders.created.v2">>)),
    ?assert(hl_core_topic:matches(<<"#">>, <<"orders.created.deeply.nested">>)),
    ?assertNot(hl_core_topic:matches(<<"payments.#">>, <<"orders.created">>)).

multi_segment_test() ->
    ?assert(hl_core_topic:matches(<<"*.created">>, <<"orders.created">>)),
    ?assert(hl_core_topic:matches(<<"*.created">>, <<"payments.created">>)),
    ?assertNot(hl_core_topic:matches(<<"*.created">>, <<"orders.updated">>)).
