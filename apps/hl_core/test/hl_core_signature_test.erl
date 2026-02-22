-module(hl_core_signature_test).
-include_lib("eunit/include/eunit.hrl").

signing_input_format_test() ->
    Input = hl_core_signature:signing_input(1700000000000, <<"hello">>),
    ?assertEqual(<<"1700000000000.hello">>, Input).

sign_verify_test() ->
    Secret  = <<"my-secret-key">>,
    Ts      = 1700000000000,
    Body    = <<"hello world">>,
    Input   = hl_core_signature:signing_input(Ts, Body),
    Sig     = hl_core_signature:sign(Secret, Input),
    ?assert(is_binary(Sig)),
    ?assertEqual(32, byte_size(Sig)),
    ?assert(hl_core_signature:verify(Secret, Input, Sig)).

wrong_secret_test() ->
    Ts    = erlang:system_time(millisecond),
    Input = hl_core_signature:signing_input(Ts, <<"data">>),
    Sig   = hl_core_signature:sign(<<"secret">>, Input),
    ?assertNot(hl_core_signature:verify(<<"wrong">>, Input, Sig)).

header_value_format_test() ->
    Ts = 1700000000000,
    H  = hl_core_signature:header_value(<<"key">>, Ts, <<"payload">>),
    %% Must start with "v1="
    ?assertMatch(<<"v1=", _/binary>>, H),
    %% "v1=" (3) + 64 hex chars = 67
    ?assertEqual(67, byte_size(H)).

header_not_sha256_prefix_test() ->
    H = hl_core_signature:header_value(<<"k">>, 0, <<"b">>),
    ?assertEqual(nomatch, binary:match(H, <<"sha256=">>)).
