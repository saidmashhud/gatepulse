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

%% ─── Replay attack: timestamp older than 5 minutes must be rejected ──────────

signature_replay_attack_test() ->
    Secret = <<"replay-secret">>,
    Body   = <<"payload">>,
    %% Timestamp 6 minutes in the past (> 5 min tolerance)
    OldTs  = erlang:system_time(millisecond) - 360_000,
    Input  = hl_core_signature:signing_input(OldTs, Body),
    Sig    = hl_core_signature:sign(Secret, Input),
    %% Signature itself is valid — but the timestamp is too old
    ?assertNot(hl_core_signature:verify_with_timestamp(Secret, OldTs, Body, Sig)).

%% ─── Fresh timestamp within tolerance is accepted ────────────────────────────

signature_fresh_timestamp_accepted_test() ->
    Secret  = <<"fresh-secret">>,
    Body    = <<"payload">>,
    NowTs   = erlang:system_time(millisecond),
    Input   = hl_core_signature:signing_input(NowTs, Body),
    Sig     = hl_core_signature:sign(Secret, Input),
    ?assert(hl_core_signature:verify_with_timestamp(Secret, NowTs, Body, Sig)).

%% ─── Timestamp at exactly 5 minutes boundary is accepted ─────────────────────

signature_boundary_timestamp_test() ->
    Secret   = <<"boundary-secret">>,
    Body     = <<"boundary-payload">>,
    BoundTs  = erlang:system_time(millisecond) - 299_000, %% just under 5 min
    Input    = hl_core_signature:signing_input(BoundTs, Body),
    Sig      = hl_core_signature:sign(Secret, Input),
    ?assert(hl_core_signature:verify_with_timestamp(Secret, BoundTs, Body, Sig)).
