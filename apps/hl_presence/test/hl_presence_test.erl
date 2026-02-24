%% hl_presence_test.erl
%% Unit tests for the HookLine presence system.
%%
%% Presence tracks which users are connected to WebSocket sessions.
%% Invariants:
%%   - join/leave are idempotent per session
%%   - a process crash triggers automatic offline (via monitor)
%%   - tenants are isolated — tenant A cannot see tenant B users

-module(hl_presence_test).
-include_lib("eunit/include/eunit.hrl").

%% ─── Setup / teardown ────────────────────────────────────────────────────────

setup() ->
    {ok, Pid} = hl_presence:start_link(),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid).

%% ─── Test: join and leave ─────────────────────────────────────────────────────

presence_join_leave_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         TenantId = <<"tenant-a">>,
         UserId   = <<"user-001">>,

         %% Join
         ok = hl_presence:join(Server, TenantId, UserId, self()),
         Members = hl_presence:list(Server, TenantId),
         ?assert(lists:member(UserId, Members)),

         %% Leave
         ok = hl_presence:leave(Server, TenantId, UserId),
         Members2 = hl_presence:list(Server, TenantId),
         ?assertNot(lists:member(UserId, Members2))
     end}.

%% ─── Test: automatic offline on process crash ────────────────────────────────

presence_auto_leave_on_crash_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         TenantId = <<"tenant-b">>,
         UserId   = <<"user-crash">>,

         %% Spawn a process that joins presence then dies
         Parent = self(),
         Pid = spawn(fun() ->
             ok = hl_presence:join(Server, TenantId, UserId, self()),
             Parent ! joined,
             receive stop -> ok end
         end),

         receive joined -> ok after 1000 -> ?assert(false) end,
         ?assert(lists:member(UserId, hl_presence:list(Server, TenantId))),

         %% Kill the process
         exit(Pid, kill),
         timer:sleep(50), % allow monitor to fire

         %% User should now be offline
         ?assertNot(lists:member(UserId, hl_presence:list(Server, TenantId)))
     end}.

%% ─── Test: tenant isolation ──────────────────────────────────────────────────

presence_tenant_isolation_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         %% Two tenants with users
         ok = hl_presence:join(Server, <<"tenant-a">>, <<"user-a1">>, self()),
         ok = hl_presence:join(Server, <<"tenant-a">>, <<"user-a2">>, self()),
         ok = hl_presence:join(Server, <<"tenant-b">>, <<"user-b1">>, self()),

         MembersA = hl_presence:list(Server, <<"tenant-a">>),
         MembersB = hl_presence:list(Server, <<"tenant-b">>),

         %% Tenant A sees only its own users
         ?assert(lists:member(<<"user-a1">>, MembersA)),
         ?assert(lists:member(<<"user-a2">>, MembersA)),
         ?assertNot(lists:member(<<"user-b1">>, MembersA)),

         %% Tenant B sees only its own users
         ?assert(lists:member(<<"user-b1">>, MembersB)),
         ?assertNot(lists:member(<<"user-a1">>, MembersB)),
         ?assertNot(lists:member(<<"user-a2">>, MembersB))
     end}.

%% ─── Test: idempotent join ────────────────────────────────────────────────────

presence_idempotent_join_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         TenantId = <<"tenant-c">>,
         UserId   = <<"user-idem">>,

         ok = hl_presence:join(Server, TenantId, UserId, self()),
         ok = hl_presence:join(Server, TenantId, UserId, self()), % duplicate

         Members = hl_presence:list(Server, TenantId),
         Count = length(lists:filter(fun(U) -> U =:= UserId end, Members)),
         ?assertEqual(1, Count)
     end}.

%% ─── Test: multiple users per tenant ─────────────────────────────────────────

presence_multiple_users_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         TenantId = <<"tenant-multi">>,
         Users = [<<"u1">>, <<"u2">>, <<"u3">>, <<"u4">>, <<"u5">>],
         lists:foreach(fun(U) ->
             ok = hl_presence:join(Server, TenantId, U, self())
         end, Users),

         Members = hl_presence:list(Server, TenantId),
         lists:foreach(fun(U) ->
             ?assert(lists:member(U, Members))
         end, Users),

         ?assertEqual(5, length(Members))
     end}.

%% ─── Test: leave unknown user is a no-op ─────────────────────────────────────

presence_leave_unknown_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         Result = hl_presence:leave(Server, <<"tenant-x">>, <<"nonexistent">>),
         ?assertEqual(ok, Result)
     end}.

%% ─── Test: presence count ─────────────────────────────────────────────────────

presence_count_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Server) ->
         TenantId = <<"tenant-count">>,

         ?assertEqual(0, hl_presence:count(Server, TenantId)),

         ok = hl_presence:join(Server, TenantId, <<"u1">>, self()),
         ok = hl_presence:join(Server, TenantId, <<"u2">>, self()),
         ?assertEqual(2, hl_presence:count(Server, TenantId)),

         ok = hl_presence:leave(Server, TenantId, <<"u1">>),
         ?assertEqual(1, hl_presence:count(Server, TenantId))
     end}.
