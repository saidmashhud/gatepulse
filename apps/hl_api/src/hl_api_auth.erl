-module(hl_api_auth).
-behaviour(cowboy_middleware).

-export([execute/2, require_scope/2]).

%% Public paths that skip auth
-define(PUBLIC_PATHS, [
    <<"/healthz">>,
    <<"/readyz">>,
    <<"/v1/health/embedded">>,
    <<"/metrics">>,
    <<"/openapi.yaml">>
]).

execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    IsPublic = lists:member(Path, ?PUBLIC_PATHS) orelse
               binary:match(Path, <<"/v1/dev/inbox/receive/">>) =/= nomatch orelse
               binary:match(Path, <<"/console">>) =/= nomatch,
    case IsPublic of
        true ->
            {ok, Req, Env};
        false ->
            AuthMode = hl_config:get_str("HL_AUTH_MODE", "api_key"),
            case extract_token(Req) of
                {ok, Token} ->
                    authenticate(AuthMode, Token, Req, Env);
                {error, _} ->
                    reply_unauthorized(Req, Env)
            end
    end.

%% service_token mode: shared secret proves caller identity;
%% tenant is resolved from the mandatory X-Tenant-Id request header.
%% Missing header → 400 (not 401) so callers get a clear error.
authenticate("service_token", Token, Req, Env) ->
    ServiceToken = list_to_binary(hl_config:get_str("HL_SERVICE_TOKEN", "")),
    if
        ServiceToken =:= <<>> orelse Token =/= ServiceToken ->
            reply_unauthorized(Req, Env);
        true ->
            case cowboy_req:header(<<"x-tenant-id">>, Req, undefined) of
                undefined ->
                    reply_missing_tenant(Req, Env);
                TenantId ->
                    Scopes = [<<"*">>],
                    Req2 = cowboy_req:set_resp_header(<<"x-tenant-id">>, TenantId, Req),
                    Req3 = Req2#{tenant_id => TenantId, scopes => Scopes},
                    Env2 = Env#{tenant_id => TenantId},
                    {ok, Req3, Env2}
            end
    end;

%% api_key mode (default): tenant is embedded in the token.
authenticate(_Mode, Token, Req, Env) ->
    case verify_token(Token) of
        {ok, TenantId} ->
            Scopes = get_scopes(Token),
            Req2 = cowboy_req:set_resp_header(<<"x-tenant-id">>, TenantId, Req),
            Req3 = Req2#{tenant_id => TenantId, scopes => Scopes},
            Env2 = Env#{tenant_id => TenantId},
            {ok, Req3, Env2};
        {error, _} ->
            reply_unauthorized(Req, Env)
    end.

extract_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> -> {ok, Token};
        undefined -> {error, missing};
        _         -> {error, invalid_format}
    end.

%% Used by api_key mode only. HL_ADMIN_KEY > HL_API_KEY > dynamic key store.
verify_token(Token) ->
    AdminKey      = list_to_binary(hl_config:get_str("HL_ADMIN_KEY", "")),
    EnvKey        = list_to_binary(hl_config:get_str("HL_API_KEY", "dev-secret")),
    DefaultTenant = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
    if
        AdminKey =/= <<>> andalso Token =:= AdminKey ->
            {ok, DefaultTenant};
        Token =:= EnvKey ->
            {ok, DefaultTenant};
        true ->
            case hl_api_key_store:lookup_by_token(Token) of
                {ok, #{tenant_id := TId}} -> {ok, TId};
                {error, not_found}        -> {error, unauthorized}
            end
    end.

%% Return 403 if Token doesn't have the required scope.
require_scope(Req, Scope) ->
    Scopes = maps:get(scopes, Req, [<<"*">>]),
    case lists:member(<<"*">>, Scopes) orelse lists:member(Scope, Scopes) of
        true  -> ok;
        false ->
            Body = jsx:encode(#{<<"error">> => <<"forbidden">>,
                                <<"code">>  => <<"INSUFFICIENT_SCOPE">>,
                                <<"required_scope">> => Scope}),
            cowboy_req:reply(403,
                #{<<"content-type">> => <<"application/json">>},
                Body, Req),
            {stop, 403}
    end.

%% Get scopes for a token (api_key mode only).
%% service_token callers receive [<<"*">>] directly in authenticate/4.
%%   HL_ADMIN_KEY  → [<<"admin">>, <<"*">>]  (all scopes + admin-only ops)
%%   HL_API_KEY    → [<<"*">>]               (all scopes, backward compat)
%%   dynamic key   → whatever was set at creation
get_scopes(Token) ->
    AdminKey = list_to_binary(hl_config:get_str("HL_ADMIN_KEY", "")),
    EnvKey   = list_to_binary(hl_config:get_str("HL_API_KEY", "dev-secret")),
    if
        AdminKey =/= <<>> andalso Token =:= AdminKey ->
            [<<"admin">>, <<"*">>];
        Token =:= EnvKey ->
            [<<"*">>];
        true ->
            case hl_api_key_store:lookup_scopes(Token) of
                []     -> [];
                Scopes -> Scopes
            end
    end.

reply_unauthorized(Req, Env) ->
    logger:warning(#{event => auth_failed,
                     path  => cowboy_req:path(Req),
                     peer  => cowboy_req:peer(Req)}),
    Body = jsx:encode(#{<<"error">> => <<"unauthorized">>,
                        <<"code">>  => <<"AUTH_REQUIRED">>}),
    Req2 = cowboy_req:reply(401,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {stop, Req2, Env}.

reply_missing_tenant(Req, Env) ->
    logger:warning(#{event => missing_tenant_header,
                     path  => cowboy_req:path(Req)}),
    Body = jsx:encode(#{<<"error">> => <<"missing_tenant">>,
                        <<"code">>  => <<"X_TENANT_ID_REQUIRED">>,
                        <<"message">> =>
                            <<"X-Tenant-Id header is required in service_token auth mode">>}),
    Req2 = cowboy_req:reply(400,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {stop, Req2, Env}.
