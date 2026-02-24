-module(hl_billing_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},

    Counters = #{id       => hl_billing_counters,
                 start    => {hl_billing_counters, start_link, []},
                 restart  => permanent,
                 shutdown => 5000,
                 type     => worker},

    Period = #{id       => hl_billing_period,
               start    => {hl_billing_period, start_link, []},
               restart  => permanent,
               shutdown => 5000,
               type     => worker},

    %% Metrics registration is a one-shot — wrap it as an anonymous child
    %% that exits immediately after init.
    _ = hl_billing_metrics:init(),

    {ok, {SupFlags, [Counters, Period]}}.
