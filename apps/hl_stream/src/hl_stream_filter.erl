-module(hl_stream_filter).
-export([matches/2]).

%% Delegate to hl_core_topic for pattern matching
matches(Pattern, Topic) ->
    hl_core_topic:matches(Pattern, Topic).
