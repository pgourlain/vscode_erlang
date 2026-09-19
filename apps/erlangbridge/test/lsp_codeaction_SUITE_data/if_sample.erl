-module(if_sample).
-export([f/1]).

f(X) ->
    if
        X > 0 -> pos;
        true -> non_pos
    end.
