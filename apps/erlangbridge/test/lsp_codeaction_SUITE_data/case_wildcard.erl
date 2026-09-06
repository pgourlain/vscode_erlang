-module(case_wildcard).
-export([f/1]).

f(X) ->
    case X of
        _ when X > 0 -> pos;
        _ -> non_pos
    end.
