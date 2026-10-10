-module(case_real_pattern).
-export([f/1]).

f(X) ->
    case X of
        {ok, V} -> V;
        error -> undefined
    end.
