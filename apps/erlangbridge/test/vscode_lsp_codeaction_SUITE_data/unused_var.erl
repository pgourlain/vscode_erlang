-module(unused_var).
-export([go/1]).

go(X) ->
    Y = X + 1,
    ok.
