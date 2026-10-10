-module(unused_variable).
-export([go/1]).

go(X) ->
    Y = X + 1,
    ok.
