-module(syntax_error).
-export([go/0]).

go() ->
    X = 1
    X + 1.
