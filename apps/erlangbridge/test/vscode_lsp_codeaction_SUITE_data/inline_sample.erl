-module(inline_sample).
-export([f/0]).

f() ->
    A = 1 + 2,
    B = A * 10,
    B.
