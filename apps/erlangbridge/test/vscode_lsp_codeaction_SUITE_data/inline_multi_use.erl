-module(inline_multi_use).
-export([f/0]).

f() ->
    A = 1 + 2,
    B = A * 10,
    C = A + B,
    C.
