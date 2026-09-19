-module(extract_multi).
-export([f/1]).

f(X) ->
    A = X + 1,
    B = A * 2,
    D = A + B,
    D.
