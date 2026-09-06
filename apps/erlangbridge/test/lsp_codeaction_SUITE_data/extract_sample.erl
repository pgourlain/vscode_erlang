-module(extract_sample).
-export([f/1]).

f(X) ->
    A = X + 1,
    B = A * 2,
    C = B - 3,
    C.
