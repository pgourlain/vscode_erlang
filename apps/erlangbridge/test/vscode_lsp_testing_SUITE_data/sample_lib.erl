-module(sample_lib).
-export([double/1, triple/1]).

double(X) ->
    X * 2.

triple(X) ->
    X * 3.
