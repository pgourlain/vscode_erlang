-module(inlinevalues_source).
-export([go/2]).

go(A, B) ->
    Sum = A + B,
    helper(A + B),
    helper(A),
    Sum.

helper(X) ->
    X.
