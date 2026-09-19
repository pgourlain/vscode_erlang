-module(signature_source).

-export([add/2, no_spec_fun/2]).

-spec add(First :: integer(), Second :: integer()) -> integer().
add(First, Second) ->
    First + Second.

no_spec_fun(A, B) ->
    A + B.

caller() ->
    Sum = add(1, 2),
    Other = no_spec_fun(1, 2),
    {Sum, Other}.
