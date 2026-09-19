-module(gen_spec).
-export([go/2, has_spec_already/1]).

go(X, _Y) ->
    X;
go(_A, B) ->
    B.

-spec has_spec_already(term()) -> term().
has_spec_already(X) ->
    X.
