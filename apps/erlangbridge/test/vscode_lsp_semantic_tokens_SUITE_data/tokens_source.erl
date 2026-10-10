-module(tokens_source).
-export([f/1, g/1, old/0]).
-deprecated([{old, 0, "use f/1"}]).
-record(rec, {a, b = 0}).
-type my_type() :: integer().
-spec f(my_type()) -> atom().

-define(FOO, 42).

f(X) ->
    R = #rec{a = X, b = ?FOO},
    Y = R#rec.a,
    R2 = R#rec{a = Y},
    lists:reverse([X, Y]),
    g(X).

g(X) -> X.

old() -> ok.
