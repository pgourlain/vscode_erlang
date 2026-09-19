-module(undefined_field).
-record(rec, {a, b}).
-export([go/0]).

go() ->
    R = #rec{a = 1, c = 2},
    R.
