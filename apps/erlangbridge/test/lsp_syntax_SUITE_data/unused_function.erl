-module(unused_function).
-export([go/0]).

go() ->
    helper().

helper() ->
    ok.

unused() ->
    ok.
