-module(include_and_module_problems).
-include("broken_include.hrl").
-export([go/0]).

go() ->
    X = 1,
    ok.
