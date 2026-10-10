-module(rename_caller).
-export([call_it/0]).

call_it() ->
    rename_target:greet("world").
