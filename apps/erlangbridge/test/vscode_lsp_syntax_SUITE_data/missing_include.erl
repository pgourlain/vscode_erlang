-module(missing_include).
-include("does_not_exist.hrl").
-export([go/0]).

go() ->
    ok.
