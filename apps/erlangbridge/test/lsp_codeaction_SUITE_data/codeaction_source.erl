-module(codeaction_source).
-export([go/0]).

go() ->
    unused_helper(),
    ok.

unused_helper() ->
    ok.
