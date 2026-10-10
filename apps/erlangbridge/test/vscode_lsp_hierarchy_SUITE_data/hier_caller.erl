-module(hier_caller).
-export([go/0]).

go() ->
    hier_gs:start_link(),
    hier_gs:helper(1).
