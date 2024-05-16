-module(navigation_source).

-export([start/0]).

start() ->
    L = [1,2,3,4,5,6,7,8],
    lists:filter(fun myfilter1/1, L),
    lists:filter(fun navigation_target:myexportedfilter/1, L).


myfilter1(_X) ->
    true.