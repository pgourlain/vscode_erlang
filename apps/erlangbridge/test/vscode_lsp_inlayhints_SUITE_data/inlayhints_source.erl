-module(inlayhints_source).

-export([add/2, named_but_ignored/1]).

-spec add(First :: integer(), Second :: integer()) -> integer().
add(First, Second) ->
    First + Second.

caller() ->
    add(1, 2).

-spec named_but_ignored(Count :: integer()) -> integer().
named_but_ignored(_) ->
    0.

use_named() ->
    named_but_ignored(5).

remote_caller() ->
    lists:reverse([1, 2, 3]).
