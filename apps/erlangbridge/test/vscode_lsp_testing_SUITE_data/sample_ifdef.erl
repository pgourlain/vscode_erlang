-module(sample_ifdef).

-export([add/2]).

add(A, B) -> A + B.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual(3, add(1, 2)).
-endif.
