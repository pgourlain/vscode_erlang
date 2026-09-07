-module(sample_tests).

-include_lib("eunit/include/eunit.hrl").

passing_test() ->
    ?assertEqual(2, 1 + 1).

failing_test() ->
    ?assertEqual(1, 2).

generator_test_() ->
    [?_assertEqual(2, 1 + 1)].

not_a_test_case() ->
    ok.
