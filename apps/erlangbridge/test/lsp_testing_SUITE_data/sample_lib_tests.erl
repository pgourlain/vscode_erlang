-module(sample_lib_tests).

-include_lib("eunit/include/eunit.hrl").

double_test() ->
    ?assertEqual(4, sample_lib:double(2)).
