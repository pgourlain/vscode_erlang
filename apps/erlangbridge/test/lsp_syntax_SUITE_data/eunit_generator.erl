-module(eunit_generator).
-include_lib("eunit/include/eunit.hrl").

%% Plain test, already handled before #89.
plain_test() ->
    ?assert(true).

%% Generator: eunit discovers and runs these through their return value
%% (a fun/list of tests), never a direct call, so erl_lint sees them as
%% unused - just like plain_test/0, but the old filter only matched names
%% ending in "_test", not "_test_" (#89).
generator_test_() ->
    [?_assert(true)].
