%% Compiled by lsp_testing_SUITE:init_per_testcase/2 into
%% codepath_project/_build/default/lib/codepath_dep/ebin/. Its *source*
%% deliberately lives outside codepath_project/ - the root the test case
%% sets - so the only way lsp_testing:run_tests/2 can reach this module is
%% by putting that ebin directory on the code path.
-module(codepath_dep).

-export([answer/0]).

answer() -> 42.
