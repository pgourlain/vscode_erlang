%% The shape every real project has and no other fixture in this data dir
%% had until now: a test module that calls *another* module of the project.
%% Before lsp_testing:add_project_ebin_paths/0 existed, this failed with
%% `undef`, not with an assertion failure.
-module(dep_user_tests).

-include_lib("eunit/include/eunit.hrl").

calls_dependency_test() ->
    ?assertEqual(42, codepath_dep:answer()).
