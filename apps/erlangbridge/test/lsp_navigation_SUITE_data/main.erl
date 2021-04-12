-module(main).
-compile(export_all).

start(RoleId) ->
    %% You could write it like this
    lib_test:apply_cast(mod_test, run, [RoleId]),
    %% or like this
    {mod_test, run, [RoleId]},
    %% And even that
    {mod_test, run},
    ok.

    %% go to definition valid for both `mod_test` and `run`

start_local() ->
    functionA([1]).

functionA(_)->
    ok.