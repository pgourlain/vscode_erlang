-module(lsp_folding_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 5.1. fold_source.erl exercises every emitted kind at once: a
%% %% region/%% endregion pair, a two-line doc comment block, a multi-line
%% -export(...), a `case ... end` whose own `end` sits alone on its own
%% line (the exact case lsp_folding:clause_end_line/2's own
%% CHARACTERIZATION note explains - a naive "largest line found in the
%% AST" walk would stop one line short here), a single-clause function
%% (no separate whole-function range - see g/0, one line, no range at
%% all), and a two-clause function (h/1: one whole-function range plus
%% one per clause).

all() -> [
    export_and_comment_and_region_ranges,
    single_clause_function_has_no_whole_function_range,
    case_block_spans_through_its_own_end_line,
    multi_clause_function_has_a_whole_function_range_and_one_per_clause,
    single_line_function_has_no_range_at_all
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

init_per_testcase(_TestCase, Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

export_and_comment_and_region_ranges(Config) ->
    Ranges = ranges(Config),
    ?assert(lists:member({3, 4, undefined}, Ranges)),
    ?assert(lists:member({7, 8, <<"comment">>}, Ranges)),
    ?assert(lists:member({2, 5, <<"region">>}, Ranges)).

single_clause_function_has_no_whole_function_range(Config) ->
    Ranges = ranges(Config),
    %% f/1 has one clause spanning lines 9-13 - that clause range doubles
    %% as the function's own, so there is exactly one (9, _, _) entry, not
    %% two identical ones.
    ?assertEqual(1, length([R || {9, _, _} = R <- Ranges])).

%% The case's own `end.` is on line 13, one line after its last real
%% clause body (`_ -> nonzero` on line 12) - both the case block AND the
%% enclosing clause must fold through line 13, not stop at 12.
case_block_spans_through_its_own_end_line(Config) ->
    Ranges = ranges(Config),
    ?assert(lists:member({10, 13, undefined}, Ranges)),
    ?assert(lists:member({9, 13, undefined}, Ranges)).

multi_clause_function_has_a_whole_function_range_and_one_per_clause(Config) ->
    Ranges = ranges(Config),
    ?assert(lists:member({17, 20, undefined}, Ranges)),
    ?assert(lists:member({17, 18, undefined}, Ranges)),
    ?assert(lists:member({19, 20, undefined}, Ranges)).

single_line_function_has_no_range_at_all(Config) ->
    Ranges = ranges(Config),
    ?assertNot(lists:any(fun ({SL, _, _}) -> SL =:= 15 end, Ranges)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

ranges(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "fold_source.erl"),
    lsp_folding:folding_ranges(File).
