-module(lsp_diagnostic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 5.9: textDocument/diagnostic (per-document pull) and workspace/
%% diagnostic (project-wide pull), alongside the existing push model
%% (unchanged - lsp_syntax_SUITE/lsp_codeaction_SUITE already exercise
%% that). Both reuse to_lsp_diagnostic/1, the exact same wire shape
%% push already uses (lsp_handlers:send_diagnostics/3) - with_warning.erl
%% has one real erl_lint warning (X unused); clean.erl has none.

all() -> [
    pull_diagnostic_reports_a_real_warning,
    pull_diagnostic_reports_nothing_for_a_clean_file,
    workspace_diagnostic_covers_every_project_file
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
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

pull_diagnostic_reports_a_real_warning(Config) ->
    #{kind := <<"full">>, items := [Diagnostic]} = pull(Config, "with_warning.erl"),
    ?assertEqual(<<"variable 'X' is unused">>, maps:get(message, Diagnostic)),
    ?assertEqual(2, maps:get(severity, Diagnostic)).

pull_diagnostic_reports_nothing_for_a_clean_file(Config) ->
    ?assertEqual(#{kind => <<"full">>, items => []}, pull(Config, "clean.erl")).

%% One WorkspaceFullDocumentDiagnosticReport per project file, each with
%% its own uri and the same items shape the single-document pull uses.
workspace_diagnostic_covers_every_project_file(_Config) ->
    #{items := Reports} = lsp_handlers:workspace_diagnostic(undefined, #{}),
    ?assertEqual(2, length(Reports)),
    [WithWarningReport] = [R || R <- Reports, binary:match(maps:get(uri, R), <<"with_warning.erl">>) =/= nomatch],
    ?assertMatch(#{kind := <<"full">>, version := null, items := [_]}, WithWarningReport),
    [CleanReport] = [R || R <- Reports, binary:match(maps:get(uri, R), <<"clean.erl">>) =/= nomatch],
    ?assertMatch(#{kind := <<"full">>, items := []}, CleanReport).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

pull(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, FileName),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    lsp_handlers:textDocument_diagnostic(undefined, Params).
