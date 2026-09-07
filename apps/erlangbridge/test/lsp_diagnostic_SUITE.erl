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
    workspace_diagnostic_covers_every_project_file,
    stale_validation_result_is_dropped_once_a_newer_edit_has_landed,
    fresh_publish_is_followed_by_a_diagnostic_refresh_request
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

%% Regression test for the out-of-order validate_file/2 race behind the
%% "quick fix leaves a stale red squiggle" bug: every incoming LSP message
%% is handled by gen_lsp_server via an independent spawn/1
%% (gen_lsp_server.erl:201/206), with no ordering guarantee, so a slow
%% validate started against an OLD document version can finish and publish
%% *after* a newer, correct one already did - clobbering the client's
%% diagnostics with stale results. Reproduced here deterministically (no
%% sleep/timing dependency) by directly driving lsp_handlers:
%% maybe_send_diagnostics/4 with a version captured before, and after, a
%% simulated intervening edit - exactly the scenario a workspace/applyEdit
%% from a quick fix (which fires a normal textDocument/didChange) creates.
stale_validation_result_is_dropped_once_a_newer_edit_has_landed(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "clean.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    StaleVersion = gen_lsp_doc_server:get_document_version(File),

    %% Simulate a newer edit (e.g. a quick fix's applyEdit) landing while
    %% the "old" validate_file call is still (hypothetically) computing.
    gen_lsp_doc_server:document_changed(File, <<Content/binary, "\n">>),
    FreshVersion = gen_lsp_doc_server:get_document_version(File),
    ?assertNotEqual(StaleVersion, FreshVersion),

    {ServerSocket, ClientSocket} = open_socket_pair(),
    StaleDiagnostic = #{type => error, info => #{line => 1, character => 1, message => <<"stale">>}},

    %% The stale validate_file, finishing late, must not publish.
    lsp_handlers:maybe_send_diagnostics(ServerSocket, File, StaleVersion, [StaleDiagnostic]),
    %% The current validate_file, finishing after, must publish.
    lsp_handlers:maybe_send_diagnostics(ServerSocket, File, FreshVersion, []),

    %% Both the stale-drop and the fresh publish's own refresh request (see
    %% the next test) can be on the wire here - drain raw bytes rather than
    %% decoding a single framed message, and check what actually landed.
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    %% The stale diagnostic never made it onto the wire at all.
    ?assertEqual(nomatch, binary:match(Raw, <<"stale">>)),
    %% Exactly the fresh, empty diagnostics list was published.
    ?assertNotEqual(nomatch, binary:match(Raw, <<"\"diagnostics\":[]">>)).

%% A pull-mode client (textDocument/diagnostic) may be showing a diagnostic
%% it pulled once and is not guaranteed to re-pull right after an edit on
%% its own schedule - every fresh publish must also nudge it to re-pull via
%% workspace/diagnostic/refresh (LSP 3.17), sent unconditionally (task
%% history: not gated on the client having declared
%% workspace.diagnostics.refreshSupport, per explicit product direction).
fresh_publish_is_followed_by_a_diagnostic_refresh_request(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "clean.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    Version = gen_lsp_doc_server:get_document_version(File),

    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:maybe_send_diagnostics(ServerSocket, File, Version, []),
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    ?assertNotEqual(nomatch, binary:match(Raw, <<"textDocument\\/publishDiagnostics">>)),
    ?assertNotEqual(nomatch, binary:match(Raw, <<"workspace\\/diagnostic\\/refresh">>)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

pull(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, FileName),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    lsp_handlers:textDocument_diagnostic(undefined, Params).

open_socket_pair() ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}], 2000),
    {ok, Server} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    {Server, Client}.

%% Raw bytes until the stream goes quiet, rather than a single
%% Content-Length-framed message: more than one notification/request can
%% legitimately be in flight per test case here (a dropped stale publish
%% alongside a fresh one, or a publish alongside its own refresh request),
%% and a framed single-message reader would silently drop whatever
%% trailing bytes of a *following* message arrived in the same TCP read.
drain_raw(Socket, Timeout, Acc) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Data} -> drain_raw(Socket, 1000, <<Acc/binary, Data/binary>>);
        {error, _} -> Acc
    end.
