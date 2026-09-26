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
    pull_diagnostic_reports_nothing_for_a_header_file,
    workspace_diagnostic_covers_every_project_file,
    workspace_diagnostic_reports_an_open_document_as_empty,
    an_unsaved_edit_republishes_diagnostics_for_the_new_contents,
    stale_validation_result_is_dropped_once_a_newer_edit_has_landed,
    fresh_publish_is_followed_by_a_diagnostic_refresh_request,
    pull_diagnostic_reports_unchanged_when_result_id_matches,
    workspace_diagnostic_long_polls_until_something_changes,
    diagnostic_refreshes_are_debounced_and_uniquely_identified,
    identical_configuration_is_not_revalidated,
    configuration_never_publishes_an_empty_clear_ahead_of_the_real_state,
    concurrent_configuration_calls_do_not_clobber_a_later_edit
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
    ?assertMatch(#{kind := <<"full">>, resultId := _, items := []}, pull(Config, "clean.erl")).

%% #356: a header is not a module - linting it on its own reported "no module
%% definition" and every record it declares as unused.
pull_diagnostic_reports_nothing_for_a_header_file(Config) ->
    ?assertMatch(#{kind := <<"full">>, resultId := _, items := []}, pull(Config, "records.hrl")).

%% One WorkspaceFullDocumentDiagnosticReport per project file, each with
%% its own uri and the same items shape the single-document pull uses.
workspace_diagnostic_covers_every_project_file(_Config) ->
    #{items := Reports} = lsp_handlers:workspace_diagnostic(undefined, #{}),
    ?assertEqual(2, length(Reports)),
    [WithWarningReport] = [R || R <- Reports, binary:match(maps:get(uri, R), <<"with_warning.erl">>) =/= nomatch],
    ?assertMatch(#{kind := <<"full">>, version := null, resultId := _, items := [_]}, WithWarningReport),
    [CleanReport] = [R || R <- Reports, binary:match(maps:get(uri, R), <<"clean.erl">>) =/= nomatch],
    ?assertMatch(#{kind := <<"full">>, resultId := _, items := []}, CleanReport).

%% Regression test for every problem being listed twice (and every quick fix
%% offered twice): while a document is open, its diagnostics belong to the
%% push channel, so the workspace pull must not report them a second time
%% into the client's own DiagnosticCollection. Reported as an empty `full`
%% report rather than omitted - a URI missing from the report keeps whatever
%% the client last pulled for it, which would freeze the duplicate on screen
%% instead of clearing it.
workspace_diagnostic_reports_an_open_document_as_empty(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "with_warning.erl"),
    {ok, Content} = file:read_file(File),

    ?assertMatch(#{kind := <<"full">>, items := [_]}, workspace_report(<<"with_warning.erl">>)),

    gen_lsp_doc_server:document_opened(File, Content),
    ?assertMatch(#{kind := <<"full">>, items := []}, workspace_report(<<"with_warning.erl">>)),
    %% the file that stayed closed is still reported for real (clean.erl has
    %% nothing to report, but it is reported)
    ?assertMatch(#{kind := <<"full">>, items := []}, workspace_report(<<"clean.erl">>)),

    %% closing it hands the file back to the pull channel
    gen_lsp_doc_server:document_closed(File),
    ?assertMatch(#{kind := <<"full">>, items := [_]}, workspace_report(<<"with_warning.erl">>)).

%% Regression test for "the quick fix is applied but the problem stays in the
%% Problems list": a quick fix's WorkspaceEdit reaches the server as a plain
%% textDocument/didChange on an unsaved buffer. That used to be a no-op
%% whenever files.autoSave was on - nothing reparsed, nothing revalidated,
%% nothing published - so the problem the fix had just removed stayed listed
%% until the file was saved. The edit must publish the diagnostics of the new
%% contents, by itself.
an_unsaved_edit_republishes_diagnostics_for_the_new_contents(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "with_warning.erl"),
    {ok, Content} = file:read_file(File),
    Uri = lsp_utils:file_to_file_uri(File),

    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:textDocument_didOpen(ServerSocket, #{textDocument =>
        #{uri => Uri, text => Content, version => 1}}),
    OpenRaw = drain_raw(ClientSocket, 5000, <<>>),
    %% opening reports the warning the file really has
    ?assertNotEqual(nomatch, binary:match(OpenRaw, <<"variable 'X' is unused">>)),

    %% the fix: use X, so erl_lint has nothing left to say. No didSave, and
    %% no parse_document/1 - exactly what a quick fix's applyEdit produces.
    Fixed = <<"-module(with_warning).\n-export([go/0]).\n\ngo() ->\n    X = 1,\n    X.\n">>,
    lsp_handlers:textDocument_didChange(ServerSocket, #{
        textDocument => #{uri => Uri, version => 2},
        contentChanges => [#{text => Fixed}]}),
    ChangeRaw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    ?assertNotEqual(nomatch, binary:match(ChangeRaw, <<"textDocument\\/publishDiagnostics">>)),
    ?assertNotEqual(nomatch, binary:match(ChangeRaw, <<"\"diagnostics\":[]">>)),
    ?assertEqual(nomatch, binary:match(ChangeRaw, <<"variable 'X' is unused">>)),

    gen_lsp_doc_server:document_closed(File).

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

%% Handing back the resultId the server just gave us means "I already have
%% this" - the server must say so rather than resend the items. Without a
%% resultId on the way out, the client can never say this, which is half of
%% why the workspace pull below used to spin.
pull_diagnostic_reports_unchanged_when_result_id_matches(Config) ->
    #{resultId := ResultId} = pull(Config, "with_warning.erl"),
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "with_warning.erl"),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)},
               previousResultId => ResultId},
    ?assertEqual(#{kind => <<"unchanged">>, resultId => ResultId},
                 lsp_handlers:textDocument_diagnostic(undefined, Params)).

%% Regression test for the workspace/diagnostic spin: vscode-languageclient
%% re-issues the pull as soon as it is answered, so a pull with nothing new to
%% report must NOT answer straight away - it parks until diagnostics actually
%% change. Before this, every round answered instantly with full reports and
%% re-linted every project file, forever.
workspace_diagnostic_long_polls_until_something_changes(_Config) ->
    #{items := Reports} = lsp_handlers:workspace_diagnostic(undefined, #{}),
    Previous = [#{uri => maps:get(uri, R), value => maps:get(resultId, R)} || R <- Reports],
    Params = #{previousResultIds => Previous},

    Self = self(),
    Puller = spawn(fun () -> Self ! {pulled, lsp_handlers:workspace_diagnostic(undefined, Params)} end),

    %% Nothing changed, so the pull must still be parked.
    receive {pulled, _} -> ct:fail("workspace_diagnostic answered an unchanged pull immediately")
    after 1000 -> ok
    end,

    lsp_diagnostics:notify_changed(),
    receive
        {pulled, #{items := Woken}} ->
            ?assertEqual(length(Reports), length(Woken))
    after 5000 ->
        exit(Puller, kill),
        ct:fail("workspace_diagnostic stayed parked after a change was signalled")
    end.

%% One refresh per push restarts the client's whole workspace pull each time,
%% so a burst must collapse to one request - and each request needs its own
%% JSON-RPC id (they used to share a constant one, so three requests went out
%% and only one response ever came back).
diagnostic_refreshes_are_debounced_and_uniquely_identified(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "clean.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    Version = gen_lsp_doc_server:get_document_version(File),

    FirstIds = refresh_ids_of_a_push_burst(File, Version),
    ?assertEqual(1, length(FirstIds)),
    SecondIds = refresh_ids_of_a_push_burst(File, Version),
    ?assertEqual(1, length(SecondIds)),
    ?assertNotEqual(FirstIds, SecondIds).

%% workspace/configuration is answered more than once per session (the trace
%% behind this change shows configuration/2 running twice at startup), and each
%% run used to redo the whole project scan plus a validation pass per open
%% document. A response carrying settings we already hold has nothing to redo.
identical_configuration_is_not_revalidated(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "with_warning.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    Sections = [#{verbose => false, linting => true}, #{}, #{}, #{}, #{}],

    {FirstServer, FirstClient} = open_socket_pair(),
    lsp_handlers:configuration(FirstServer, Sections),
    FirstRaw = drain_raw(FirstClient, 5000, <<>>),
    gen_tcp:close(FirstServer),
    gen_tcp:close(FirstClient),
    ?assertNotEqual(nomatch, binary:match(FirstRaw, <<"textDocument\\/publishDiagnostics">>)),

    %% Same settings again: nothing to rescan, nothing to revalidate.
    {SecondServer, SecondClient} = open_socket_pair(),
    lsp_handlers:configuration(SecondServer, Sections),
    SecondRaw = drain_raw(SecondClient, 2000, <<>>),
    gen_tcp:close(SecondServer),
    gen_tcp:close(SecondClient),
    ?assertEqual(<<>>, SecondRaw).

%% Regression test for the "diagnostic desynchronised after a manual edit"
%% bug: configuration/2 used to publish an unguarded send_diagnostics(Socket,
%% File, []) clear ahead of validate_file for every open document - unlike
%% every other diagnostic publish (which goes through maybe_send_diagnostics/4
%% and is dropped if a newer edit has landed since), that clear had no version
%% check at all. With more than one configuration/2 call able to be in flight
%% at once (workspace_didChangeConfiguration/2's doc comment), a lagging
%% one's clear could reach the client after a genuinely newer, correct push
%% for the same file - blanking it with nothing to catch it. validate_file
%% alone (version-guarded) is what must appear on the wire.
configuration_never_publishes_an_empty_clear_ahead_of_the_real_state(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "with_warning.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    %% A marker unique to this call guarantees configuration/2 sees a genuine
    %% change and actually runs its per-document loop, rather than being
    %% short-circuited by the identical-sections check.
    Sections = [#{verbose => false, linting => true, marker => erlang:unique_integer()},
                #{}, #{}, #{}, #{}],

    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:configuration(ServerSocket, Sections),
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    %% Other documents opened by earlier test cases in this suite stay open
    %% for the whole suite run and legitimately have nothing to report (e.g.
    %% clean.erl) - scope the assertion to publishDiagnostics frames for
    %% *this* file rather than the raw byte stream, so their correctly-empty
    %% reports don't produce a false failure here.
    PublishesForFile = publishes_for(Raw, File),
    ?assertNotEqual([], PublishesForFile),
    lists:foreach(fun (Diagnostics) -> ?assertNotEqual([], Diagnostics) end, PublishesForFile).

%% Same regression, under the actual condition that made it visible: more
%% than one configuration/2 call running concurrently for the same open
%% document (VS Code firing Workspace.onDidChangeConfiguration more than
%% once for a single settings change - see workspace_didChangeConfiguration/2).
%% Whatever the interleaving, neither call has an unguarded write left to
%% race with, so no empty clear can ever land on the wire.
concurrent_configuration_calls_do_not_clobber_a_later_edit(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "with_warning.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),

    {ServerSocket, ClientSocket} = open_socket_pair(),
    Self = self(),
    Run = fun (Marker) ->
        spawn(fun () ->
            Sections = [#{verbose => false, linting => true, marker => Marker},
                        #{}, #{}, #{}, #{}],
            lsp_handlers:configuration(ServerSocket, Sections),
            Self ! {configuration_done, Marker}
        end)
    end,
    Run(1),
    Run(2),
    ?assertEqual(ok, receive {configuration_done, 1} -> ok after 5000 -> timeout end),
    ?assertEqual(ok, receive {configuration_done, 2} -> ok after 5000 -> timeout end),

    Raw = drain_raw(ClientSocket, 2000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    PublishesForFile = publishes_for(Raw, File),
    ?assertNotEqual([], PublishesForFile),
    lists:foreach(fun (Diagnostics) -> ?assertNotEqual([], Diagnostics) end, PublishesForFile).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

%% Three pushes back to back, then whatever refresh ids reached the client.
refresh_ids_of_a_push_burst(File, Version) ->
    {ServerSocket, ClientSocket} = open_socket_pair(),
    lists:foreach(fun (_) ->
        lsp_handlers:maybe_send_diagnostics(ServerSocket, File, Version, [])
    end, [1, 2, 3]),
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    case re:run(Raw, "\"(workspace_diagnostic_refresh#[0-9]+)\"",
                [global, {capture, all_but_first, binary}]) of
        {match, Matches} -> lists:usort([Id || [Id] <- Matches]);
        nomatch -> []
    end.

pull(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, FileName),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    lsp_handlers:textDocument_diagnostic(undefined, Params).

%% One named file's report out of a fresh whole-workspace pull. Sending no
%% previousResultIds makes every report `full`, so the pull answers at once
%% rather than parking in its long poll.
workspace_report(FileName) ->
    #{items := Reports} = lsp_handlers:workspace_diagnostic(undefined, #{}),
    [Report] = [R || R <- Reports, binary:match(maps:get(uri, R), FileName) =/= nomatch],
    Report.

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

%% The `diagnostics` list of every textDocument/publishDiagnostics frame in
%% Raw addressed to File, in wire order. More than one document can
%% legitimately be open across a whole suite run (each with its own,
%% independently correct, possibly-empty diagnostics), so scoping by uri
%% matters - see configuration_never_publishes_an_empty_clear_ahead_of_the_real_state.
publishes_for(Raw, File) ->
    Uri = lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
    [Diagnostics || #{method := <<"textDocument/publishDiagnostics">>,
                      params := #{uri := U, diagnostics := Diagnostics}} <- frames(Raw),
                    U =:= Uri].

%% Decode every Content-Length-framed JSON-RPC message in Raw, in wire order.
frames(Raw) ->
    frames(Raw, []).

frames(Raw, Acc) ->
    case binary:match(Raw, <<"\r\n\r\n">>) of
        nomatch ->
            lists:reverse(Acc);
        {HeadersEnd, HeadersSepLen} ->
            case re:run(Raw, "Content-Length: *([0-9]+)") of
                {match, [_, {LStart, LLen}]} ->
                    Length = binary_to_integer(binary:part(Raw, LStart, LLen)),
                    BodyStart = HeadersEnd + HeadersSepLen,
                    Available = byte_size(Raw) - BodyStart,
                    case Available >= Length of
                        true ->
                            Body = binary:part(Raw, BodyStart, Length),
                            {ok, Term, _} = vscode_jsone_decode:decode(Body, [{keys, atom}]),
                            Rest = binary:part(Raw, BodyStart + Length, Available - Length),
                            frames(Rest, [Term | Acc]);
                        false ->
                            lists:reverse(Acc)
                    end;
                nomatch ->
                    lists:reverse(Acc)
            end
    end.
