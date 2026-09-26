-module(lsp_diagnostic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Diagnostics are pushed only (textDocument/publishDiagnostics): open
%% documents from their buffer, closed project files from disk, through
%% lsp_diagnostics' background queue. No pull (textDocument/diagnostic,
%% workspace/diagnostic): running both made the client hold two collections
%% for the same file, so every problem showed twice on hover and in quick
%% fixes. with_warning.erl has one real erl_lint warning (X unused);
%% clean.erl has none; records.hrl is a header, never linted (#356).

all() -> [
    initialize_does_not_advertise_pull_diagnostics,
    project_scan_pushes_diagnostics_for_closed_files,
    closing_a_document_republishes_its_on_disk_diagnostics,
    watched_file_change_republishes_a_closed_file,
    deleted_file_is_cleared,
    excluded_file_is_cleared_on_rescan,
    linting_disabled_clears_published_diagnostics,
    open_document_is_published_once,
    an_open_header_file_is_never_linted,
    an_unsaved_edit_republishes_diagnostics_for_the_new_contents,
    stale_validation_result_is_dropped_once_a_newer_edit_has_landed,
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

%% Every case starts with nothing open, default settings, and no lint left
%% over from the previous case that could land on its socket.
end_per_testcase(_TestCase, Config) ->
    lists:foreach(fun gen_lsp_doc_server:document_closed/1, gen_lsp_doc_server:opened_documents()),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    gen_lsp_config_server:update_config(search, #{}),
    wait_until_idle(100),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% Without diagnosticProvider, vscode-languageclient never pulls: one channel,
%% one collection, no duplicate.
initialize_does_not_advertise_pull_diagnostics(_Config) ->
    #{capabilities := Capabilities} = lsp_handlers:initialize(undefined, #{rootPath => null}),
    ?assertNot(maps:is_key(diagnosticProvider, Capabilities)).

%% Problems for files never opened come from the project scan: each closed
%% module is linted from disk and pushed - a clean one as an explicit empty
%% list - and a header is not linted at all.
project_scan_pushes_diagnostics_for_closed_files(Config) ->
    Raw = configure(#{}),
    ?assertMatch([[#{message := <<"variable 'X' is unused">>}]],
                 publishes_for(Raw, data_file(Config, "with_warning.erl"))),
    ?assertEqual([[]], publishes_for(Raw, data_file(Config, "clean.erl"))),
    ?assertEqual([], publishes_for(Raw, data_file(Config, "records.hrl"))).

%% Closing a document no longer clears it: its unsaved edits are gone, so
%% what it shows from then on is what is on disk.
closing_a_document_republishes_its_on_disk_diagnostics(Config) ->
    File = data_file(Config, "with_warning.erl"),
    {ok, Content} = file:read_file(File),
    Uri = lsp_utils:file_to_file_uri(File),

    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:textDocument_didOpen(ServerSocket, #{textDocument =>
        #{uri => Uri, text => Content, version => 1}}),
    lsp_handlers:textDocument_didChange(ServerSocket, #{
        textDocument => #{uri => Uri, version => 2},
        contentChanges => [#{text => fixed_with_warning()}]}),
    EditRaw = drain_raw(ClientSocket, 5000, <<>>),
    ?assertEqual([[]], publishes_for(EditRaw, File)),

    lsp_handlers:textDocument_didClose(ServerSocket, #{textDocument => #{uri => Uri}}),
    CloseRaw = drain_raw(ClientSocket, 5000, <<>>),
    close_socket_pair(ServerSocket, ClientSocket),
    ?assertMatch([[#{message := <<"variable 'X' is unused">>}]], publishes_for(CloseRaw, File)).

%% A closed file edited outside the editor (another tool, a branch switch) is
%% relinted from disk - reparsed, not linted from the tree held for it.
watched_file_change_republishes_a_closed_file(Config) ->
    File = filename:join(?config(priv_dir, Config), "watched.erl"),
    ok = file:write_file(File, with_warning_module(watched)),
    {ServerSocket, ClientSocket} = open_socket_pair(),

    watched_file_event(ServerSocket, File, 1),
    CreatedRaw = drain_raw(ClientSocket, 5000, <<>>),
    ?assertMatch([[#{message := <<"variable 'X' is unused">>}]], publishes_for(CreatedRaw, File)),

    ok = file:write_file(File, <<"-module(watched).\n-export([go/0]).\n\ngo() -> ok.\n">>),
    watched_file_event(ServerSocket, File, 2),
    ChangedRaw = drain_raw(ClientSocket, 5000, <<>>),
    close_socket_pair(ServerSocket, ClientSocket),
    ?assertEqual([[]], publishes_for(ChangedRaw, File)).

deleted_file_is_cleared(Config) ->
    File = filename:join(?config(priv_dir, Config), "deleted.erl"),
    ok = file:write_file(File, with_warning_module(deleted)),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    watched_file_event(ServerSocket, File, 1),
    CreatedRaw = drain_raw(ClientSocket, 5000, <<>>),
    ?assertMatch([[_]], publishes_for(CreatedRaw, File)),

    ok = file:delete(File),
    watched_file_event(ServerSocket, File, 3),
    DeletedRaw = drain_raw(ClientSocket, 5000, <<>>),
    close_socket_pair(ServerSocket, ClientSocket),
    ?assertEqual([[]], publishes_for(DeletedRaw, File)).

%% With push only, the client keeps whatever it was last sent: a file that
%% leaves the project has to be cleared explicitly.
excluded_file_is_cleared_on_rescan(Config) ->
    File = data_file(Config, "with_warning.erl"),
    ?assertMatch([[_]], publishes_for(configure(#{}), File)),
    Raw = configure(#{exclude => #{'**/with_warning.erl' => true}}),
    ?assertEqual([[]], publishes_for(Raw, File)).

linting_disabled_clears_published_diagnostics(Config) ->
    File = data_file(Config, "with_warning.erl"),
    ?assertMatch([[_]], publishes_for(configure(#{}), File)),
    Raw = configure(#{}, #{linting => false}),
    ?assertEqual([[]], publishes_for(Raw, File)),
    ?assertEqual([], publishes_for(Raw, data_file(Config, "clean.erl"))).

%% The duplicate on hover came from the same file being reported through two
%% channels: opening a file now yields one push, and nothing asks the client
%% to pull.
open_document_is_published_once(Config) ->
    File = data_file(Config, "with_warning.erl"),
    {ok, Content} = file:read_file(File),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:textDocument_didOpen(ServerSocket, #{textDocument =>
        #{uri => lsp_utils:file_to_file_uri(File), text => Content, version => 1}}),
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    close_socket_pair(ServerSocket, ClientSocket),
    ?assertMatch([[#{message := <<"variable 'X' is unused">>}]], publishes_for(Raw, File)),
    ?assertEqual(nomatch, binary:match(Raw, <<"workspace\\/diagnostic\\/refresh">>)).

%% #356: a header is not a module - linting it on its own reported "no module
%% definition" and every record it declares as unused.
an_open_header_file_is_never_linted(Config) ->
    File = data_file(Config, "records.hrl"),
    {ok, Content} = file:read_file(File),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:textDocument_didOpen(ServerSocket, #{textDocument =>
        #{uri => lsp_utils:file_to_file_uri(File), text => Content, version => 1}}),
    Raw = drain_raw(ClientSocket, 3000, <<>>),
    close_socket_pair(ServerSocket, ClientSocket),
    ?assertEqual([], publishes_for(Raw, File)).

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
    lsp_handlers:textDocument_didChange(ServerSocket, #{
        textDocument => #{uri => Uri, version => 2},
        contentChanges => [#{text => fixed_with_warning()}]}),
    ChangeRaw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    ?assertNotEqual(nomatch, binary:match(ChangeRaw, <<"textDocument\\/publishDiagnostics">>)),
    ?assertNotEqual(nomatch, binary:match(ChangeRaw, <<"\"diagnostics\":[]">>)),
    ?assertEqual(nomatch, binary:match(ChangeRaw, <<"variable 'X' is unused">>)),

    gen_lsp_doc_server:document_closed(File).

%% Regression test for the out-of-order validate_file/2 race behind the
%% "quick fix leaves a stale red squiggle" bug: every incoming LSP message
%% but document sync is handled by gen_lsp_server via an independent spawn/1,
%% with no ordering guarantee between them, so a slow
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

    %% Drain raw bytes rather than decoding a single framed message, and
    %% check what actually landed.
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),

    %% The stale diagnostic never made it onto the wire at all.
    ?assertEqual(nomatch, binary:match(Raw, <<"stale">>)),
    %% Exactly the fresh, empty diagnostics list was published.
    ?assertNotEqual(nomatch, binary:match(Raw, <<"\"diagnostics\":[]">>)).

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

data_file(Config, Name) ->
    filename:join(?config(data_dir, Config), Name).

fixed_with_warning() ->
    <<"-module(with_warning).\n-export([go/0]).\n\ngo() ->\n    X = 1,\n    X.\n">>.

with_warning_module(Name) ->
    iolist_to_binary(["-module(", atom_to_list(Name), ").\n-export([go/0]).\n\ngo() ->\n    X = 1,\n    ok.\n"]).

watched_file_event(Socket, File, Type) ->
    lsp_handlers:workspace_didChangeWatchedFiles(Socket, #{changes =>
        [#{uri => lsp_utils:file_to_file_uri(File), type => Type}]}).

%% configuration/2 with the given search section and erlang settings, plus a
%% marker so it is never short-circuited as identical to the previous call;
%% returns everything pushed until the background lint goes quiet.
configure(SearchSection) ->
    configure(SearchSection, #{}).

configure(SearchSection, ErlangSettings) ->
    Erlang = maps:merge(#{verbose => false, linting => true, marker => erlang:unique_integer()},
                        ErlangSettings),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    lsp_handlers:configuration(ServerSocket, [Erlang, #{}, #{}, #{}, SearchSection]),
    Raw = drain_raw(ClientSocket, 5000, <<>>),
    close_socket_pair(ServerSocket, ClientSocket),
    Raw.

close_socket_pair(ServerSocket, ClientSocket) ->
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket).

wait_until_idle(0) ->
    ct:fail(lsp_diagnostics_never_idle);
wait_until_idle(N) ->
    case lsp_diagnostics:idle() of
        true -> ok;
        false -> timer:sleep(100), wait_until_idle(N - 1)
    end.

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
%% alongside a fresh one, or one publish per project file after a scan),
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
