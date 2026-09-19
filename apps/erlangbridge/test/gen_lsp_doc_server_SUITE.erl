-module(gen_lsp_doc_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% This module is the rewrite target of tasks 1.4 (incremental sync) and
%% 4.1 (symbol index), so it gets the strongest net in Phase 0: document
%% lifecycle, cache invalidation, project scan / module->file index, and
%% all three erlang.cacheManagement backends.
%%
%% The cache-mgmt tests run on isolated `peer` nodes (like lsp_protocol_SUITE)
%% because the backend is chosen once, at application start, from a
%% `-vscode_cache_mgmt` command-line argument via a global persistent_term -
%% it cannot be flipped inside the shared CT node without corrupting every
%% other suite's assumption that document caches are plain ets tables.
%% Every other test case here runs in-process, like the rest of Phase 0.

all() -> [
    document_opened_is_readable_via_get_document_contents,
    document_changed_replaces_the_cached_contents,
    document_version_increases_on_open_and_on_each_change,
    document_version_resets_after_close,
    document_range_changed_replaces_text_inside_a_range,
    document_range_changed_inserts_at_a_zero_width_range,
    document_range_changed_deletes_when_replacement_is_empty,
    document_range_changed_spans_multiple_lines,
    document_closed_removes_the_cached_contents,
    get_syntax_tree_lazily_parses_and_caches_on_first_call,
    get_syntax_tree_reparses_the_buffer_after_an_unsaved_change,
    get_syntax_tree_of_an_open_document_never_comes_from_disk,
    project_file_changed_on_an_open_document_is_a_no_op,
    project_file_changed_on_a_closed_document_reparses_it,
    project_file_deleted_clears_every_cache_for_that_file,
    config_change_scans_the_project_into_project_modules,
    get_module_file_resolves_a_scanned_module,
    cache_mgmt_defaults_to_ets_memory_with_no_argument,
    cache_mgmt_memory_mode_uses_plain_ets,
    cache_mgmt_memory_compressed_mode_uses_compressed_ets,
    cache_mgmt_file_mode_uses_dets_in_a_pid_scoped_directory
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

init_per_testcase(TestCase, Config) when
        TestCase =:= cache_mgmt_defaults_to_ets_memory_with_no_argument;
        TestCase =:= cache_mgmt_memory_mode_uses_plain_ets;
        TestCase =:= cache_mgmt_memory_compressed_mode_uses_compressed_ets;
        TestCase =:= cache_mgmt_file_mode_uses_dets_in_a_pid_scoped_directory ->
    Config;
init_per_testcase(_TestCase, Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    File = filename:join(AppDir, "docserver_a.erl"),
    {ok, Content} = file:read_file(File),
    [{file_a, File}, {content_a, Content} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% open / change / close lifecycle %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

document_opened_is_readable_via_get_document_contents(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    ?assertEqual(Content, gen_lsp_doc_server:get_document_contents(File)),
    ?assert(lists:member(File, gen_lsp_doc_server:opened_documents())).

document_changed_replaces_the_cached_contents(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    NewContent = <<"-module(docserver_a).\n-export([go/0]).\n\ngo() -> changed.\n">>,
    gen_lsp_doc_server:document_changed(File, NewContent),
    ?assertEqual(NewContent, gen_lsp_doc_server:get_document_contents(File)).

%% The LSP `didSave` message carries no `version` field (unlike `didOpen`/
%% `didChange`), so lsp_handlers relies on this internal counter rather
%% than the client-supplied one to detect a stale, late-finishing
%% validate_file/2 call - see maybe_send_diagnostics/4.
document_version_increases_on_open_and_on_each_change(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    V1 = gen_lsp_doc_server:get_document_version(File),
    ?assert(V1 > 0),
    gen_lsp_doc_server:document_changed(File, <<Content/binary, "\n">>),
    V2 = gen_lsp_doc_server:get_document_version(File),
    ?assert(V2 > V1),
    gen_lsp_doc_server:document_range_changed(File, {0, 0}, {0, 0}, <<"">>),
    ?assert(gen_lsp_doc_server:get_document_version(File) > V2).

document_version_resets_after_close(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    ?assert(gen_lsp_doc_server:get_document_version(File) > 0),
    gen_lsp_doc_server:document_closed(File),
    ?assertEqual(0, gen_lsp_doc_server:get_document_version(File)).

%% docserver_a.erl content: "-module(docserver_a).\n-export([go/0]).\n\ngo() -> ok.\n"
%% Line 3 (0-based) is "go() -> ok.", where "ok" sits at characters 8-10.

%% task 1.4: textDocumentSync moved from Full (1) to Incremental (2), so
%% document_range_changed/4 is what now keeps the cached buffer in sync
%% with each keystroke instead of a full-document resend.
document_range_changed_replaces_text_inside_a_range(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:document_range_changed(File, {3, 8}, {3, 10}, <<"done">>),
    ?assertEqual(
        <<"-module(docserver_a).\n-export([go/0]).\n\ngo() -> done.\n">>,
        gen_lsp_doc_server:get_document_contents(File)
    ).

%% A zero-width range (start =:= end) is an insertion - typing a single
%% character sends exactly this shape.
document_range_changed_inserts_at_a_zero_width_range(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:document_range_changed(File, {3, 10}, {3, 10}, <<"!">>),
    ?assertEqual(
        <<"-module(docserver_a).\n-export([go/0]).\n\ngo() -> ok!.\n">>,
        gen_lsp_doc_server:get_document_contents(File)
    ).

%% An empty replacement text over a non-zero-width range is a deletion -
%% backspace/delete send this shape.
document_range_changed_deletes_when_replacement_is_empty(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:document_range_changed(File, {3, 8}, {3, 10}, <<>>),
    ?assertEqual(
        <<"-module(docserver_a).\n-export([go/0]).\n\ngo() -> .\n">>,
        gen_lsp_doc_server:get_document_contents(File)
    ).

%% Pressing Enter mid-line sends a zero-width range whose replacement text
%% contains the newline itself - the range can also legitimately span
%% several existing lines (selecting text across lines, then typing).
document_range_changed_spans_multiple_lines(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    %% select from just after "-module(docserver_a)." through just before
    %% "go()" on the last line, replace the whole span with one line
    gen_lsp_doc_server:document_range_changed(File, {0, 22}, {3, 0}, <<"\n">>),
    ?assertEqual(
        <<"-module(docserver_a).\ngo() -> ok.\n">>,
        gen_lsp_doc_server:get_document_contents(File)
    ).

document_closed_removes_the_cached_contents(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:document_closed(File),
    ?assertEqual(undefined, gen_lsp_doc_server:get_document_contents(File)),
    ?assertNot(lists:member(File, gen_lsp_doc_server:opened_documents())).

%%%%%%%%%%%%%%%%%%%%%%%
%% cache invalidation %%
%%%%%%%%%%%%%%%%%%%%%%%

%% get_syntax_tree/1 parses on demand (straight from disk) and caches the
%% result - a second call must not need to reparse.
get_syntax_tree_lazily_parses_and_caches_on_first_call(Config) ->
    File = ?config(file_a, Config),
    Tree1 = gen_lsp_doc_server:get_syntax_tree(File),
    ?assertNotEqual(undefined, Tree1),
    ?assertEqual(Tree1, gen_lsp_doc_server:get_syntax_tree(File)).

%% Regression test for "the quick fix applies but the problem stays in the
%% Problems list": a textDocument/didChange updates the buffer and bumps
%% document_version but does not itself reparse, so a cached tree from before
%% the edit must be recognised as stale here - otherwise every tree consumer
%% (the linter first of all) keeps answering from the pre-edit source and
%% reports problems the edit already fixed. No parse_document/1 call in
%% between: that is precisely what didChange does not do.
get_syntax_tree_reparses_the_buffer_after_an_unsaved_change(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File),
    TreeBefore = gen_lsp_doc_server:get_syntax_tree(File),
    ?assertNotEqual(undefined, TreeBefore),

    Fixed = <<"-module(docserver_a).\n-export([go/0]).\n\ngo() -> reparsed.\n">>,
    gen_lsp_doc_server:document_changed(File, Fixed),
    TreeAfter = gen_lsp_doc_server:get_syntax_tree(File),

    ?assertNotEqual(TreeBefore, TreeAfter),
    ?assert(lists:member(reparsed, atoms_in(TreeAfter))),
    ?assertNot(lists:member(reparsed, atoms_in(TreeBefore))),
    %% and the fresh tree is now the cached one, not reparsed on every call
    ?assertEqual(TreeAfter, gen_lsp_doc_server:get_syntax_tree(File)).

%% An open document's buffer can hold changes the file on disk does not, so
%% a reparse triggered while it is open must read the buffer. Otherwise a
%% background project parse or a file-watcher event lands a disk tree stamped
%% with the buffer's version - stale contents passed off as current.
get_syntax_tree_of_an_open_document_never_comes_from_disk(Config) ->
    File = ?config(file_a, Config),
    Buffer = <<"-module(docserver_a).\n-export([go/0]).\n\ngo() -> only_in_the_buffer.\n">>,
    gen_lsp_doc_server:document_opened(File, Buffer),
    %% no parse_document/1: the lazy path in get_syntax_tree/1 is what has to
    %% pick the buffer over the file
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    ?assert(lists:member(only_in_the_buffer, atoms_in(Tree))),
    %% a disk reparse while the document is open leaves the buffer's tree in place
    gen_lsp_doc_server:project_file_changed(File),
    sys:get_state(gen_lsp_doc_server),
    ?assert(lists:member(only_in_the_buffer, atoms_in(gen_lsp_doc_server:get_syntax_tree(File)))).

%% CHARACTERIZATION: handle_cast({project_file_changed, File}, State) skips
%% reparsing entirely whenever the file already has an open buffer
%% (?XETS:lookup(document_contents, File) succeeds) - on the theory that an
%% open document's own didChange/didSave notifications already keep it in
%% sync. So a project_file_changed notification for a currently-open file
%% is a pure no-op: the cached syntax tree is left exactly as it was, even
%% though the file legitimately changed on disk.
project_file_changed_on_an_open_document_is_a_no_op(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File),
    TreeBefore = gen_lsp_doc_server:get_syntax_tree(File),
    gen_lsp_doc_server:project_file_changed(File),
    %% the cast is async; give it a moment then confirm nothing changed
    sys:get_state(gen_lsp_doc_server),
    ?assertEqual(TreeBefore, gen_lsp_doc_server:get_syntax_tree(File)).

%% Without an open buffer, project_file_changed queues the file and a
%% background worker reparses it straight from disk.
project_file_changed_on_a_closed_document_reparses_it(Config) ->
    File = ?config(file_a, Config),
    %% undo any residual "open" state a previous test case in this suite left behind
    gen_lsp_doc_server:document_closed(File),
    ?assertEqual(undefined, gen_lsp_doc_server:get_document_contents(File)),
    gen_lsp_doc_server:project_file_changed(File),
    Tree = wait_for_syntax_tree(File, 50),
    ?assertNotEqual(undefined, Tree).

%% project_file_deleted removes the file from every cache: contents,
%% syntax tree, dodged syntax tree, references, and inlay hints.
project_file_deleted_clears_every_cache_for_that_file(Config) ->
    File = ?config(file_a, Config),
    Content = ?config(content_a, Config),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File),
    ?assertNotEqual(undefined, gen_lsp_doc_server:get_syntax_tree(File)),
    gen_lsp_doc_server:project_file_deleted(File),
    sys:get_state(gen_lsp_doc_server),
    ?assertEqual(undefined, gen_lsp_doc_server:get_document_contents(File)),
    ?assertEqual(0, gen_lsp_doc_server:get_document_version(File)),
    ?assertEqual([], ets:lookup(syntax_tree, File)),
    ?assertEqual([], ets:lookup(dodged_syntax_tree, File)),
    ?assertEqual([], ets:lookup(references, File)),
    ?assertEqual([], ets:lookup(document_inlayhints, File)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% project scan / module->file index %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CHARACTERIZATION: project_modules/0 returns module names as *strings*
%% (do_add_project_file/3 keys its map with filename:rootname(basename(File)),
%% which is a string, never converted to an atom), unlike get_module_file/1's
%% own Module parameter, which is expected to be an atom.
config_change_scans_the_project_into_project_modules(Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    Modules = gen_lsp_doc_server:project_modules(),
    ?assert(lists:member("docserver_a", Modules)),
    ?assert(lists:member("docserver_b", Modules)),
    ?assert(lists:all(fun is_list/1, Modules)),
    AppDir =/= undefined.

get_module_file_resolves_a_scanned_module(Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    ExpectedFile = filename:join(AppDir, "docserver_b.erl"),
    ?assertEqual(ExpectedFile, gen_lsp_doc_server:get_module_file(docserver_b)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% erlang.cacheManagement backends %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% No -vscode_cache_mgmt argument at all falls back to plain ets (the same
%% branch "memory" takes) - do_persist_cache_mgmt_opts/1's final catch-all.
cache_mgmt_defaults_to_ets_memory_with_no_argument(_Config) ->
    {Mod, Opts, DetsInfo} = cache_mgmt_probe([]),
    ?assertEqual({ets, []}, {Mod, Opts}),
    ?assertEqual(undefined, DetsInfo).

cache_mgmt_memory_mode_uses_plain_ets(_Config) ->
    {Mod, Opts, DetsInfo} = cache_mgmt_probe(["-vscode_cache_mgmt", "memory"]),
    ?assertEqual({ets, []}, {Mod, Opts}),
    ?assertEqual(undefined, DetsInfo).

cache_mgmt_memory_compressed_mode_uses_compressed_ets(_Config) ->
    {Mod, Opts, DetsInfo} = cache_mgmt_probe(["-vscode_cache_mgmt", "memory", "compressed"]),
    ?assertEqual({ets, [compressed]}, {Mod, Opts}),
    ?assertEqual(undefined, DetsInfo).

%% "file" mode backs the same four caches with dets, in a directory scoped
%% by both the (client-supplied) username and this node's own OS pid - so
%% two extension instances for the same user never share a cache directory.
cache_mgmt_file_mode_uses_dets_in_a_pid_scoped_directory(_Config) ->
    {Mod, Opts, DetsInfo} = cache_mgmt_probe(["-vscode_cache_mgmt", "file", "testuser", "/tmp"]),
    ?assertEqual({dets, []}, {Mod, Opts}),
    ?assertNotEqual(undefined, DetsInfo),
    ?assert(string:str(DetsInfo, "/tmp/vscode_erlang_testuser/cache/") =:= 1).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

%% Every atom literal in a syntax tree, so a test can assert on what the
%% parsed source actually said without pinning the AST's exact shape.
atoms_in(Term) when is_tuple(Term) ->
    atoms_in(tuple_to_list(Term));
atoms_in(Term) when is_list(Term) ->
    lists:flatmap(fun atoms_in/1, Term);
atoms_in(Term) when is_atom(Term) ->
    [Term];
atoms_in(_Term) ->
    [].

wait_for_syntax_tree(_File, 0) ->
    undefined;
wait_for_syntax_tree(File, N) ->
    case ets:lookup(syntax_tree, File) of
        %% cached trees carry the document_version they were parsed at
        [{File, _Version, Tree}] -> Tree;
        [] ->
            timer:sleep(100),
            wait_for_syntax_tree(File, N - 1)
    end.

cache_mgmt_probe(ExtraArgs) ->
    {ok, Peer, _Node} = peer:start_link(#{
        name => peer:random_name(?MODULE),
        args => ["-pa" | code:get_path()] ++ ExtraArgs,
        connection => standard_io
    }),
    try
        ok = peer:call(Peer, application, start, [vscode_lsp, permanent]),
        Mod = peer:call(Peer, persistent_term, get, [large_cache_module]),
        Opts = peer:call(Peer, persistent_term, get, [large_cache_create_opts]),
        DetsInfo = peer:call(Peer, dets, info, [document_contents, filename]),
        {Mod, Opts, DetsInfo}
    after
        catch peer:stop(Peer)
    end.
