-module(lsp_codeaction_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 2.1: infrastructure only - codeActionProvider/executeCommandProvider
%% capability flags, textDocument/codeAction + codeAction/resolve +
%% workspace/executeCommand dispatch, and lsp_rename:build_workspace_edit/1
%% (a WorkspaceEdit builder factored out of lsp_rename:rename/5 so this
%% module and any future one can reuse it). No quick fix exists yet - that
%% is task 2.2+ - so lsp_codeaction:code_actions/3 always returns [] today
%% and this suite pins exactly that, plus the plumbing around it.
%%
%% The capability flags themselves are pinned in lsp_protocol_SUITE's
%% golden capability map, not duplicated here.

all() -> [
    code_actions_returns_nothing_until_a_fix_is_registered,
    resolve_is_the_identity_function_until_a_fix_is_registered,
    build_workspace_edit_produces_one_document_change_per_edit,
    wire_level_handlers_dispatch_without_crashing
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    File = filename:join(AppDir, "codeaction_source.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    [{source_file, File} | Config].

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% CHARACTERIZATION: no fix is registered yet (task 2.2+), so code_actions/3
%% is unconditionally empty regardless of file, range, or diagnostics
%% context - the client shows no lightbulb at all today.
code_actions_returns_nothing_until_a_fix_is_registered(Config) ->
    File = ?config(source_file, Config),
    Range = #{start => #{line => 0, character => 0}, 'end' => #{line => 5, character => 0}},
    SomeDiagnostic = #{
        severity => 2,
        range => Range,
        message => <<"function unused_helper/0 is unused">>,
        source => <<"erl">>,
        data => #{module => erl_lint, messageBody => [unused_function, [unused_helper, 0]]}
    },
    Context = #{diagnostics => [SomeDiagnostic], triggerKind => 1},
    ?assertEqual([], lsp_codeaction:code_actions(File, Range, Context)).

%% CHARACTERIZATION: with no fix populating a `data` field on any code
%% action, resolve/1 has nothing to act on - it is the identity function.
resolve_is_the_identity_function_until_a_fix_is_registered(_Config) ->
    CodeAction = #{title => <<"placeholder">>, kind => <<"quickfix">>},
    ?assertEqual(CodeAction, lsp_codeaction:resolve(CodeAction)),
    %% identity holds regardless of shape - including one carrying a `data`
    %% field a future fix might have attached before asking for resolution
    WithData = CodeAction#{data => #{anything => <<"at all">>}},
    ?assertEqual(WithData, lsp_codeaction:resolve(WithData)).

%% Direct unit test of the extracted builder (task 2.1 factors this out of
%% lsp_rename:rename/5 - see lsp_rename_SUITE for rename/5's own behavior,
%% unchanged by the refactor).
build_workspace_edit_produces_one_document_change_per_edit(_Config) ->
    Edits = [
        {"/tmp/a.erl", 3, 4, 8, "renamed"},
        {"/tmp/a.erl", 10, 0, 4, "also_here"},
        {"/tmp/b.erl", 1, 2, 3, "elsewhere"}
    ],
    #{documentChanges := Changes} = lsp_rename:build_workspace_edit(Edits),
    %% one group per edit, never merged by file - matches rename/5's own
    %% pinned characterization (task 0.10)
    ?assertEqual(3, length(Changes)),
    ?assert(lists:all(fun (#{edits := E}) -> length(E) =:= 1 end, Changes)),
    Uris = [maps:get(uri, maps:get(textDocument, C)) || C <- Changes],
    ?assertEqual(2, length([U || U <- Uris, binary:match(U, <<"a.erl">>) =/= nomatch])),
    ?assertEqual(1, length([U || U <- Uris, binary:match(U, <<"b.erl">>) =/= nomatch])),
    NewTexts = lists:sort([maps:get(newText, hd(maps:get(edits, C))) || C <- Changes]),
    ?assertEqual([<<"also_here">>, <<"elsewhere">>, <<"renamed">>], NewTexts).

%% textDocument_codeAction/2, codeAction_resolve/2 and
%% workspace_executeCommand/2 are all exported and dispatchable (Socket is
%% unused by every one of them, so `undefined` stands in), and none of them
%% crash on a realistic request shape.
wire_level_handlers_dispatch_without_crashing(Config) ->
    File = ?config(source_file, Config),
    Uri = lsp_utils:file_to_file_uri(File),
    CodeActionParams = #{
        textDocument => #{uri => Uri},
        range => #{start => #{line => 0, character => 0}, 'end' => #{line => 5, character => 0}},
        context => #{diagnostics => [], triggerKind => 1}
    },
    ?assertEqual([], lsp_handlers:textDocument_codeAction(undefined, CodeActionParams)),

    CodeAction = #{title => <<"placeholder">>, kind => <<"quickfix">>},
    ?assertEqual(CodeAction, lsp_handlers:codeAction_resolve(undefined, CodeAction)),

    %% CHARACTERIZATION: no command is registered yet (task 2.1 is
    %% infrastructure only) - the handler is a fixed no-op regardless of
    %% what command/arguments the client asks to execute.
    ?assertEqual(null, lsp_handlers:workspace_executeCommand(undefined, #{command => <<"anything">>, arguments => []})).
