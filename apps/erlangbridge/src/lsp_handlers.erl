-module(lsp_handlers).

-export([initialize/2, initialized/2, shutdown/2, exit/2, cancelRequest/2, setTrace/2, configuration/2,
    workspace_didChangeConfiguration/2, workspace_didChangeWatchedFiles/2, workspace_didChangeWorkspaceFolders/2,
    textDocument_didOpen/2, textDocument_didClose/2, textDocument_didSave/2, textDocument_didChange/2,
    textDocument_definition/2, textDocument_references/2, textDocument_hover/2, textDocument_completion/2,
    textDocument_formatting/2, textDocument_codeLens/2, textDocument_documentSymbol/2,
    textDocument_signatureHelp/2, textDocument_prepareRename/2, textDocument_rename/2]).
-export([textDocument_inlineValues/2, textDocument_inlineValue/2]).
-export([textDocument_inlayHints/2, textDocument_inlayHint/2]).
-export([textDocument_codeAction/2, codeAction_resolve/2, workspace_executeCommand/2]).
-export([textDocument_semanticTokens_full/2, textDocument_semanticTokens_full_delta/2,
    textDocument_semanticTokens_range/2]).
-export([textDocument_declaration/2, textDocument_typeDefinition/2, textDocument_implementation/2,
    textDocument_documentHighlight/2, workspace_symbol/2, workspaceSymbol_resolve/2]).
-export([textDocument_prepareCallHierarchy/2, callHierarchy_incomingCalls/2, callHierarchy_outgoingCalls/2]).
-export([textDocument_prepareTypeHierarchy/2, typeHierarchy_supertypes/2, typeHierarchy_subtypes/2]).
-export([textDocument_rangeFormatting/2, textDocument_onTypeFormatting/2]).
-export([textDocument_foldingRange/2, textDocument_selectionRange/2]).
-export([completionItem_resolve/2, textDocument_documentLink/2, documentLink_resolve/2]).
-export([codeLens_resolve/2, inlayHint_resolve/2]).
-export([erlang_discoverTests/2, erlang_runTests/2]).
%% Exported for lsp_diagnostic_SUITE, which drives the stale-version drop
%% directly - it is unreachable through a handler.
-export([maybe_send_diagnostics/4]).
%% Exported for lsp_diagnostics, which debounces workspace/didChangeConfiguration
%% notifications into a single call, textDocument/didChange edits into a
%% single validation per file, lints closed project files in the background and
%% clears files that should no longer show anything.
-export([request_configuration/2, validate_file/2, validate_closed_file/3, send_diagnostics/3]).

-include("lsp_log.hrl").

initialize(_Socket, Params) ->
    RootPath = resolve_root(Params),
    gen_lsp_config_server:update_config(root, RootPath),
    gen_lsp_doc_server:root_available(),
    #{capabilities => #{
        textDocumentSync => 2, % Incremental
        completionProvider => #{triggerCharacters => <<":#.?-">>, resolveProvider => true}, %% task 5.6
        hoverProvider => true,
        signatureHelpProvider => #{triggerCharacters => <<"(,">>, retriggerCharacters => <<",">>},
        declarationProvider => true, %% task 4.2
        definitionProvider => true,
        typeDefinitionProvider => true, %% task 4.3
        implementationProvider => true, %% task 4.4
        referencesProvider => true,
        documentHighlightProvider => true, %% task 4.7
        documentSymbolProvider => true,
        codeActionProvider => #{
            codeActionKinds => [<<"quickfix">>, <<"source">>, <<"refactor">>],
            resolveProvider => true
        },
        codeLensProvider => #{resolveProvider => true}, %% task 5.10
        documentLinkProvider => #{resolveProvider => false}, %% task 5.7
        colorProvider => false,
        documentFormattingProvider => true,
        documentRangeFormattingProvider => true, %% task 5.3
        documentOnTypeFormattingProvider => #{ %% task 5.4
            firstTriggerCharacter => <<".">>,
            moreTriggerCharacter => [<<";">>, <<",">>, <<"\n">>]
        },
        renameProvider => #{ prepareProvider => true },
        foldingRangeProvider => true, %% task 5.1
        %% CHARACTERIZATION / known limitation: no command is registered yet
        %% (task 2.1 is infrastructure only) - this list grows as task 2.2+
        %% adds fixes that need workspace/executeCommand rather than a plain
        %% WorkspaceEdit.
        executeCommandProvider => #{commands => []},
        selectionRangeProvider => true, %% task 5.2
        linkedEditingRangeProvider => false,
        callHierarchyProvider => true, %% task 4.5
        semanticTokensProvider => #{
            legend => lsp_semantic_tokens:legend(),
            full => #{delta => true}, %% task 3.2
            range => true %% task 3.2
        },
        monikerProvider => false,
        typeHierarchyProvider => true, %% task 4.6
        inlineValueProvider => true,
        inlayHintProvider => #{resolveProvider => true}, %% task 5.8
        %% No diagnosticProvider: diagnostics are pushed only
        %% (textDocument/publishDiagnostics), closed project files included -
        %% see lsp_diagnostics. Advertising the pull model as well made
        %% vscode-languageclient keep a second collection for the same files,
        %% so every problem showed twice on hover and in quick fixes.
        workspaceSymbolProvider => #{resolveProvider => true}, %% task 4.1
        workspace => #{
            workspaceFolders => #{supported => true, changeNotifications => true}
        }
    },
    %% task 7.3: the client status bar shows which OTP the bridge runs on.
    %% The bridge app's own vsn is a meaningless constant, so `version` is
    %% the runtime's full OTP version instead.
    serverInfo => #{name => <<"vscode_erlang">>, version => otp_version()}}.

%% @doc Full OTP version ("26.2.5"), falling back to the major release
%% ("26") when the OTP_VERSION file is missing (stripped releases).
otp_version() ->
    Release = erlang:system_info(otp_release),
    File = filename:join([code:root_dir(), "releases", Release, "OTP_VERSION"]),
    case file:read_file(File) of
        {ok, Bin} -> string:trim(Bin);
        {error, _} -> list_to_binary(Release)
    end.

%% @doc Resolve the initial workspace root, preferring the modern,
%% possibly-multi-folder `workspaceFolders` field over the single-folder
%% `rootUri`, itself preferred over the deprecated `rootPath`.
%%
%% CHARACTERIZATION / known limitation: gen_lsp_config_server only ever
%% stores a single root path, so with several workspace folders only the
%% first one is used - true multi-root project scanning (folding over
%% every folder in gen_lsp_doc_server's scan) is a separate follow-up task,
%% not yet implemented here.
resolve_root(Params) ->
    case maps:get(workspaceFolders, Params, null) of
        [#{uri := Uri} | _] ->
            lsp_utils:to_string(lsp_utils:file_uri_to_file(Uri));
        _ ->
            case maps:get(rootUri, Params, null) of
                null -> resolve_root_path(Params);
                RootUri -> lsp_utils:to_string(lsp_utils:file_uri_to_file(RootUri))
            end
    end.

resolve_root_path(Params) ->
    case maps:get(rootPath, Params, null) of
        null -> "";
        RootPath -> lsp_utils:to_string(RootPath)
    end.

initialized(Socket, _Params) ->
    request_configuration(Socket, <<"initialized">>).

shutdown(_Socket, _) ->
    init:stop().

exit(_Socket, _) ->
    init:stop().

cancelRequest(_Socket, _Params) ->
    ok.

setTrace(_Socket, _Params) ->
    ok.

configuration(Socket, [ErlangSection, FilesSection, ComputedSection, HttpSection, SearchSection] = Sections) ->
    Documents = gen_lsp_doc_server:opened_documents(),
    %% Captured before the updates below: a workspace/configuration response
    %% carrying exactly the settings we already hold means nothing to rescan or
    %% revalidate, and the full project scan + a validation pass per open
    %% document is far too expensive to repeat for nothing.
    PreviousSections = [gen_lsp_config_server:get_section(Key) ||
                        Key <- [erlang, files, computed, http, search]],
    gen_lsp_config_server:update_config(erlang, ErlangSection),
    %% because 'verbose' is stored in erlang section, loggin should be after update erlang config
    ?LOG(<<"diag">>, "configuration: Opened documents ~p", [Documents]),
    gen_lsp_config_server:update_config(files, FilesSection),
    gen_lsp_config_server:update_config(computed, ComputedSection),
    gen_lsp_config_server:update_config(http, HttpSection),
    gen_lsp_config_server:update_config(search, SearchSection),
    ?LOG(<<"config">>, "vscode configuration:~n"
                           " - erlang: ~p~n"
                           " - files: ~p~n"
                           " - computed: ~p~n"
                           " - http: ~p~n"
                           " - search: ~p",
                           [ErlangSection, FilesSection, ComputedSection,
                            HttpSection, SearchSection]),

    %% Delete old caches left there by brutally killed extension instances
    case ComputedSection of
        #{tmpdir := TmpDir, username := UserName} ->
            gen_lsp_doc_server:delete_unused_caches(TmpDir, UserName);
        _ ->
            ok
    end,

    case PreviousSections =:= Sections of
        true ->
            ?LOG(<<"diag">>, "configuration: unchanged, skipping project scan and validation of ~p", [Documents]);
        false ->
            %% Scan workspace for source files, then lint the closed ones in
            %% the background (or clear everything if linting is now off).
            rescan_project(Socket),
            ?LOG(<<"diag">>, "configuration: Starting validation for documents ~p", [Documents]),
            %% NOTE: no send_diagnostics(Socket, File, []) clear ahead of
            %% validate_file here - see the "Why validate_file alone is enough"
            %% note above configuration/2. That clear used to be unguarded
            %% (unlike every other diagnostic publish, which goes through
            %% maybe_send_diagnostics/4's version check), so a lagging
            %% configuration/2 call - and more than one can be in flight at
            %% once, see workspace_didChangeConfiguration/2 - could blank a
            %% file's diagnostics after a genuinely newer edit had already
            %% published the correct ones, with nothing to catch it.
            %% validate_file publishes the file's actual current state on its
            %% own, version-guarded.
            lists:foreach(fun (File) ->
                ?LOG(<<"diag">>, "configuration: Starting validation for file ~p", [File]),
                validate_file(Socket, File)
            end, Documents)
    end.

%% @doc VS Code's Workspace.onDidChangeConfiguration can fire more than once
%% for a single logical settings change (once per settings scope that
%% resolves), each independently sending this notification - without
%% debouncing, each launches its own full workspace/configuration round-trip
%% plus (for however many the identical-sections check in configuration/2
%% doesn't catch as a true duplicate) a full project rescan and per-document
%% revalidation, all running concurrently. lsp_diagnostics collapses a burst
%% into a single request_configuration/2 call.
workspace_didChangeConfiguration(Socket, _Params) ->
    lsp_diagnostics:schedule_configuration_request(Socket, <<"didChangeConfiguration">>).

%% A closed file changed on disk is relinted from disk (a deleted one is
%% cleared). An open one is left alone: its buffer, not the disk, is what its
%% diagnostics describe. A header is not a project module: only the modules
%% that include it are relinted.
workspace_didChangeWatchedFiles(Socket, Params) ->
    lists:foreach(fun (#{uri := Uri, type := Type} = Change) ->
        ?LOG(<<"diag">>, "watchedFiles: ~p", [Change]),
        File = lsp_utils:file_uri_to_file(Uri),
        case {filename:extension(File), Type} of
            {".hrl", _} ->
                revalidate_includers(Socket, File);
            {_, 1} -> % Created
                gen_lsp_doc_server:project_file_added(File),
                revalidate_closed_file(Socket, File);
            {_, 2} -> % Changed
                gen_lsp_doc_server:project_file_changed(File),
                revalidate_closed_file(Socket, File);
            {_, 3} -> % Deleted
                gen_lsp_doc_server:project_file_deleted(File),
                revalidate_closed_file(Socket, File)
        end
    end, maps:get(changes, Params)).

%% @doc Relint every module whose last parse included Hrl: their syntax
%% trees hold the header as it was then, and nothing about the module itself
%% changed to make get_syntax_tree/1 reparse it.
revalidate_includers(Socket, Hrl) ->
    lists:foreach(fun (File) ->
        case gen_lsp_doc_server:get_document_contents(File) of
            undefined -> lsp_diagnostics:schedule_disk_validation(Socket, File);
            _Open -> lsp_diagnostics:schedule_validation(Socket, File, reparse)
        end
    end, gen_lsp_doc_server:get_includers(Hrl)).

revalidate_closed_file(Socket, File) ->
    case {filename:extension(File), gen_lsp_doc_server:get_document_contents(File)} of
        {".erl", undefined} -> lsp_diagnostics:schedule_disk_validation(Socket, File);
        _ -> ok
    end.

%% @doc Rescan the project, then push diagnostics for every closed project
%% file - or, with erlang.linting off, clear every file showing any.
%%
%% The scan runs in gen_lsp_doc_server, from a cast that does not know the
%% socket. all_project_files/0 is a call to that same server, so it is only
%% answered once the config_change cast sent just before has been handled,
%% i.e. once the scan is done: the socket stays here, with the handler that
%% has it, and the scan needs no knowledge of diagnostics.
rescan_project(Socket) ->
    gen_lsp_doc_server:config_change(),
    case gen_lsp_config_server:linting() of
        true ->
            %% Only modules are linted (#356).
            Files = [File || File <- gen_lsp_doc_server:all_project_files(),
                             filename:extension(File) =:= ".erl"],
            lsp_diagnostics:project_scanned(Socket, Files);
        _ ->
            lsp_diagnostics:clear_all(Socket)
    end.

%% @doc Only handles the case where the server started with no root at all
%% (single-file mode) and a folder is then added to the workspace: that
%% first added folder is adopted as the root and scanned.
%%
%% CHARACTERIZATION / known limitation: once a root is already set, further
%% additions or removals are not reflected - see resolve_root/1's comment
%% and task 1.3's scope note. Real multi-root support is a separate,
%% follow-up task.
workspace_didChangeWorkspaceFolders(Socket, Params) ->
    #{event := #{added := Added}} = Params,
    case {gen_lsp_config_server:root(), Added} of
        {"", [#{uri := Uri} | _]} ->
            NewRoot = lsp_utils:to_string(lsp_utils:file_uri_to_file(Uri)),
            gen_lsp_config_server:update_config(root, NewRoot),
            gen_lsp_doc_server:root_available(),
            rescan_project(Socket);
        _ ->
            ok
    end.

%% didOpen, didChange, didClose and didSave run in gen_lsp_server's socket
%% process, in wire order (see gen_lsp_server:dispatch/2), so they only update
%% the buffer and hand the lint to lsp_diagnostics. No parse here either:
%% gen_lsp_doc_server:get_syntax_tree/1 parses the buffer on demand.
textDocument_didOpen(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    ?LOG(<<"diag">>, "didOpen: ~p version=~p", [File, mapmapfind(textDocument, version, Params, fun () -> undefined end)]),
    gen_lsp_doc_server:document_opened(File, mapmapget(textDocument, text, Params)),
    %% Not gated on autosave() any more - see textDocument_didChange/2. With
    %% autosave off, the gate left a freshly opened document with no
    %% diagnostics at all until its first edit.
    lsp_diagnostics:schedule_validation(Socket, File).

%% Closing a document does not clear its diagnostics: a closed project file
%% keeps showing its problems, computed from disk - which is what it holds
%% once unsaved edits are discarded. Other files (e.g. rebar.config) are only
%% linted while open, so closing one clears it.
textDocument_didClose(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    ?LOG(<<"diag">>, "didClose: ~p", [File]),
    lsp_diagnostics:cancel_validation(File),
    gen_lsp_doc_server:document_closed(File),
    case filename:extension(File) of
        ".erl" -> lsp_diagnostics:schedule_disk_validation(Socket, File);
        _ -> send_diagnostics(Socket, File, [])
    end.

textDocument_didSave(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    ?LOG(<<"diag">>, "didSave: ~p", [File]),
    case filename:extension(File) of
        %% epp reads headers from disk, so saving is what changes them for
        %% the modules including them.
        ".hrl" -> revalidate_includers(Socket, File);
        %% Not gated on autosave() any more - see textDocument_didChange/2.
        _ -> lsp_diagnostics:schedule_validation(Socket, File)
    end.

%% Content changes are applied in the order the client sent them - each
%% one (range-based or, still legal even under Incremental sync, a full
%% replacement with no range) is resolved against the buffer state left by
%% the previous one.
textDocument_didChange(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    ContentChanges = maps:get(contentChanges, Params),
    ?LOG(<<"diag">>, "didChange: ~p version=~p changes=~p",
        [File, mapmapfind(textDocument, version, Params, fun () -> undefined end), length(ContentChanges)]),
    lists:foreach(fun (ContentChange) -> apply_content_change(File, ContentChange) end, ContentChanges),
    %% Not gated on autosave() any more: skipping revalidation here would
    %% leave a problem listed until the next save - which is what made an
    %% applied quick fix look like it had done nothing. No parse_document/1 call:
    %% gen_lsp_doc_server:get_syntax_tree/1 now reparses the buffer by itself
    %% when the cached tree predates this edit, so only the files a request
    %% actually touches get reparsed.
    lsp_diagnostics:schedule_validation(Socket, File).

apply_content_change(File, #{range := #{start := #{line := SL, character := SC},
                                         'end' := #{line := EL, character := EC}},
                              text := NewText}) ->
    gen_lsp_doc_server:document_range_changed(File, {SL, SC}, {EL, EC}, NewText);
apply_content_change(File, #{text := NewText}) ->
    gen_lsp_doc_server:document_changed(File, NewText).

textDocument_definition(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Locations = lsp_navigation:definition(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1),
    [#{uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
       range => lsp_utils:client_range(L, S, E)
     }
     || {File, L, S, E}<-Locations].

textDocument_references(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    lists:map(fun ({File, L, S, E}) ->
        #{
            uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
            range => lsp_utils:client_range(L, S, E)
        }
    end, lsp_navigation:references(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1)).

%% Erlang has no separate declaration/definition distinction (unlike e.g.
%% a C header vs its .c file) - task 4.2 is exactly this alias.
textDocument_declaration(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Locations = lsp_navigation:definition(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1),
    [#{uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
       range => lsp_utils:client_range(L, S, E)
     }
     || {File, L, S, E} <- Locations].

textDocument_typeDefinition(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Locations = lsp_navigation:type_definition(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1),
    [#{uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
       range => lsp_utils:client_range(L, S, E)
     }
     || {File, L, S, E} <- Locations].

textDocument_implementation(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Locations = lsp_navigation:implementation(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1),
    [#{uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
       range => lsp_utils:client_range(L, S, E)
     }
     || {File, L, S, E} <- Locations].

textDocument_documentHighlight(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Highlights = lsp_navigation:document_highlights(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1),
    [#{range => lsp_utils:client_range(L, S, E), kind => Kind} || {Kind, L, S, E} <- Highlights].

workspace_symbol(_Socket, Params) ->
    Query = maps:get(query, Params, <<>>),
    lsp_workspace_symbol:symbols(Query).

workspaceSymbol_resolve(_Socket, Symbol) ->
    lsp_workspace_symbol:resolve(Symbol).

textDocument_prepareCallHierarchy(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    lsp_hierarchy:prepare_call_hierarchy(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1).

callHierarchy_incomingCalls(_Socket, Params) ->
    Item = maps:get(item, Params),
    lsp_hierarchy:incoming_calls(Item).

callHierarchy_outgoingCalls(_Socket, Params) ->
    Item = maps:get(item, Params),
    lsp_hierarchy:outgoing_calls(Item).

textDocument_prepareTypeHierarchy(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    lsp_hierarchy:prepare_type_hierarchy(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1).

typeHierarchy_supertypes(_Socket, Params) ->
    Item = maps:get(item, Params),
    lsp_hierarchy:supertypes(Item).

typeHierarchy_subtypes(_Socket, Params) ->
    Item = maps:get(item, Params),
    lsp_hierarchy:subtypes(Item).

textDocument_hover(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    case lsp_navigation:hover_info(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1) of
        undefined -> #{contents => <<>>};
        Contents -> #{contents => Contents}
    end.

textDocument_completion(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    {TextBefore,_} = text_before_character(File, Line, Character),
    auto_complete(File, Line + 1, TextBefore).

text_before_character(File, Line, Character) ->
    Contents = gen_lsp_doc_server:get_document_contents(File),
    LineText = lists:nth(Line + 1, binary:split(Contents, <<"\n">>, [global])),
    {binary:part(LineText, 0, min(Character + 1, byte_size(LineText))), LineText}.

%% @doc task 5.5: erlang.formatterEnabled (default true) gates all three
%% formatting entry points below - like codeLensEnabled/inlayHintsEnabled/
%% semanticTokensEnabled, the capability itself stays permanently
%% advertised (VS Code has no easy way to flip that dynamically) and each
%% handler just returns no edits when disabled.
textDocument_formatting(_Socket, Params) ->
    case gen_lsp_config_server:formatterEnabled() of
        false ->
            [];
        true ->
            File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
            Contents = case gen_lsp_doc_server:get_document_contents(File) of
                undefined ->
                    {ok, FileContents} = file:read_file(File),
                    FileContents;
                StoredContents ->
                    StoredContents
            end,
            UpdatedContents = formatting(Contents),
            %% task 5.3: the result range used to be a hardcoded
            %% {0,0}-{999999,255} sentinel, regardless of the document's
            %% real length - now the actual end position.
            {EndLine, EndCol} = lsp_formatting:document_end(Contents),
            [#{range => lsp_utils:client_range(1, 1, EndLine, EndCol), newText => UpdatedContents}]
    end.

textDocument_rangeFormatting(_Socket, Params) ->
    case gen_lsp_config_server:formatterEnabled() of
        false ->
            [];
        true ->
            File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
            #{line := LS} = mapmapget(range, start, Params),
            #{line := LE} = mapmapget(range, 'end', Params),
            lsp_formatting:range(File, LS + 1, LE + 1)
    end.

textDocument_onTypeFormatting(_Socket, Params) ->
    case gen_lsp_config_server:formatterEnabled() of
        false ->
            [];
        true ->
            File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
            Line = mapmapget(position, line, Params),
            lsp_formatting:on_type(File, Line + 1)
    end.

%% @doc task 5.10: list only ever does the cheap half
%% (lsp_navigation:codelens_positions/1 - position and exported-ness, no
%% reference count) and leaves `command` out entirely; codeLens/resolve
%% below computes the (project-wide-search-requiring) reference count for
%% just the one lens actually about to be shown, not eagerly for every
%% function in the file the moment it opens.
%%
%% CHARACTERIZATION: the old eager version could show an exported,
%% referenced function *two* lenses ("exported" and "N references" side
%% by side), because it already knew the count before deciding how many
%% lenses to emit. list can no longer make that decision (it doesn't have
%% the count yet, by design), so there is always exactly one lens per
%% function now; for an exported function with real references, resolve
%% combines both pieces of information into that one lens's own title
%% ("exported, N references"), still clickable (still a
%% findReferences command), rather than dropping either one.
textDocument_codeLens(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    case gen_lsp_config_server:codeLensEnabled() of
        false ->
            [];
        _ ->
            File = lsp_utils:file_uri_to_file(Uri),
            [#{
                range => lsp_utils:client_range(Line, Column, Column + length(atom_to_list(Function))),
                data => #{
                    file => unicode:characters_to_binary(File),
                    function => Function,
                    arity => Arity,
                    exported => Exported,
                    line => Line,
                    column => Column
                }
             } || {Function, Arity, Exported, Line, Column} <- lsp_navigation:codelens_positions(File)]
    end.

codeLens_resolve(_Socket, #{data := Data} = Lens) ->
    #{file := FileBin, function := FunctionData, arity := Arity, exported := Exported, line := Line, column := Column} = Data,
    File = binary_to_list(FileBin),
    Function = to_atom(FunctionData),
    RefCount = lsp_navigation:codelens_ref_count(File, Function, Arity),
    Lens#{command => codelens_command(Exported, RefCount, File, Line, Column)}.

codelens_command(true, 0, _File, _Line, _Column) ->
    #{title => <<"exported">>, command => <<>>};
codelens_command(true, RefCount, File, Line, Column) ->
    #{
        title => iolist_to_binary(io_lib:format("exported, ~p references", [RefCount])),
        command => <<"editor.action.findReferences">>,
        arguments => [lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)), #{lineNumber => Line, column => Column}]
    };
codelens_command(false, 0, _File, _Line, _Column) ->
    #{title => <<"unused">>, command => <<>>};
codelens_command(false, RefCount, File, Line, Column) ->
    #{
        title => list_to_binary(integer_to_list(RefCount) ++ " references"),
        command => <<"editor.action.findReferences">>,
        arguments => [lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)), #{lineNumber => Line, column => Column}]
    }.

to_atom(Value) when is_atom(Value) -> Value;
to_atom(Value) when is_binary(Value) -> binary_to_atom(Value, utf8).


textDocument_inlayHint(_Socket, Params) ->
    textDocument_inlayHints(_Socket, Params).

%% @doc task 5.8: `kind` used to be sent as the literal string "parameter"/
%% "type" - InlayHintKind is actually a number (1 Type, 2 Parameter) per
%% spec, so this was previously sending a value no real client field
%% expects (harmless in practice - VS Code just never applies the kind-
%% specific styling - but not what the protocol says). Also adds
%% paddingRight/paddingLeft and, for a parameter hint, a tooltip and a
%% textEdit that materializes the inferred name into real source text if
%% the hint is double-clicked.
%%
%% CHARACTERIZATION / known limitations *not* addressed by this pass (see
%% tasks.md 5.8's own list): inlayhints_info/3 still only ever looks at
%% local (same-file) calls, never a -spec's own parameter names for a
%% call into another module; and there is still no type-hint mode at all
%% for -spec return types ("type" is a real, reachable kind value below,
%% but lsp_inlayhints.erl never actually produces one today).
textDocument_inlayHints(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    case gen_lsp_config_server:inlayHintsEnabled() of
        false -> [];
        _ ->
            #{line:=LS, character:=CS} = mapmapget(range, start, Params),
            #{line:=LE, character:=CE} = mapmapget(range, 'end', Params),
            [inlay_hint_item(Position, Label, Kind)
             || {Position, Label, Kind} <- lsp_navigation:inlayhints_info(lsp_utils:file_uri_to_file(Uri), {LS,CS}, {LE,CE})]
    end.

textDocument_foldingRange(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    [folding_range_item(StartLine, EndLine, Kind) || {StartLine, EndLine, Kind} <- lsp_folding:folding_ranges(File)].

folding_range_item(StartLine, EndLine, Kind) ->
    Base = #{startLine => StartLine - 1, endLine => EndLine - 1},
    case Kind of
        undefined -> Base;
        _ -> Base#{kind => Kind}
    end.

%% Params has a `positions` array (multi-cursor support) - one
%% SelectionRange chain per position, same order, each linked
%% innermost-to-outermost via its own `parent` field.
textDocument_selectionRange(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    Positions = maps:get(positions, Params),
    [selection_range_chain(lsp_navigation:selection_range(File, maps:get(line, P) + 1, maps:get(character, P) + 1))
     || P <- Positions].

selection_range_chain([{StartLine, EndLine}]) ->
    #{range => full_lines_range(StartLine, EndLine)};
selection_range_chain([{StartLine, EndLine} | Rest]) ->
    #{range => full_lines_range(StartLine, EndLine), parent => selection_range_chain(Rest)}.

%% Whole lines StartLine..EndLine inclusive - see
%% lsp_navigation:selection_range/3's own note on why this stays
%% line-granular rather than column-precise.
full_lines_range(StartLine, EndLine) ->
    lsp_utils:client_range(StartLine, 1, EndLine + 1, 1).

inlay_hint_item({Line, Col} = Position, Label, "parameter") ->
    LabelBin = lsp_utils:to_binary(Label),
    #{
        position => lsp_utils:client_position(Position),
        kind => 2, % Parameter
        label => LabelBin,
        paddingRight => true,
        tooltip => #{value => <<"Inferred parameter name">>, kind => <<"plaintext">>},
        textEdits => [#{range => lsp_utils:client_range(Line, Col, Line, Col), newText => LabelBin}]
    };
inlay_hint_item(Position, Label, Kind) ->
    #{
        position => lsp_utils:client_position(Position),
        kind => inlay_kind_number(Kind),
        label => lsp_utils:to_binary(Label),
        paddingLeft => true
    }.

inlay_kind_number("type") -> 1;
inlay_kind_number(_) -> 2.

%% Nothing is deferred here - list already computes everything cheaply
%% (see textDocument_inlayHints/2's own note) - identity, like
%% documentLink_resolve/2 and workspaceSymbol_resolve/2, whose features
%% likewise have no lazy half to defer to begin with.
inlayHint_resolve(_Socket, Hint) ->
    Hint.

textDocument_codeAction(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Range = maps:get(range, Params),
    Context = maps:get(context, Params),
    lsp_codeaction:code_actions(lsp_utils:file_uri_to_file(Uri), Range, Context).

%% Per the LSP spec, `codeAction/resolve`'s own Params *is* the CodeAction
%% being resolved (not wrapped in anything else).
codeAction_resolve(_Socket, CodeAction) ->
    lsp_codeaction:resolve(CodeAction).

%% CHARACTERIZATION / known limitation: no command is registered yet (task
%% 2.1 is infrastructure only, see executeCommandProvider's empty commands
%% list above) - task 2.2+ will dispatch on maps:get(command, Params) once
%% a fix actually needs workspace/executeCommand rather than a plain
%% WorkspaceEdit returned directly from a resolved code action.
workspace_executeCommand(_Socket, _Params) ->
    null.

textDocument_semanticTokens_full(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    lsp_semantic_tokens:full_tokens(lsp_utils:file_uri_to_file(Uri)).

textDocument_semanticTokens_full_delta(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    PreviousResultId = maps:get(previousResultId, Params),
    lsp_semantic_tokens:full_tokens_delta(lsp_utils:file_uri_to_file(Uri), PreviousResultId).

textDocument_semanticTokens_range(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    #{line := LS} = mapmapget(range, start, Params),
    #{line := LE} = mapmapget(range, 'end', Params),
    lsp_semantic_tokens:range_tokens(lsp_utils:file_uri_to_file(Uri), {LS + 1, LE + 1}).

%% Target tells an include (file:filename(), a list) apart from a comment
%% URL (binary) - a documentLink's `target` is a URI string either way.
textDocument_documentLink(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    [#{
        range => lsp_utils:client_range(Line, StartCol, EndCol),
        target => link_target(Target)
     } || {Line, StartCol, EndCol, Target} <- lsp_navigation:document_links(File)].

link_target({url, Url}) -> Url;
link_target({file, Path}) -> lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(Path)).

%% No lazy work is deferred to resolve for this feature (every link
%% already carries its own real target up front) - resolveProvider is
%% declared false in the capability, so the client should never call this,
%% but identity is the correct answer if it ever does.
documentLink_resolve(_Socket, Link) ->
    Link.

textDocument_documentSymbol(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    lists:map(fun ({Name, Kind, {L, C, L1, C1}}) ->
        #{
            name => Name,
            kind => Kind, 
            location => #{ 
                uri => Uri, 
                range => lsp_utils:client_range(L, C, L1, C1)
            }
        }
    end, lsp_navigation:symbol_info(lsp_utils:file_uri_to_file(Uri))).


textDocument_inlineValue(_Socket, Params) ->
    textDocument_inlineValues(_Socket, Params).

% provide inlive values while debugging (values are shown directly in editor)
textDocument_inlineValues(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    ContextMap = maps:get(context, Params),
    #{line:=LE, character:=CE} = mapmapget(stoppedLocation, 'end', ContextMap),
    lists:map(fun ({Kind, Position, Label}) ->
        #{
            position => lsp_utils:client_position(Position),
            kind => lsp_utils:to_binary(Kind), %"var" or "text" or "expression"
            label => lsp_utils:to_binary(Label)
        }
        end, 
        lsp_navigation:inlinevalues_info(lsp_utils:file_uri_to_file(Uri), {LE,CE}))
    .

% Params is like this :
% [
%     #{
%         position => #{line => 28, character => 23},
%         context =>
%             #{
%                 isRetrigger => false,
%                 triggerCharacter => <<"(">>,
%                 triggerKind => 2
%             },
%         textDocument =>
%             #{
%                 uri =>
%                     <<"file:///..../sources/erlang/sample/src/sample.erl">>
%             }
%     }
% ]
% https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureHelpParams
textDocument_signatureHelp(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    IsRetrigger = mapmapget(context, isRetrigger, Params),
    %TriggerCharacter = mapmapfind(context, triggerCharacter, Params),
    %TriggerKind = mapmapget(context, triggerKind, Params), % 1: manual activation, 2: trigger by trigger character, 3: cursor move or content document changing
    
    File = lsp_utils:file_uri_to_file(Uri),
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    SignatureHelp = case IsRetrigger of
        true ->
            % activeSignature or empty
            CurrentResult = mapmapfind(context, activeSignatureHelp, Params, fun lsp_signature:disable_signature_help/0),
            case signature_from_location(FileModule, File, Line, Character) of
                error -> CurrentResult;
                Value -> Value
            end;
        false -> 
            %%if triggerkind ==1, tokens can be used to find method signature
            case signature_from_location(FileModule, File, Line, Character) of
                error -> lsp_signature:disable_signature_help();
                Value -> Value
            end; 
        _ -> []
    end,
    SignatureHelp.

textDocument_prepareRename(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),    
    Character = mapmapget(position, character, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    lsp_rename:prepareRename(File, Line+1, Character+1).

textDocument_rename(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),    
    Character = mapmapget(position, character, Params),    
    NewName = maps:get(newName, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    lsp_rename:rename(Uri,File,Line+1, Character+1, NewName).

signature_from_location(FileModule, File, Line, Character) ->
    %read text before location and take function
    {TextBefore, LineText} = text_before_character(File, Line, Character-1),
    case erl_scan:string(lsp_utils:to_string(LineText),{1,1}) of
        {ok, Tokens, _} -> 
            %filter tokens
            FilteredTokens = lists:filter(fun 
                ({_,{_,Col},_}) when Col =< Character  -> true;
                ({_,{_,Col}}) when Col =< Character  -> true;                
                (_) -> false 
                end, Tokens),
            lsp_signature:signature_help_fromtokens(FileModule, FilteredTokens);
        {error, _, _} -> 
            case erl_scan:string(lsp_utils:to_string(TextBefore),{1,1}) of
                {ok, Tokens, _} -> 
                    lsp_signature:signature_help_fromtokens(FileModule, Tokens);
                {error, ErrorInfo, ErrLoc} -> 
                    ?LOG("parse_error: ~p/~p",[ErrorInfo,ErrLoc]),
                    error
            end
    end.    

validate_file(Socket, File) ->
    case gen_lsp_config_server:linting() of
        true ->
            case filename:extension(File) of
                ".erl" ->
                    validate_parsed_source_file(Socket, File);
                ".src" ->
                    validate_config_file(Socket, File);
                ".config" ->
                    validate_config_file(Socket, File);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%% @doc Background lint of a file that is not open (see lsp_diagnostics):
%% from disk, reparsed first when Mode is `reparse`. A file no longer on disk is
%% cleared.
validate_closed_file(Socket, File, Mode) ->
    case filelib:is_regular(File) of
        false ->
            maybe_send_diagnostics(Socket, File, gen_lsp_doc_server:get_document_version(File), []);
        true ->
            Mode =:= reparse andalso gen_lsp_doc_server:reparse(File),
            validate_file(Socket, File)
    end.

validate_parsed_source_file(Socket, File) ->
    ValidatingVersion = gen_lsp_doc_server:get_document_version(File),
    ?LOG(<<"diag">>, "Validating parsed source file ~p at version ~p", [File, ValidatingVersion]),
    ErrorsWarnings = lsp_syntax:validate_parsed_source_file(File),
    maybe_send_diagnostics(Socket, File, ValidatingVersion, maps:get(errors_warnings, ErrorsWarnings, [])).

validate_config_file(Socket, File) ->
    ValidatingVersion = gen_lsp_doc_server:get_document_version(File),
    {ContentsFile, Cleaner} = case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {File, fun () -> ok end};
        Contents ->
            InnerContentsFile = lsp_utils:make_temporary_file(Contents),
            {InnerContentsFile, fun () -> file:delete(InnerContentsFile) end}
    end,
    ErrorsWarnings = lsp_parse:parse_config_file(File, ContentsFile),
    maybe_send_diagnostics(Socket, File, ValidatingVersion, maps:get(errors_warnings, ErrorsWarnings, [])),
    Cleaner().

-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 21).

formatting(Contents) ->
    case vscode_erlfmt:format_string(binary_to_list(Contents), [{print_width, gen_lsp_config_server:formatting_line_length()}]) of
        {ok, UpdatedContents, _} -> list_to_binary(UpdatedContents);
        {ok, UpdatedContents} -> list_to_binary(UpdatedContents);
        _ -> Contents
    end.

    -else.

formatting(Contents) ->
    TempFile = mktemp(Contents),
    erl_tidy:file(binary_to_list(TempFile), [
        {backups, false},
        {idem, true}
    ]),
    {ok, UpdatedContents} = file:read_file(TempFile),
    file:delete(TempFile),
    UpdatedContents.

    -endif.
-else.

formatting(Contents) ->
    TempFile = mktemp(Contents),
    erl_tidy:file(binary_to_list(TempFile), [
        {backups, false},
        {idem, true}
    ]),
    {ok, UpdatedContents} = file:read_file(TempFile),
    file:delete(TempFile),
    UpdatedContents.

-endif.

%% @doc Source is the call site that asked (<<"initialized">> or
%% <<"didChangeConfiguration">>) - it rides along in the request id, so the log
%% for the response says outright which trigger produced it.
request_configuration(Socket, Source) ->
    gen_lsp_server:send_to_client(Socket, <<"workspace/configuration">>, #{
        id => gen_lsp_server:next_request_id(<<"configuration#", Source/binary>>),
        method => <<"workspace/configuration">>,
        params => #{items => [#{section => <<"erlang">>},
                              #{section => <<"files">>},
                              #{section => <<"<computed>">>},
                              #{section => <<"http">>},
                              #{section => <<"search">>}]}
    }).

%% @doc Only publish diagnostics computed against ValidatingVersion (the
%% document_version captured right before the - possibly slow - parse/lint
%% work started) if no newer document_opened/document_changed has landed
%% in the meantime. Prevents an out-of-order validate_file/2 call, started
%% before a since-applied edit (e.g. a quick fix's workspace/applyEdit
%% firing a normal textDocument/didChange), from finishing late and
%% clobbering a fresher/correct publishDiagnostics with stale results.
%% A closed file is at version 0, so a background lint that started before the
%% file was opened is dropped the same way.
maybe_send_diagnostics(Socket, File, ValidatingVersion, Diagnostics) ->
    case gen_lsp_doc_server:get_document_version(File) of
        ValidatingVersion ->
            ?LOG(<<"diag">>, "maybe_send_diagnostics: pushing for ~p at version ~p (~p diagnostics)",
                [File, ValidatingVersion, length(Diagnostics)]),
            send_diagnostics(Socket, File, Diagnostics);
        CurrentVersion ->
            ?LOG(<<"diag">>, "maybe_send_diagnostics: SKIPPED for ~p, stale version ~p (current ~p)",
                [File, ValidatingVersion, CurrentVersion])
    end.

send_diagnostics(Socket, File, Diagnostics) ->
    ?LOG(<<"diag">>, "send_diagnostics: ~p (~p diagnostics)", [File, length(Diagnostics)]),
    lsp_diagnostics:published(File, Diagnostics),
    gen_lsp_server:send_to_client(Socket, <<"textDocument/publishDiagnostics">>, #{
        method => <<"textDocument/publishDiagnostics">>,
        params => #{
            uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
            diagnostics => lists:map(fun to_lsp_diagnostic/1, Diagnostics)
        }
    }).

to_lsp_diagnostic(Diagnostic) ->
    Info = maps:get(info, Diagnostic),
    LspDiagnostic = #{
        severity => severity(maps:get(type, Diagnostic)),
        range => get_range(Info),
        message => maps:get(message, Info),
        source => lsp_utils:try_get(source, Diagnostic, <<"erl">>),
        data => lsp_utils:try_get(correlation_data, Diagnostic, null)
    },
    case maps:get(related, Diagnostic, undefined) of
        undefined ->
            LspDiagnostic;
        %% Problems reported on an -include line link to where they really
        %% are (see lsp_syntax:extract_group/4).
        Related ->
            LspDiagnostic#{relatedInformation => [#{
                location => #{uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
                              range => lsp_utils:client_range(Line, Character, Line, Character + 1)},
                message => Message}
                || #{file := File, line := Line, character := Character, message := Message} <- Related]}
    end.

%% @doc `erlang/discoverTests` - task 6.1. Delegates to lsp_testing.erl.
erlang_discoverTests(Socket, Params) ->
    lsp_testing:discover_tests(Socket, Params).

%% @doc `erlang/runTests` - task 6.3. Delegates to lsp_testing.erl; progress
%% is streamed to the client as `erlang/testRunProgress` notifications
%% while this request is in flight, the response carries only the summary.
erlang_runTests(Socket, Params) ->
    lsp_testing:run_tests(Socket, Params).

get_range(Info) ->
    LS = maps:get(line, Info),
    CS = maps:get(character, Info),
    LE = lsp_utils:try_get(line_end, Info, LS),
    CE = lsp_utils:try_get(character_end, Info, 256),
    lsp_utils:client_range(LS, CS, LE, CE).


severity(<<"info">>) -> 3;
severity(<<"warning">>) -> 2;
severity(_) -> 1.

auto_complete(File, Line, Text) ->
    RegexList = [
        {"[^a-zA-Z0-9_@](case)[^a-zA-Z0-9_@].*\sof?\r?$", case_of},
        {"[^a-zA-Z0-9_@]([a-z][a-zA-Z0-9_@]*):((?:[a-z][a-zA-Z0-9_@]*)?)\r?$", module_function},
        {"#((?:[a-z][a-zA-Z0-9_@]*)?)\r?$", record},
        {"#([a-z][a-zA-Z0-9_@]*)\.((?:[a-z][a-zA-Z0-9_@]*)?)\r?$", field},
        %% task 5.6: must come before `variable` - "...= ?MAX_" also
        %% matches variable's own "non-word char then [A-Z]..." pattern
        %% (the `?` counts as the delimiter), which would otherwise always
        %% win and dispatch a macro prefix to variable completion instead.
        {"\\?([A-Za-z0-9_@]*)\r?$", macro},
        {"[^a-zA-Z0-9_@]([A-Z][a-zA-Z0-9_@]*)\r?$", variable},
        {"^-([a-z]*)\r?$", attribute},
        {"([a-z][a-zA-Z0-9_@]*)\r?$", atom}
    ],
    case match_regex(Text, RegexList) of
        {case_of, [_]} ->
            lsp_completion:disable_completion();
        {module_function, [Module, Function]} ->
            lsp_completion:module_function(list_to_atom(binary_to_list(Module)), binary_to_list(Function));
        {record, [Record]} ->
            lsp_completion:record(File, binary_to_list(Record));
        {field, [Record, Field]} ->
            lsp_completion:field(File, list_to_atom(binary_to_list(Record)), binary_to_list(Field));
        {variable, [Variable]} ->
            lsp_completion:variable(File, Line, binary_to_list(Variable));
        {attribute, [Attribute]} ->
            lsp_completion:attribute(binary_to_list(Attribute));
        {macro, [Macro]} ->
            lsp_completion:macro(File, binary_to_list(Macro));
        {atom, [Atom]} ->
            lsp_completion:atom(File, binary_to_list(Atom));
        {nomatch, _}
            -> []
    end.

completionItem_resolve(_Socket, Item) ->
    lsp_completion:resolve_item(Item).
  
match_regex(Str, [{Pattern, Result} | T]) ->
    case re:run(Str, Pattern) of
        {match, MatchList} ->
            {Result, lists:map(fun (Part) ->
                binary:part(Str, Part)
            end, lists:nthtail(1, MatchList))};
        nomatch ->
            match_regex(Str, T)
    end;
match_regex(_, []) ->
    {nomatch, []}.

mapmapget(Key1, Key2, Map) ->
    maps:get(Key2, maps:get(Key1, Map)).

mapmapfind(Key1, Key2, Map, NotFound) ->
    case maps:find(Key1, Map) of
        {ok, Value} ->
            case maps:find(Key2, Value) of
                {ok, Value1} -> Value1;
                _ -> NotFound()
                end;
        _ -> NotFound()
    end.