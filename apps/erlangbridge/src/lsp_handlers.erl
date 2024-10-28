-module(lsp_handlers).

-export([initialize/2, initialized/2, shutdown/2, exit/2, cancelRequest/2, setTrace/2, configuration/2,
    workspace_didChangeConfiguration/2, workspace_didChangeWatchedFiles/2,
    textDocument_didOpen/2, textDocument_didClose/2, textDocument_didSave/2, textDocument_didChange/2,
    textDocument_definition/2, textDocument_references/2, textDocument_hover/2, textDocument_completion/2,
    textDocument_formatting/2, textDocument_codeLens/2, textDocument_documentSymbol/2,
    textDocument_signatureHelp/2, textDocument_prepareRename/2, textDocument_rename/2]).
-export([textDocument_inlineValues/2, textDocument_inlineValue/2]).
-export([textDocument_inlayHints/2, textDocument_inlayHint/2]).

-include("lsp_log.hrl").

initialize(_Socket, Params) ->
    % usefull when file is open instead of folder
    RootPath = case maps:get(rootPath, Params) of
        null -> <<"">>;
        Other -> Other
    end,
    gen_lsp_config_server:update_config(root, binary_to_list(RootPath)),
    gen_lsp_doc_server:root_available(),
    #{capabilities => #{
        textDocumentSync => 1, % Full
        completionProvider => #{triggerCharacters => <<":#.">>},
        hoverProvider => true,
        signatureHelpProvider => #{triggerCharacters => <<"(,">>, retriggerCharacters => <<",">>},
        %declarationProvider => true
        definitionProvider => true,
        typeDefinitionProvider => false,
        implementationProvider => false,
        referencesProvider => true,
        documentHighlightProvider => false,
        documentSymbolProvider => true,
        codeActionProvider => false,
        codeLensProvider => true,
        documentLinkProvider => false,
        colorProvider => false,
        documentFormattingProvider => true,
        documentRangeFormattingProvider => false,
        documentOnTypeFormattingProvider => false,
        renameProvider => #{ prepareProvider => true },
        foldingRangeProvider => false,
        executeCommandProvider => false,
        selectionRangeProvider => false,
        linkedEditingRangeProvider => false,
        callHierarchyProvider => false,
        semanticTokensProvider => false,
        monikerProvider => false,
        typeHierarchyProvider => false,
        inlineValueProvider => true,
        inlayHintProvider => true,
        diagnosticProvider => false,
        workspaceSymbolProvider => false

    }}.

initialized(Socket, _Params) ->
    request_configuration(Socket).

shutdown(_Socket, _) ->
    init:stop().

exit(_Socket, _) ->
    init:stop().

cancelRequest(_Socket, _Params) ->
    ok.

setTrace(_Socket, _Params) ->
    ok.

configuration(Socket, [ErlangSection, FilesSection, ComputedSection, HttpSection, SearchSection]) ->
    Documents = gen_lsp_doc_server:opened_documents(),
    gen_lsp_config_server:update_config(erlang, ErlangSection),
    %% because 'verbose' is stored in erlang section, loggin should be after update erlang config
    gen_lsp_server:lsp_log("configuration ~p", [Documents]),
    gen_lsp_config_server:update_config(files, FilesSection),
    gen_lsp_config_server:update_config(computed, ComputedSection),
    gen_lsp_config_server:update_config(http, HttpSection),
    gen_lsp_config_server:update_config(search, SearchSection),
    gen_lsp_server:lsp_log("vscode configuration:~n"
                           " - erlang: ~p~n"
                           " - files: ~p~n"
                           " - computed: ~p~n"
                           " - http: ~p~n"
                           " - search: ~p",
                           [ErlangSection, FilesSection, ComputedSection,
                            HttpSection, SearchSection]),

    %% Scan workspace for source files
    gen_lsp_doc_server:config_change(),

    lists:foreach(fun (File) ->
        gen_lsp_server:lsp_log("File = ~p",[File]),
        send_diagnostics(Socket, File, []),
        validate_file(Socket, File)
    end, Documents).

workspace_didChangeConfiguration(Socket, _Params) ->
    request_configuration(Socket).

workspace_didChangeWatchedFiles(_Socket, Params) ->
    lists:foreach(fun
        (#{uri := Uri, type := 1}) -> % Created 
            gen_lsp_doc_server:project_file_added(lsp_utils:file_uri_to_file(Uri));
        (#{uri := Uri, type := 2}) -> % Changed  
            gen_lsp_doc_server:project_file_changed(lsp_utils:file_uri_to_file(Uri));
        (#{uri := Uri, type := 3}) -> % Deleted  
            gen_lsp_doc_server:project_file_deleted(lsp_utils:file_uri_to_file(Uri))
    end, maps:get(changes, Params)).

textDocument_didOpen(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    gen_lsp_doc_server:document_opened(File, mapmapget(textDocument, text, Params)),
    case gen_lsp_config_server:autosave() of
        true ->
            gen_lsp_doc_server:parse_document(File),
            validate_file(Socket, File);
        _ ->
            ok
    end.

textDocument_didClose(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    send_diagnostics(Socket, File, []),
    gen_lsp_doc_server:document_closed(File).

textDocument_didSave(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    case gen_lsp_config_server:autosave() of
        true ->
            gen_lsp_doc_server:parse_document(File),
            validate_file(Socket, File);
        _ ->
            ok
    end.

textDocument_didChange(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    [ContentChange] = maps:get(contentChanges, Params),
    gen_lsp_doc_server:document_changed(File, maps:get(text, ContentChange)),
    case gen_lsp_config_server:autosave() of
        true ->
            ok;
        _ ->
            gen_lsp_doc_server:parse_document(File),
            validate_file(Socket, File)
    end.

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

textDocument_formatting(_Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    Contents = case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, FileContents} = file:read_file(File),
            FileContents;
        StoredContents ->
            StoredContents
    end,
    UpdatedContents = formatting(Contents),
    [
        #{range =>
            #{
                <<"start">> => #{line => 0, character => 0},
                <<"end">> => #{line => 999999, character => 255}
            },
        newText => UpdatedContents}
    ].

textDocument_codeLens(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    case gen_lsp_config_server:codeLensEnabled() of
        false ->
            [];
        _ ->
            lists:foldl(fun ({Function, RefCount, Exported, Line, Column}, Acc) ->
                Range = lsp_utils:client_range(Line, Column, Column + length(atom_to_list(Function))),
                Base = #{range => Range, data => #{function => Function, count => RefCount, exported => Exported}},
                ExportedCL = Base#{command => #{title => <<"exported">>, command => <<>>}},
                ReferenceCL = case RefCount of
                    0 ->
                        Base#{command => #{title => <<"unused">>, command => <<>>}};
                    _ ->
                        Base#{command => #{
                            title => list_to_binary(integer_to_list(RefCount) ++ " references"),
                            command => <<"editor.action.findReferences">>,
                            arguments => [
                                lsp_utils:file_uri_to_vscode_uri(Uri),
                                #{lineNumber => Line, column => Column}
                            ]
                        }}
                end,
                case {RefCount > 0, Exported} of
                    {_, false} -> [ReferenceCL | Acc];
                    {false, true} -> [ExportedCL | Acc];
                    {true, true} -> [ReferenceCL, ExportedCL | Acc]
                end
            end, [], lsp_navigation:codelens_info(lsp_utils:file_uri_to_file(Uri)))
    end.


textDocument_inlayHint(_Socket, Params) ->
    textDocument_inlayHints(_Socket, Params).

textDocument_inlayHints(_Socket, Params) ->
    %gen_lsp_server:lsp_log("textDocument_inlayHints ~p", [Params]),
    Uri = mapmapget(textDocument, uri, Params),
    case gen_lsp_config_server:inlayHintsEnabled() of
        false -> [];
        _ ->
            %#{range =>
            %   #{'end' => #{character => 51,line => 73},
            %     start => #{character => 0,line => 0}},
            #{line:=LS, character:=CS} = mapmapget(range, start, Params),
            #{line:=LE, character:=CE} = mapmapget(range, 'end', Params),
            lists:map(fun ({Position, Label, Kind}) ->
                #{
                    position => lsp_utils:client_position(Position),
                    kind => lsp_utils:to_binary(Kind), %"type" or "parameter"
                    label => lsp_utils:to_binary(Label)
                }
                end, 
                lsp_navigation:inlayhints_info(lsp_utils:file_uri_to_file(Uri), {LS,CS}, {LE,CE}))
    end.

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

validate_parsed_source_file(Socket, File) ->
    ErrorsWarnings = lsp_syntax:validate_parsed_source_file(File),
    send_diagnostics(Socket, File, maps:get(errors_warnings, ErrorsWarnings, [])).

validate_config_file(Socket, File) ->
    {ContentsFile, Cleaner} = case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {File, fun () -> ok end};
        Contents ->
            InnerContentsFile = lsp_utils:make_temporary_file(Contents),
            {InnerContentsFile, fun () -> file:delete(InnerContentsFile) end}
    end,
    ErrorsWarnings = lsp_parse:parse_config_file(File, ContentsFile),
    send_diagnostics(Socket, File, maps:get(errors_warnings, ErrorsWarnings, [])),
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

request_configuration(Socket) ->
    gen_lsp_server:send_to_client(Socket, #{
        id => <<"configuration">>,
        method => <<"workspace/configuration">>,
        params => #{items => [#{section => <<"erlang">>},
                              #{section => <<"files">>},
                              #{section => <<"<computed>">>},
                              #{section => <<"http">>},
                              #{section => <<"search">>}]}
    }).

send_diagnostics(Socket, File, Diagnostics) ->
    gen_lsp_server:send_to_client(Socket, #{
        method => <<"textDocument/publishDiagnostics">>,
        params => #{
            uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
            diagnostics => lists:map(fun (Diagnostic) ->
                Info = maps:get(info, Diagnostic),
                #{
                    severity => severity(maps:get(type, Diagnostic)),
                    range => get_range(Info),
                    message => maps:get(message, Info),
                    source => lsp_utils:try_get(source, Diagnostic, <<"erl">>),
                    data => lsp_utils:try_get(correlation_data, Diagnostic, null)
                }
            end, Diagnostics)
        }
    }).

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
        {atom, [Atom]} ->
            lsp_completion:atom(File, binary_to_list(Atom));
        {nomatch, _}
            -> []
    end.
  
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