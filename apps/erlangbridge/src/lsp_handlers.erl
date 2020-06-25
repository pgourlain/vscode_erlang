-module(lsp_handlers).

-export([initialize/2, initialized/2, shutdown/2, exit/2, configuration/2,
    workspace_didChangeConfiguration/2, workspace_didChangeWatchedFiles/2,
    textDocument_didOpen/2, textDocument_didClose/2, textDocument_didSave/2, textDocument_didChange/2,
    textDocument_definition/2, textDocument_references/2, textDocument_hover/2, textDocument_completion/2,
    textDocument_formatting/2, textDocument_codeLens/2, textDocument_documentSymbol/2]).

initialize(_Socket, Params) ->
    gen_lsp_config_server:update_config(root, binary_to_list(maps:get(rootPath, Params))),
    gen_lsp_doc_server:root_available(),
    #{capabilities => #{
        textDocumentSync => 1, % Full
        definitionProvider => true,
        documentFormattingProvider => true,
        referencesProvider => true,
        hoverProvider => true,
        completionProvider => #{triggerCharacters => <<":#.">>},
        codeLensProvider => true,
        documentSymbolProvider => true
    }}.

initialized(Socket, _Params) ->
    request_configuration(Socket).

shutdown(_Socket, _) ->
    init:stop().

exit(_Socket, _) ->
    init:stop().

configuration(Socket, [ErlangSection, ComputedSecton, HttpSection, SearchSection]) ->
    Documents = gen_lsp_doc_server:get_documents(),
    gen_lsp_config_server:update_config(erlang, ErlangSection),
    %% because 'verbose' is stored in erlang section, loggin should be after update erlang config
    gen_lsp_server:lsp_log("configuration ~p", [Documents]),
    gen_lsp_config_server:update_config(computed, ComputedSecton),
    gen_lsp_config_server:update_config(http, HttpSection),
    gen_lsp_config_server:update_config(search, SearchSection),
    gen_lsp_server:lsp_log("vscode configuration:~n"
                           " - erlang: ~p~n"
                           " - computed: ~p~n"
                           " - http: ~p~n"
                           " - search: ~p",
                           [ErlangSection, ComputedSecton, HttpSection, SearchSection]),
    lists:foreach(fun (File) ->
        gen_lsp_server:lsp_log("File = ~p",[File]),
        send_diagnostics(Socket, File, []),
        file_contents_update(Socket, File, undefined)
    end, Documents).

workspace_didChangeConfiguration(Socket, _Params) ->
    request_configuration(Socket).

workspace_didChangeWatchedFiles(_Socket, Params) ->
    lists:foreach(fun
        (#{uri := Uri, type := 1}) -> % Created 
            gen_lsp_doc_server:add_project_file(lsp_utils:file_uri_to_file(Uri));
        (#{uri := _Uri, type := 2}) -> % Changed  
            nothing;
        (#{uri := Uri, type := 3}) -> % Deleted  
            gen_lsp_doc_server:remove_project_file(lsp_utils:file_uri_to_file(Uri))
    end, maps:get(changes, Params)).

textDocument_didOpen(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    gen_lsp_doc_server:set_document_attribute(File, contents, mapmapget(textDocument, text, Params)),
    gen_lsp_config_server:autosave() andalso file_contents_update(Socket, File, undefined).    

textDocument_didClose(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    send_diagnostics(Socket, File, []),
    gen_lsp_doc_server:remove_document(File).

textDocument_didSave(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    gen_lsp_config_server:autosave() andalso file_contents_update(Socket, File, undefined).

textDocument_didChange(Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    case filename:extension(File) of
        ".erl" ->
            [ContentChange] = maps:get(contentChanges, Params),
            
            gen_lsp_doc_server:set_document_attribute(File, contents, maps:get(text, ContentChange));
        _ ->
            ok
    end,
    case gen_lsp_config_server:autosave() of
        false ->
            Version = mapmapget(textDocument, version, Params),
            Contents = if
                Version =:= 1 ->
                    undefined;
                true ->
                    [ParseContentChange] = maps:get(contentChanges, Params),
                    maps:get(text, ParseContentChange)
            end,
            file_contents_update(Socket, File, Contents);
        _ ->
            ok
    end.

textDocument_definition(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    lsp_navigation:goto_definition(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1).

textDocument_references(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Result = lsp_navigation:references_info(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1),
    case Result of
        #{references := References} = _ ->
            lists:map(fun (#{uri := RefUri, line := RefLine, character := RefCharacter} = _) ->
                #{
                    uri => lsp_utils:file_uri_to_vscode_uri(RefUri),
                    range => lsp_utils:client_range(RefLine, RefCharacter, RefCharacter)
                }
            end, References);
        _ ->
            #{}
    end.

textDocument_hover(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    lsp_navigation:hover_info(lsp_utils:file_uri_to_file(Uri), Line + 1, Character + 1).

textDocument_completion(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    File = lsp_utils:file_uri_to_file(Uri),
    Contents = gen_lsp_doc_server:get_document_attribute(File, contents),
    LineText = lists:nth(Line + 1, binary:split(Contents, <<"\n">>, [global])),
    TextBefore = binary:part(LineText, 0, min(Character + 1, byte_size(LineText))),
    auto_complete(File, Line + 1, TextBefore).

textDocument_formatting(_Socket, Params) ->
    File = lsp_utils:file_uri_to_file(mapmapget(textDocument, uri, Params)),
    Contents = case gen_lsp_doc_server:get_document_attribute(File, contents) of
        undefined ->
            {ok, FileContents} = file:read_file(File),
            FileContents;
        StoredContents ->
            StoredContents
    end,
    TempFile = mktemp(Contents),
    erl_tidy:file(binary_to_list(TempFile), [
        {backups, false},
        {idem, true}
    ]),
    {ok, UpdatedContents} = file:read_file(TempFile),
    file:delete(TempFile),
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
        false -> [];
        _ ->
            lists:foldl(fun (#{data := Data} = Item, Acc) ->
                case maps:get(exported, Data) of
                    true ->
                        AccWithExported = [exported_code_lens(Item) | Acc],
                        case maps:get(count, Data) > 0 of
                            true -> [references_code_lens(Uri, Item) | AccWithExported];
                            _ -> AccWithExported
                        end;
                    _ ->
                        [references_code_lens(Uri, Item) | Acc]
                end
            end, [], lsp_navigation:codelens_info(lsp_utils:file_uri_to_file(Uri)))
    end.


textDocument_documentSymbol(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    lsp_navigation:symbol_info(Uri, lsp_utils:file_uri_to_file(Uri)).
    %test_symbols(Uri, 25).

exported_code_lens(#{data := Data} = Item) ->
    StartChar = maps:get(character, Item),
    #{
        range => lsp_utils:client_range(maps:get(line, Item), StartChar, StartChar + byte_size(maps:get(func_name, Data))),
        data => Data,
        command => #{title => <<"exported">>, command => <<>>}
    }.

references_code_lens(Uri, #{data := Data} = Item) ->
    StartChar = maps:get(character, Item),
    #{
        range => lsp_utils:client_range(maps:get(line, Item), StartChar, StartChar + byte_size(maps:get(func_name, Data))),
        data => Data,
        command => case maps:get(count, Data) of
            0 -> #{title => <<"unused">>, command => <<>>};
            _ -> #{
                title => list_to_binary(integer_to_list(maps:get(count, Data)) ++ " private references"),
                command => <<"editor.action.findReferences">>,
                arguments => [
                    lsp_utils:file_uri_to_vscode_uri(Uri),
                    #{lineNumber => maps:get(line, Item), column => maps:get(character, Item)}
                ]
            }
        end
    }.

file_contents_update(Socket, File, Contents) ->
    Linting = gen_lsp_config_server:linting(),
    {ContentsFile, Cleaner} = case Contents of
        undefined ->
            {File, fun () -> ok end};
        _ ->
            InnerContentsFile = mktemp(Contents),
            {InnerContentsFile, fun () -> file:delete(InnerContentsFile) end}
    end,
    case filename:extension(File) of
        ".erl" ->
            lsp_syntax:parse_source_file(File, ContentsFile),
            Linting andalso validate_parsed_source_file(Socket, File);
        ".hrl" ->
            ok;
        ".src" ->
            Linting andalso validate_config_file(Socket, File, ContentsFile);
        ".config" ->
            Linting andalso validate_config_file(Socket, File, ContentsFile)
    end,
    Cleaner().

validate_parsed_source_file(Socket, File) ->
    ErrorsWarnings = lsp_syntax:validate_parsed_source_file(File),
    send_diagnostics(Socket, File, maps:get(errors_warnings, ErrorsWarnings, [])).

validate_config_file(Socket, File, ContentsFile) ->
    ErrorsWarnings = lsp_syntax:parse_config_file(File, ContentsFile),
    send_diagnostics(Socket, File, maps:get(errors_warnings, ErrorsWarnings, [])).

mktemp(Contents) ->
    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36) ++ ".erl",
    TempFile = filename:join(gen_lsp_config_server:tmpdir(), Rand),
    filelib:ensure_dir(TempFile),
    file:write_file(TempFile, Contents),
    TempFile.

request_configuration(Socket) ->
    gen_lsp_server:send_to_client(Socket, #{
        id => <<"configuration">>,
        method => <<"workspace/configuration">>,
        params => #{items => [#{section => <<"erlang">>},
                              #{section => <<"<computed>">>},
                              #{section => <<"http">>},
                              #{section => <<"search">>}]}
    }).

send_diagnostics(Socket, File, Diagnostics) ->
    BinFile = list_to_binary(File),
    gen_lsp_server:send_to_client(Socket, #{
        method => <<"textDocument/publishDiagnostics">>,
        params => #{
            uri => lsp_utils:file_uri_to_vscode_uri(<<"file://", BinFile/binary>>),
            diagnostics => lists:map(fun (Diagnostic) ->
                Info = maps:get(info, Diagnostic),
                #{
                    severity => severity(maps:get(type, Diagnostic)),
                    range => lsp_utils:client_range(maps:get(line, Info), maps:get(character, Info), 256),
                    message => maps:get(message, Info),
                    source => <<"erl">>
                }
            end, Diagnostics)
        }
    }).

severity(<<"info">>) -> 3;
severity(<<"warning">>) -> 2;
severity(_) -> 1.

auto_complete(File, Line, Text) ->
    RegexList = [
        {"[^a-zA-Z0-9_@]([a-z][a-zA-Z0-9_@]*):((?:[a-z][a-zA-Z0-9_@]*)?)$", module_function},
        {"#((?:[a-z][a-zA-Z0-9_@]*)?)$", record},
        {"#([a-z][a-zA-Z0-9_@]*)\.((?:[a-z][a-zA-Z0-9_@]*)?)$", field},
        {"[^a-zA-Z0-9_@]([A-Z][a-zA-Z0-9_@]*)$", variable},
        {"^-([a-z]*)$", attribute},
        {"([a-z][a-zA-Z0-9_@]*)$", atom}
    ],
    case match_regex(Text, RegexList) of
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
