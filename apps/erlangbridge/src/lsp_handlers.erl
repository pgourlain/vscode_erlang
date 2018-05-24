-module(lsp_handlers).

-export([initialize/2, initialized/2, shutdown/2, exit/2, configuration/2, workspace_didChangeConfiguration/2,
    textDocument_didOpen/2, textDocument_didClose/2, textDocument_didSave/2, textDocument_didChange/2,
    textDocument_definition/2, textDocument_formatting/2]).

initialize(_Socket, Params) ->
    gen_lsp_doc_server:set_config(#{
        root => binary_to_list(maps:get(rootPath, Params)),
        linting => true,
        includePaths => [],
        autosave => true
    }),
    #{capabilities => #{
        textDocumentSync => 1, % Full
        definitionProvider => true,
        documentFormattingProvider => true
    }}.

initialized(Socket, _Params) ->
    request_configuration(Socket).

shutdown(_Socket, _) ->
    init:stop().

exit(_Socket, _) ->
    init:stop().

configuration(Socket, [ErlangSection, _, FilesSecton, _]) ->
    error_logger:error_msg("configuration ~p", [gen_lsp_doc_server:get_documents()]),
    gen_lsp_doc_server:set_config((gen_lsp_doc_server:get_config())#{
        linting => maps:get(linting, ErlangSection, true),
        includePaths => maps:get(includePaths, ErlangSection, []),
        autosave => maps:get(autoSave, FilesSecton, <<"afterDelay">>) =:= <<"afterDelay">>
    }),
    lists:foreach(fun (File) ->
        error_logger:error_msg("File = ~p",[File]),
        send_diagnostics(Socket, File, []),
        file_contents_update(Socket, File, undefined)
    end, gen_lsp_doc_server:get_documents()).

workspace_didChangeConfiguration(Socket, _Params) ->
    request_configuration(Socket).

textDocument_didOpen(Socket, Params) ->
    File = file_uri_to_file(mapmapget(textDocument, uri, Params)),
    case maps:get(autosave, gen_lsp_doc_server:get_config(), true) of
        true ->
            file_contents_update(Socket, File, undefined);
        _ ->
            ok
    end.

textDocument_didClose(Socket, Params) ->
    File = file_uri_to_file(mapmapget(textDocument, uri, Params)),
    send_diagnostics(Socket, File, []),
    gen_lsp_doc_server:remove_document(File).

textDocument_didSave(Socket, Params) ->
    File = file_uri_to_file(mapmapget(textDocument, uri, Params)),
    case maps:get(autosave, gen_lsp_doc_server:get_config(), false) of
        true ->
            file_contents_update(Socket, File, undefined);
        _ ->
            ok
    end.

textDocument_didChange(Socket, Params) ->
    File = file_uri_to_file(mapmapget(textDocument, uri, Params)),
    case maps:get(autosave, gen_lsp_doc_server:get_config(), true) of
        false ->
            Version = mapmapget(textDocument, version, Params),
            Contents = if
                Version =:= 1 ->
                    [ContentChange] = maps:get(contentChanges, Params),
                    maps:get(text, ContentChange);
                true ->
                    undefined
            end,
            file_contents_update(Socket, File, Contents);
        _ ->
            ok
    end.

textDocument_definition(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Result = lsp_navigation:goto_definition(file_uri_to_file(Uri), Line + 1, Character + 1),
    case Result of
        #{uri := DefUri, line := DefLine, character := DefCharacter} = _ ->
            #{
                uri => file_uri_to_vscode_uri(DefUri),
                range => #{
                    start => #{line => DefLine - 1, character => DefCharacter - 1},
                    <<"end">> => #{line => DefLine - 1, character => DefCharacter - 1}
                }
            };
        _ ->
            #{error => <<"Definition not found">>}
    end.

textDocument_formatting(_Socket, Params) ->
    erl_tidy:file(file_uri_to_file(mapmapget(textDocument, uri, Params)), [{backups, false}]).

file_contents_update(Socket, File, Contents) ->
    Linting = maps:get(linting, gen_lsp_doc_server:get_config(), true),
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
        ".src" ->
            Linting andalso validate_config_file(Socket, File, ContentsFile);
        ".config" ->
            Linting andalso validate_config_file(Socket, File, ContentsFile)
    end,
    Cleaner().

validate_parsed_source_file(Socket, File) ->
    ErrorsWarnings = lsp_syntax:validate_parsed_source_file(File),
    send_diagnostics(Socket, File, maps:get(errors_warnings, ErrorsWarnings)).

validate_config_file(Socket, File, ContentsFile) ->
    ErrorsWarnings = lsp_syntax:parse_config_file(File, ContentsFile),
    send_diagnostics(Socket, File, maps:get(errors_warnings, ErrorsWarnings)).

mktemp(Contents) ->
    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
    TempPath = filename:basedir(user_cache, "ERL"),
    TempFile = filename:join(TempPath, Rand),
    Result1 = file:ensure_dir(TempFile),
    Result2 = file:write_file(TempFile, Contents),
    file:write_file(TempFile, Contents),
    TempFile.

request_configuration(Socket) ->
    send_request_to_client(Socket, <<"configuration">>, <<"workspace/configuration">>,
        #{items => [#{section => <<"erlang">>}, #{section => <<"files">>}]}).

send_diagnostics(Socket, File, Diagnostics) ->
    BinFile = list_to_binary(File),
    send_request_to_client(Socket, undefined, <<"textDocument/publishDiagnostics">>, #{
        uri => file_uri_to_vscode_uri(<<"file://", BinFile/binary>>),
        diagnostics => lists:map(fun (Diagnostic) ->
            Info = maps:get(info, Diagnostic),
            #{
                severity => severity(maps:get(type, Diagnostic)),
                range => #{
                    start => #{line => maps:get(line, Info) - 1, character => maps:get(character, Info) - 1},
                    <<"end">> => #{line => maps:get(line, Info) - 1, character => 255}
                },
                message => maps:get(message, Info),
                source => <<"erl">>
            }
        end, Diagnostics)
    }).

severity(<<"info">>) -> 3;
severity(<<"warning">>) -> 2;
severity(_) -> 1.

send_request_to_client(Socket, Id, Method, Params) ->
    ParamsMap = case Id of
        undefined -> #{method => Method, params => Params};
        _ -> #{id => Id, method => Method, params => Params}
    end,
    {ok, Json} = vscode_jsone:encode(ParamsMap),
    Header = iolist_to_binary(io_lib:fwrite("Content-Length: ~p", [byte_size(Json)])),
    gen_tcp:send(Socket, <<Header/binary, "\r\n\r\n", Json/binary>>).

mapmapget(Key1, Key2, Map) ->
    maps:get(Key2, maps:get(Key1, Map)).

file_uri_to_file(Uri) ->
    binary_to_list(case Uri of
        <<"file:///", Drive, "%3A", Rest/binary>> -> <<Drive, ":", Rest/binary>>;
        <<"file://", Rest/binary>> -> Rest;
      _ -> Uri
    end).

file_uri_to_vscode_uri(Uri) ->
    case Uri of 
        <<"file://", Drive, ":/", Rest/binary>> -> <<"file:///", Drive, "%3A/", Rest/binary>>;
      _ -> Uri
    end.
