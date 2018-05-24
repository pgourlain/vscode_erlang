-module(lsp_handlers).

-export([initialize/2, initialized/2, shutdown/2, exit/2, configuration/2, workspace_didChangeConfiguration/2,
    textDocument_didOpen/2, textDocument_definition/2]).

initialize(_Socket, Params) ->
    gen_lsp_doc_server:set_config(#{
        root => binary_to_list(maps:get(rootPath, Params)),
        linting => true,
        includePaths => [],
        autosave => false
    }),
    #{capabilities => #{
        textDocumentSync => 1, % Full
        definitionProvider => true
    }}.

initialized(Socket, _Params) ->
    request_configuration(Socket).

shutdown(_Socket, _) ->
    init:stop().

exit(_Socket, _) ->
    init:stop().

configuration(_Socket, [ErlangSection, _, FilesSecton, _]) ->
    gen_lsp_doc_server:set_config((gen_lsp_doc_server:get_config())#{
        linting => maps:get(linting, ErlangSection, true),
        includePaths => maps:get(includePaths, ErlangSection, []),
        autosave => maps:get(autoSave, FilesSecton, <<>>) =:= <<"afterDelay">>
    }).

workspace_didChangeConfiguration(Socket, _Params) ->
    request_configuration(Socket).

textDocument_didOpen(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    lsp_syntax:parse_source_file(file_uri_to_file(Uri), file_uri_to_file(Uri)),
    error_logger:error_msg("textDocument_didOpen").

textDocument_definition(_Socket, Params) ->
    Uri = mapmapget(textDocument, uri, Params),
    Line = mapmapget(position, line, Params),
    Character = mapmapget(position, character, Params),
    Result = lsp_navigation:goto_definition(file_uri_to_file(Uri), Line + 1, Character + 1),
    error_logger:error_msg("textDocument_definition ~p", [Result]),
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

request_configuration(Socket) ->
    send_request_to_client(Socket, <<"configuration">>, <<"workspace/configuration">>,
        #{items => [#{section => <<"erlang">>}, #{section => <<"files">>}]}).

send_request_to_client(Socket, Id, Method, Params) ->
    {ok, Json} = vscode_jsone:encode(#{
        id => Id,
        method => Method,
        params => Params
    }),
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
