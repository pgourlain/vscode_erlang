-module(gen_lsp_server).

-behavior(gen_server).

%inspired from https://github.com/kevinlynx/erlang-tcpserver/blob/master/test/test.erl
%http://20bits.com/article/erlang-a-generalized-tcp-server

% à regarder
% http://learnyousomeerlang.com/buckets-of-sockets


%API
-export([start_link/1, start_link/2]).

%export for gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([send_to_client/2]).

-define(SERVER, ?MODULE).
%state
-record(state, {socket, content_length, contents}).

start_link(VsCodePort) ->
    start_link(VsCodePort, undefined).

start_link(VsCodePort, Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [VsCodePort, Socket, self()],[]).

init([_VsCodePort, Socket, _Parent]) ->
    {ok, #state{socket = Socket, contents = <<"">>}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {stop, normal, State}.

remove_text_for_logging(#{params := #{contentChanges := ChangesList} = Params} = Input) ->
    Input#{params := Params#{contentChanges := lists:map(fun 
        (#{text := <<Text/binary>>} = Change) when byte_size(Text) > 20 ->
            Cut = binary:part(Text, 0, 20),
            Change#{text := <<Cut/binary, " ...">>};
        (Change) ->
            Change
    end, ChangesList)}};
remove_text_for_logging(#{params := #{textDocument := #{text := Text} = TextDocument} = Params} = Input) when byte_size(Text) > 20 ->
    Cut = binary:part(Text, 0, 20),
    Input#{params := Params#{textDocument := TextDocument#{text := <<Cut/binary, " ...">>}}};
remove_text_for_logging(Input) ->
    Input.

do_contents(Socket, #{method := Method} = Input) ->
    error_logger:error_msg("LSP received ~p", [remove_text_for_logging(Input)]),
    Handler = list_to_atom(binary_to_list(binary:replace(Method, <<"/">>, <<"_">>))),
    case lists:keyfind(Handler, 1, lsp_handlers:module_info(exports)) of
        false ->
            error_logger:error_msg("Method not handled: ~p", [Method]),
            ok;
        {Function, 2} ->
            Result = apply(lsp_handlers, Function, [Socket, maps:get(params, Input, undefined)]),
            case maps:get(id, Input, undefined) of
                undefined ->
                    ok;
                Id ->
                    send_to_client(Socket, #{id => Id, result => Result})
            end
    end;
do_contents(Socket, #{id := Id} = Input) ->
    error_logger:error_msg("LSP received ~p", [Input]),
    Handler = list_to_atom(binary_to_list(binary:replace(Id, <<"/">>, <<"_">>))),
    case lists:keyfind(Handler, 1, lsp_handlers:module_info(exports)) of
        false ->
            error_logger:error_msg("Notification not handled: ~p ~p", [Id, Input]),
            ok;
        {Function, 2} ->
            apply(lsp_handlers, Function, [Socket, maps:get(result, Input, undefined)])
    end.

send_to_client(Socket, Body) ->
    error_logger:error_msg("LSP sends ~p", [Body]),
    {ok, Json} = vscode_jsone:encode(Body),
    Header = iolist_to_binary(io_lib:fwrite("Content-Length: ~p", [byte_size(Json)])),
    gen_tcp:send(Socket, <<Header/binary, "\r\n\r\n", Json/binary>>).

handle_tcp_data(Socket, Contents, State) ->
    StateWithContents = State#state{contents = <<(State#state.contents)/binary, Contents/binary>>},
    StateWithLength = case StateWithContents#state.content_length of
        undefined ->
            HeadersEnd = binary:match(StateWithContents#state.contents, <<"\r\n\r\n">>),
            case HeadersEnd of
                nomatch ->
                    StateWithContents;
                {HeadersSeparatorStart, HeadersSeparatorLen} ->
                    {match, [_, {LengthStart, LengthLen}]} =
                        re:run(StateWithContents#state.contents, "Content-Length: *([0-9]+)"),
                    Length = binary_to_integer(binary:part(StateWithContents#state.contents, LengthStart, LengthLen)),
                    BodyStart = HeadersSeparatorStart + HeadersSeparatorLen,
                    BodyLen = byte_size(StateWithContents#state.contents) - BodyStart,
                    StateWithContents#state{
                        contents = binary:part(StateWithContents#state.contents, BodyStart, BodyLen),
                        content_length = Length
                    }
            end;
        _ ->
            StateWithContents
    end,
    case StateWithLength#state.content_length of
        undefined ->
            StateWithLength;
        ContentLength when ContentLength > byte_size(StateWithLength#state.contents) ->
            StateWithLength;
        ContentLength when ContentLength =:= byte_size(StateWithLength#state.contents) ->
            {ok, Input, _} = vscode_jsone_decode:decode(StateWithLength#state.contents, [{keys, atom}]),
            do_contents(Socket, Input),
            StateWithLength#state{contents = <<"">>, content_length = undefined};
        ContentLength when ContentLength < byte_size(StateWithLength#state.contents) ->
            ShorterContents = binary:part(StateWithLength#state.contents, 0, ContentLength),
            {ok, Input, _} = vscode_jsone_decode:decode(ShorterContents, [{keys, atom}]),
            do_contents(Socket, Input),
            handle_tcp_data(
                Socket,
                binary:part(StateWithLength#state.contents, ContentLength, byte_size(StateWithLength#state.contents) - ContentLength),
                StateWithLength#state{contents = <<"">>, content_length = undefined})
    end.

handle_info({tcp, Socket, Contents}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, handle_tcp_data(Socket, Contents, State)};
handle_info(timeout, #state{socket = Socket} = State) ->
    {ok, _} = gen_tcp:accept(Socket), 
    {noreply, State};
handle_info(Data, State) ->
    error_logger:error_msg("Data = ~p",[Data]),
    send(State#state.socket, <<"ok">>),
    {noreply, State}.    

to_int(X)->X.
handle_info1({tcp, Socket, RawData}, State) ->
    error_logger:error_msg("RawData = ~p",[RawData]),
    %entry point for each feature
    inet:setopts(Socket, [{active, once}]),
    Result = case parse_request(RawData) of
        {set_config, Entries} ->
            gen_lsp_doc_server:set_config(get_entries_map(Entries));
        {parse_source_file, FileName} ->
            lsp_syntax:parse_source_file(file_uri_to_file(FileName), file_uri_to_file(FileName));
        {parse_source_file, FileName, TmpContentsFile} ->
            lsp_syntax:parse_source_file(file_uri_to_file(FileName), TmpContentsFile);
        {validate_parsed_source_file, FileName} ->
            lsp_syntax:validate_parsed_source_file(file_uri_to_file(FileName));
        {validate_config_file, FileName} ->
            lsp_syntax:parse_config_file(file_uri_to_file(FileName), file_uri_to_file(FileName));
        {validate_config_file, FileName, TmpContentsFile} ->
            lsp_syntax:parse_config_file(file_uri_to_file(FileName), TmpContentsFile);
        {complete_module_function, Module} ->
            lsp_completion:module_function(list_to_atom(Module), "");
        {complete_module_function, Module, Prefix} ->
            lsp_completion:module_function(list_to_atom(Module), Prefix);
        {complete_record, Uri} ->
            lsp_completion:record(file_uri_to_file(Uri), "");
        {complete_record, Uri, Prefix} ->
            lsp_completion:record(file_uri_to_file(Uri), Prefix);
        {complete_field, Uri, Record} ->
            lsp_completion:field(file_uri_to_file(Uri), list_to_atom(Record), "");
        {complete_field, Uri, Record, Prefix} ->
            lsp_completion:field(file_uri_to_file(Uri), list_to_atom(Record), Prefix);
        {complete_variable, Uri, Line} ->
            lsp_completion:variable(file_uri_to_file(Uri), list_to_integer(Line), "");
        {complete_variable, Uri, Line, Prefix} ->
            lsp_completion:variable(file_uri_to_file(Uri), list_to_integer(Line), Prefix);
        {format_document, FileName} ->
            format_file_uri(FileName);   
        {document_closed, FileName} ->
            gen_lsp_doc_server:remove_document(file_uri_to_file(FileName)), #{result => true};
        {goto_definition, FileName, Line, Column} -> 
            lsp_navigation:goto_definition(file_uri_to_file(FileName), to_int(Line), to_int(Column));
        {hover_info, FileName, Line, Column} -> 
            lsp_navigation:hover_info(file_uri_to_file(FileName), to_int(Line), to_int(Column));
        {references_info, FileName, Line, Column} ->
            lsp_navigation:references_info(file_uri_to_file(FileName), to_int(Line), to_int(Column));
        {codelens_info, FileName} ->
            lsp_navigation:codelens_info(file_uri_to_file(FileName));
        {stop_server} ->
            init:stop();
        _ ->
            #{parse_result => false,
            error_message => <<"unknown command">>}
    end,
    send(Socket, Result),
    {noreply, State};

handle_info1(timeout, #state{socket = Socket} = State) ->
    {ok, ClientSocket} = gen_tcp:accept(Socket), 
    {noreply, State};

handle_info1({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info1(_Info, StateData) ->
    {noreply, StateData}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%% Features functions 
%% TODO: move each feature in separate file

parse_http(Packet) ->
    {ok, {http_request,'POST', Verb, _}, Rest} = erlang:decode_packet(http, Packet, []),
    {HeadersMap, Body} = parse_http_headers(Rest, #{}),
    {Verb, HeadersMap, Body}.

parse_http_headers(Packet, HeadersMap) ->
    case erlang:decode_packet(httph, Packet, []) of
        {ok, {http_header, _, Header, _, Value}, Rest} ->
            parse_http_headers(Rest, HeadersMap#{Header => Value});
        {ok, http_eoh, Body} ->
            {HeadersMap, Body}
    end.

parse_request(Data) ->
    {Verb, HeadersMap, Body} = parse_http(Data),
    case maps:get("X-Multiline-Body", HeadersMap, "false") of
        "false" ->
            list_to_tuple([list_to_atom(Verb) | string:tokens(binary_to_list(Body), "\r\n")]);
        "true" ->
            {list_to_atom(Verb), binary_to_list(Body)}
    end.

get_entries_map(Entries) ->
    lists:foldl(fun (Line, Acc) ->
        case string:tokens(Line, "=") of
            [Key, Value] -> Acc#{list_to_atom(Key) => Value};
            _ -> Acc
        end
    end, #{}, string:tokens(Entries, "\r\n")).

send(Socket, Data) ->
    Answer = response_json(Data),
    gen_tcp:send(Socket, list_to_binary(Answer)),
    inet:setopts(Socket, [{active,once}]).

response_json(M) ->
    {ok, B} = vscode_jsone:encode(M),
    binary_to_list(iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: application/js"
                          "on\nContent-Length: ~p\n\n~s",
                          [byte_size(B), B]))).

file_uri_to_file(FileName) ->
    case string:left(FileName, 7) of
      "file://" -> string:sub_string(FileName, 8);
      _ -> FileName
    end.

format_file_uri(FileName) ->
    format_file(file_uri_to_file(FileName)).

format_file(FileName) ->
    erl_tidy:file(FileName, [{backups, false}]),
    #{format_result => true}.
