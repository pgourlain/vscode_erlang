-module(gen_lsp_server).

-behavior(gen_server).

%inspired from https://github.com/kevinlynx/erlang-tcpserver/blob/master/test/test.erl
%http://20bits.com/article/erlang-a-generalized-tcp-server

% à regarder
% http://learnyousomeerlang.com/buckets-of-sockets


%API
-export([start_link/1, start_link/2]).

%export for gen_server
-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%state
-record(state, {vscode_port, port, parent, lsock, socket}).

start_link(VsCodePort) ->
    start_link(VsCodePort, undefined).

start_link(VsCodePort, LSock) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [VsCodePort, LSock, self()],[]).


to_int(V) when is_integer(V) ->
    V;
to_int(V) when is_list(V) ->
    erlang:list_to_integer(V).

init([VsCodePort, LSock, Parent]) ->
    {ok, #state{vscode_port = to_int(VsCodePort), lsock=LSock, parent=Parent}, 
        0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
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

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, ClientSocket} = gen_tcp:accept(LSock), 
    {noreply, State#state{socket=ClientSocket}};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(_Info, StateData) ->
    {noreply, StateData}.

terminate(_Reason, #state{socket = Socket, parent=Parent}) ->
    (catch gen_tcp:close(Socket)),
    spawn_link(
        fun () -> _Ret = supervisor:start_child(Parent, []),
                %error_logger:info_msg("terminate start_child(~p)", [_Ret]),
                ok
        end),    
    ok.

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
