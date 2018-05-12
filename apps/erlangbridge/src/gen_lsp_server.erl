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
        {validate_text_document, FileName} ->
	        parse_file_uri(FileName);
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

parse_request(Data) ->
    Content = binary_to_list(Data),
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    Lines = string:tokens(Content, "\r\n"),
    Command = list_to_atom(lists:nth(2,
				     string:tokens(lists:nth(1, Lines), " "))),
    command_and_args({Command}, lists:nthtail(5, Lines)).

command_and_args(State, [H | T]) ->
    command_and_args(erlang:append_element(State, H), T);
command_and_args(State, []) -> State.

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

parse_file_uri(FileName) ->
    F = file_uri_to_file(FileName),
    case is_src_file(F) of
    true -> lsp_syntax:parse_src_file(F);
    _ -> parse_file(F)
    end.

parse_file(File) ->
    %error_logger:info_report([{parse_file, File}]),
    lsp_syntax:parse_and_lint(File).

is_src_file(File) ->
    case filename:extension(File) of
    ".src" -> true;
    ".config" -> true;
    _ -> false
    end.

format_file_uri(FileName) ->
    format_file(file_uri_to_file(FileName)).

format_file(FileName) ->
    erl_tidy:file(FileName, [{backups, false}]),
    #{format_result => true}.