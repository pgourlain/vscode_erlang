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


get_port() ->
    gen_server:call(?SERVER, {get_port}).
    %{ok, [[P]]} = init:get_argument(vscode_port), P.

start_link(VsCodePort) ->
    %io:format("gen_server start_link/1(~p)", [VsCodePort]),
    start_link(VsCodePort, undefined).

start_link(VsCodePort, LSock) ->
    %io:format("gen_server start_link/2(~p)", [VsCodePort]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [VsCodePort, LSock, self()],[]).


to_int(V) when is_integer(V) ->
    V;
to_int(V) when is_list(V) ->
    erlang:list_to_integer(V).

init([VsCodePort, LSock, Parent]) ->
    %io:format("gen_server init/1 (~p)", [self()]),
    {ok, #state{vscode_port = to_int(VsCodePort), lsock=LSock, parent=Parent}, 
        0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    %entry point for each feature
    %io:format("handle_info tcp data", []),
    inet:setopts(Socket, [{active, once}]),
    Result = case parse_request(RawData) of
      {validate_text_document, FileName} ->
	  parse_file_uri(FileName);
      {format_document, FileName} ->
	  format_file_uri(FileName);      
      _ ->
	  #{parse_result => false,
	    error_message => <<"unknown command">>}
    end,
    %io:format("handle_info tcp result : ~p", [Result]),
    send(Socket, Result),
    {noreply, State};

handle_info(timeout, #state{lsock = LSock, parent=Parent} = State) ->
    {ok, ClientSocket} = gen_tcp:accept(LSock),	
    %gen_lsp_sup:start_socket(),
    {noreply, State#state{socket=ClientSocket}};

handle_info({tcp_closed, Socket}, State) ->
    %io:format("handle_info tcp closed", []),
    {stop, normal, State};

handle_info(_Info, StateData) ->
    {noreply, StateData}.

terminate(_Reason, #state{socket = Socket, parent=Parent}) ->
    %io:format("terminate (~p)", [_Reason]),
    (catch gen_tcp:close(Socket)),
    spawn_link(fun () -> Ret=supervisor:start_child(Parent, []), io:format("terminate start_child(~p)", [Ret]) end),    
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
    parse_file(file_uri_to_file(FileName)).

parse_file(File) ->
    case epp:parse_file(File, []) of
      {ok, Forms} ->
	  case erl_lint:module(Forms, File) of
	    % nothing wrong
	    {ok, []} -> #{parse_result => true};
	    % just warnings
	    {ok, [Warnings]} ->
		#{parse_result => true,
		  errors_warnings =>
		      extract_error_or_warning(<<"warning">>, Warnings)};
	    % errors, no warnings
	    {error, [Errors], []} ->
		#{parse_result => true,
		  errors_warnings =>
		      extract_error_or_warning(<<"error">>, Errors)};
	    % errors and warnings
	    {error, [Errors], [Warnings]} ->
		%io:format("~p --- ~p", [Errors, Warnings]),
		#{parse_result => true,
		  errors_warnings =>
		      extract_error_or_warning(<<"error">>, Errors) ++
			extract_error_or_warning(<<"warning">>, Warnings)}
	  end;
      {error, _} ->
	  #{parse_result => false,
	    error_message => <<"Cannot open file">>}
    end.

extract_error_or_warning(Type, ErrorsOrWarnings) ->
    [#{type => Type,
       file =>
	   erlang:list_to_binary(element(1, ErrorsOrWarnings)),
       info => extract_info(X)}
     || X <- element(2, ErrorsOrWarnings)].

extract_info(X) ->
    % samples of X
    %{20,erl_parse,["syntax error before: ","load_xy"]}
    %{11,erl_lint,{undefined_function,{load_xy,1}}}]}
    #{line => element(1, X),
      message =>
	  erlang:list_to_binary(io_lib:fwrite("~p",
					      [element(3, X)]))}.

format_file_uri(FileName) ->
    format_file(file_uri_to_file(FileName)).

format_file(FileName) ->
    erl_tidy:file(FileName, [{backups, false}]),
    #{format_result => true}.