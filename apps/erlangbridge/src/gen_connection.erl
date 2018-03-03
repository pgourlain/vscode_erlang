-module(gen_connection).

-export([behaviour_info/1]).

-export([send_message_to_vscode/3, start/1]).

behaviour_info(callbacks) ->
    [{get_port, 0}, {decode_request, 1}, {init, 1}];
behaviour_info(_) -> undefined.

%%future : use a gen_server

start(Module) ->
    Port = Module:get_port(),
    start(Module, to_integer(Port)).

start(Module, Port) ->
    inets:start(),
    VsCodePort = Port,
    Module:init(Port),
    % subcribe to int
    % init_subscribe(VsCodePort),
    % send that debugger is ready
    start_command_server(VsCodePort, Module),
    ok.

to_integer(Port) when is_atom(Port) ->
    erlang:list_to_integer(erlang:atom_to_list(Port));
to_integer(Port) when is_list(Port) ->
    erlang:list_to_integer(Port);
to_integer(Port) -> Port.

send_message_to_vscode(Port, Verb, Data) ->
    {ok, Json} = vscode_jsone:encode(Data),
    %io:format("Notify ~s ~p~n", [Verb, Json]),
    Uri = "http://127.0.0.1:" ++
	    erlang:integer_to_list(Port) ++ "/" ++ Verb,
    httpc:request(post, {Uri, [], "application/json", Json},
		  [], []).

%-------------------------------
% Command receiver (from nodejs)
%-------------------------------
-define(TCP_OPTIONS, [binary, {active, false}]).

start_command_server(VsCodePort, Module) ->
    spawn(fun () ->
		  {ok, Sock} = gen_tcp:listen(0, ?TCP_OPTIONS),
		  % get assigned port
		  {ok, Port} = inet:port(Sock),
		  %send to vscode debugger
		  send_message_to_vscode(VsCodePort, "listen",
					 #{port => Port}),
		  loop_command_server(Sock, Module)
	  end).

loop_command_server(Sock, Module) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Pid = spawn(fun () -> loop_handle_command(Conn, Module)
		end),
    gen_tcp:controlling_process(Conn, Pid),
    loop_command_server(Sock, Module).

loop_handle_command(Socket, Module) ->
    inet:setopts(Socket, [{active, once}]),
    receive
      {tcp, Socket, Data} ->
	  Answer = response_json(Module:decode_request(Data)),
	  gen_tcp:send(Socket, list_to_binary(Answer)),
	  gen_tcp:close(Socket),
	  loop_handle_command(Socket, Module);
      {tcp_closed, Socket} ->
      error_logger:info_msg("Socket ~p closed~n", [Socket]);
      {tcp_error, Socket, Reason} ->
      error_logger:error_msg("Error on socket ~p reason: ~p~n",
		    [Socket, Reason])
    end.

response_json(M) ->
    {ok, B} = vscode_jsone:encode(M),
    binary_to_list(iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: application/js"
						  "on\nContent-Length: ~p\n\n~s",
						  [byte_size(B), B]))).
