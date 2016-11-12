-module(vscode_connection).


-export([start/0, start/1, send_message_to_vscode/3]).

%%called with "erl -s vscode_connection -vscode_port 1234"
start() ->
    inets:start(),
    %get port from command line
    {ok, [[P]]}=init:get_argument(vscode_port),
    start([erlang:list_to_atom(P)]).

%%called with "erl -s vscode_connection start 1234"
start(Args) ->
    inets:start(),
    %get port from Arg, because -s is used Args is an atom list (from erlang command line doc)
    [P] = Args,
    VsCodePort = erlang:list_to_integer(erlang:atom_to_list(P)),
    % subcribe to int
    init_subscribe(VsCodePort),
    % send that debugger is ready
    start_command_server(VsCodePort),
    ok.


send_message_to_vscode(Port, Verb, Body) ->
    Uri = "http://127.0.0.1:"++erlang:integer_to_list(Port) ++ "/"++Verb,
    httpc:request(post, {Uri, [], "application/json", Body}, [],[]).

%-------------------------------
% Command receiver (from nodejs)
%-------------------------------

start_command_server(VsCodePort) ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(0, [{active, false}]),
                    % get assigned port
                    {ok, Port} = inet:port(Sock),
                    %send to vscode debugger
                    send_message_to_vscode(VsCodePort, "listen", "{\"port\":" ++erlang:integer_to_list(Port)++"}"),
                    loop_command_server(Sock) end).

loop_command_server(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle_command(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop_command_server(Sock).

handle_command(Conn) ->
    %process for commands (breakpoints_list, set_breakpoint, ...)
    case decode_command(Conn) of 
		{add_bp, File, Module, LineNumber} ->
			int:ni(File),
			int:break(Module, LineNumber)
			;
		{remove_bp, File, Module, LineNumber} ->
			int:ni(File),
			int:delete_break(Module, LineNumber)
			;
		{debugger_next, Pid} ->
			int:next(Pid);
		{debugger_step, Pid} ->
			int:step(Pid);
		{debugger_continue, Pid} ->
			int:continue(Pid);
		M ->
			io:format("command receive : ~p~n",[M])
	end,
    gen_tcp:send(Conn, response("Hello World")),
    gen_tcp:close(Conn).

decode_command(Conn) ->
    ok.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).

init_subscribe(VsCodePort) ->
    spawn(fun () -> do_subscribe_loop(VsCodePort) end).

do_subscribe_loop(VsCodePort) ->
    register(?MODULE, self()),
    int:subscribe(),
    loop_subscribe(VsCodePort).

loop_subscribe(VsCodePort) ->
    	receive
		{int, M} ->
			decode_debugger_message(VsCodePort, M),
			loop_subscribe(VsCodePort);
		M ->
			io:format("receive : ~p~n", [M]),
			loop_subscribe(VsCodePort)
	end.

decode_debugger_message(VsCodePort, M) ->
    %% samples of M
    %{interpret,zcmailchecker_app}
    %{new_break,{{zcmailchecker_app,15},[active,enable,null,null]}}
    %{new_process,
    %                              {<0.97.0>,
    %                               {zcmailchecker_app,start,[normal,[]]},
    %                               running,{}}}
    case M of
    {Verb, Data} ->
        send_message_to_vscode(VsCodePort,to_string(Verb), to_json(Verb, Data));
    {new_status,Pid,idle,_} ->
        send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, idle}));        
    {new_status,Pid,exit,normal} ->
        send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, exit, normal}));    
    _ -> 
        io:format("decode debugger receive : ~p~n", [M])    
    end,
    ok.

to_json(new_break, {{Module, Line}, _Options}) ->
    fmt("{\"module\":~p, \"line\":~p}",[to_string(Module), Line]);
    %"{}";
to_json(interpret, Data) ->
    "{\"module\":\"" ++ to_string(Data) ++"\"}";
to_json(new_process, {Pid, _Start, Status, _Other}) ->
    fmt("{\"process\":~p, \"status\":~p}",[pid_to_list(Pid), to_string(Status)]);
to_json(new_break, Data) ->
    "{}";
to_json(new_status, {Pid, idle}) ->
    fmt("{\"process\":~p, \"status\":~p}", [pid_to_list(Pid), to_string(idle)]);
to_json(new_status, {Pid, exit, normal}) ->
    fmt("{\"process\":~p, \"status\":~p,\"reason\":~p}", [pid_to_list(Pid), to_string(exit), to_string(normal)]);
to_json(_, _) ->
    "{}".

fmt(Fmt, Args) ->
    binary_to_list(iolist_to_binary(io_lib:fwrite(Fmt,Args))).

to_string(X) when is_atom(X) ->
    erlang:atom_to_list(X);

to_string(X) ->
    X.