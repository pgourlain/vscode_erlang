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
    Handler = spawn(fun () -> loop_handle_command(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop_command_server(Sock).


loop_handle_command(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
    {tcp, Socket, Data} ->
        %io:format("Got packet: ~p~n", [Data]),
        decode_request(Data),
        loop_handle_command(Socket);
    {tcp_closed, Socket}->
        io:format("Socket ~p closed~n", [Socket]);
    {tcp_error, Socket, Reason} ->
        io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.

decode_request(Data) ->
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    case parse_request(Data) of
    {debugger_continue, SPid_as_body} ->
        int:continue(list_to_pid(SPid_as_body)),
        response("ok");
    _ ->
        ok
    end,
    ok.

parse_request(Data) ->
    Lines = string:tokens(Data, "\r\n"),
    Command = list_to_atom(lists:nth(2, string:tokens(lists:nth(1, Lines), " "))),
    Body = string:join(lists:nthtail(5, Lines), "\r\n"),
    {Command, Body}.

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
    {new_status,Pid,break,ModuleAndLine} ->
        %{new_status,<0.3.0>,break,{myapp,11}}   
        send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, break, ModuleAndLine})); 
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
    fmt("new_break:~p}",[Data]),
    "{}";
to_json(new_status, {Pid, idle}) ->
    fmt("{\"process\":~p, \"status\":~p}", [pid_to_list(Pid), to_string(idle)]);
to_json(new_status, {Pid, exit, normal}) ->
    fmt("{\"process\":~p, \"status\":~p,\"reason\":~p}", [pid_to_list(Pid), to_string(exit), to_string(normal)]);
to_json(new_status, {Pid, break, {Module, Line}}) ->
    fmt("{\"process\":~p, \"status\":~p,\"reason\":~p,\"module\":~p, \"line\":~p}", [pid_to_list(Pid), to_string(break), 
        to_string(normal), to_string(Module), Line]);
to_json(_, _) ->
    "{}".

fmt(Fmt, Args) ->
    binary_to_list(iolist_to_binary(io_lib:fwrite(Fmt,Args))).

to_string(X) when is_atom(X) ->
    erlang:atom_to_list(X);

to_string(X) ->
    X.