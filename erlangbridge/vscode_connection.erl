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
    gen_tcp:send(Conn, response("Hello World")),
    gen_tcp:close(Conn).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).