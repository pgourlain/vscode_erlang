-module(vscode_connection).


-export([start/0, start/1, send_message_to_vscode/3]).
-export([debugger_stacktrace/2, debugger_bindings/2]).

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
-define(TCP_OPTIONS, [binary, {active, false}]).

start_command_server(VsCodePort) ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(0, ?TCP_OPTIONS),
                    % get assigned port
                    {ok, Port} = inet:port(Sock),
                    %send to vscode debugger
                    send_message_to_vscode(VsCodePort, "listen", "{\"port\":" ++erlang:integer_to_list(Port)++"}"),
                    loop_command_server(Sock) end).

loop_command_server(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Pid = spawn(fun () -> loop_handle_command(Conn) end),
    gen_tcp:controlling_process(Conn, Pid),
    loop_command_server(Sock).


loop_handle_command(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
    {tcp, Socket, Data} ->
        %io:format("Got packet: ~p~n", [Data]),
        Answer = decode_request(Data),
        %io:format("Result request: ~p~n", [Answer]),
        gen_tcp:send(Socket, list_to_binary(Answer)),
        gen_tcp:close(Socket),
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
        response("{}");
    {debugger_next, SPid_as_body} ->
        int:next(list_to_pid(SPid_as_body)),
        response("{}");
    {debugger_stepin, SPid_as_body} ->
        int:step(list_to_pid(SPid_as_body)),
        response("{}");
    {debugger_stepout, SPid_as_body} ->
        int:step(list_to_pid(SPid_as_body)),
        response("{}");
    {debugger_stacktrace, SPid_as_body} ->
        response(debugger_stacktrace(list_to_pid(SPid_as_body), -1));
    {debugger_bindings, Body} ->
        Lines =  string:tokens(Body, "\r\n"),
        Pid = lists:nth(1, Lines),
        Sp = erlang:list_to_integer(lists:nth(2, Lines)),
        response(debugger_bindings(list_to_pid(Pid), Sp));
    {set_bp, Body} ->
        % first deserialize user breaks
        ParseBody = fun(S) -> string:tokens(S, "\r\n") end,
        ParseLine = fun(ML) -> list_to_tuple(string:tokens(ML, ",")) end,
        ToBp = fun({M,L}) -> {list_to_atom(M), list_to_integer(L)} end,
        Bps = lists:map(ToBp, lists:map(ParseLine, ParseBody(Body))),
        % take first bp to get modulename
        { TargetModuleName, _ } = lists:nth(1, Bps),
        %get current breakpoints
        AllBreaks = lists:map(fun({{M,L},_}) -> {M,L} end, int:all_breaks(TargetModuleName)),
        { BpsAlreadySet, BpsToDelete } = lists:partition(fun(X) -> lists:member(X, Bps) end, AllBreaks),
        lists:foreach(fun({M, L}) -> int:delete_break(M, L) end, BpsToDelete),
        %set only for new breakpoints
        lists:foreach(fun({M,L}) -> int:break(M,L) end,
            sets:to_list(sets:subtract(sets:from_list(Bps), sets:from_list(BpsAlreadySet)))
            ),
        response("{}");    
    _ ->
        unknown_command
    end.

parse_request(Data) ->
    Content = binary_to_list(Data),
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    Lines = string:tokens(Content, "\r\n"),
    Command = list_to_atom(lists:nth(2, string:tokens(lists:nth(1, Lines), " "))),
    Body = string:join(lists:nthtail(5, Lines), "\r\n"),
    {Command, Body}.

response(Str) ->
    B = Str,
    binary_to_list(iolist_to_binary(
      io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: application/json\nContent-Length: ~p\n\n~s",[length(B), B]))).

%[{3,{mymodule_app,load_config,[]}},
%{2,{mymodyle_app,start,[normal,[]]}}]
backtrace_to_json([H|T]) ->
    binary_to_list(iolist_to_binary("[" ++ backtrace_item_to_json(H) 
    ++ backtrace_items_to_json(T)    
    ++ "]")).

backtrace_items_to_json([H|T]) ->
    "," ++ backtrace_item_to_json(H) ++ backtrace_items_to_json(T);
backtrace_items_to_json([]) ->
    "".

backtrace_item_to_json(I) ->
    {Sp, {M, F, Args, Frame}} = I,
    Line = get_line_from_frame(Frame),
    %TODO:convert args in standard json
    SArgs = re:replace(io_lib:format("~p", [Args]), "\\r\\n", "", [global,{return,list}]),
    io_lib:format("{\"sp\":\"~p\", \"module\":\"~p\", \"function\":\"~p\", \"args\":\"~s\", \"source\":~p, \"line\":~p}", 
        [Sp, M, F, SArgs, sourceFileOf(M), Line]).

get_line_from_frame({_,{_,Line},_})->
    Line;
get_line_from_frame(_)->
    -1.

add_framesinfo(Pid, BT, FirstLine) ->
    {H, _} = lists:split(length(BT) - 1, BT),
    Frames = [frameinfo(Pid, F) || F <- H],
    [add_frameinfo(F, Frames, FirstLine) || F <- BT].


frameinfo(Pid, F) ->
    {Sp, {_, _, _}} = F,
    Frame = int:meta(Pid, stack_frame, {up, Sp}),
    Frame.

add_frameinfo(F, Frames, FirstLine) ->
    %sample : {2,{mymodule_app,start,[normal,[]]}}
    {Sp, {M, Fun, Args}} = F,
    Frame = lists:keyfind(Sp, 1, Frames),
    NewFrame = case Frame of
        false -> {-1, {uknown_module, FirstLine},[]};
        _ -> Frame
    end,
    {Sp, {M, Fun, Args, NewFrame}}.
    %F.

debugger_stacktrace(Pid, FirstLine) when is_pid(Pid) ->
    %%get the underlying Pid
    {ok, UnderlyingPid} = dbg_iserver:call({get_meta,Pid}),
    BackTrace = int:meta(UnderlyingPid,backtrace,5),
    BTAndSF = add_framesinfo(UnderlyingPid, BackTrace, FirstLine),
    backtrace_to_json(BTAndSF).

bindings_to_json(Bindings) ->
    %sample : [{'Result',[]},{'ConfigFile',"conf/local/myconfig.cfg"},{'_',{error,enoent}}]
    "[" ++ bindings_array_to_json(Bindings) ++ "]".
    %"[{\"name\":\"myvar\", \"value\":\"tagaad\"},{\"name\":\"myvar2\", \"value\":\"oulala\"}]".

bindings_array_to_json([H|T]) when length(T) > 0 ->
    onebinding_to_json(H) ++ "," ++ bindings_array_to_json(T);

bindings_array_to_json([H|_T]) ->
    onebinding_to_json(H);

bindings_array_to_json([]) ->
    "".

onebinding_to_json(B) ->
    {Name, Value} = B,
    Vfn = fun (VarValue) -> re:replace(io_lib:format("~p", [VarValue]), "\\r\\n", "", [global,{return,list}]) end,
    "{\"name\":" ++ io_lib:write_string(atom_to_list(Name)) ++ ", \"value\":" ++ io_lib:write_string(Vfn(Value))++ "}". 

debugger_bindings(Pid, Sp) when is_pid(Pid) ->
    {ok, UnderlyingPid} = dbg_iserver:call({get_meta,Pid}),
    Bindings = int:meta(UnderlyingPid, bindings, Sp),
    bindings_to_json(Bindings).

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
    %{interpret,mymodule_app}
    %{new_break,{{mymodule_app,15},[active,enable,null,null]}}
    %{new_process,
    %                              {<0.97.0>,
    %                               {mymodule_app,start,[normal,[]]},
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
        send_message_to_vscode(VsCodePort,to_string(on_break), to_json(on_break, {Pid, break, ModuleAndLine}));
    {new_status,Pid,running,_} ->
        %{new_status,<0.3.0>,running,{}}
        send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, running}));
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
to_json(new_status, {Pid, Status}) ->
    fmt("{\"process\":~p, \"status\":~p}", [pid_to_list(Pid), to_string(Status)]);
to_json(new_status, {Pid, exit, normal}) ->
    fmt("{\"process\":~p, \"status\":~p,\"reason\":~p}", [pid_to_list(Pid), to_string(exit), to_string(normal)]);
to_json(new_status, {Pid, break, {Module, Line}}) ->
    fmt("{\"process\":~p, \"status\":~p,\"reason\":~p,\"module\":~p, \"line\":~p}", [pid_to_list(Pid), to_string(break), 
        to_string(normal), to_string(Module), Line]);
to_json(on_break, {Pid, break, {Module, Line}}) ->
    StackTrace = debugger_stacktrace(Pid, Line),
    R = io_lib:format("{\"process\":~p, \"module\":~p, \"line\":~p, \"stacktrace\":~s}", 
        [pid_to_list(Pid), to_string(Module), Line, StackTrace]),
    binary_to_list(list_to_binary(R));
to_json(_, _) ->
    "{}".

sourceFileOf(M) ->
    F = fun ({compile,_}) -> true ; (_) -> false end,
    C = [L || L <- M:module_info(), F(L)],
    [{compile, O}] = C,
    F1 = fun({source, _}) -> true ; (_) -> false end,
    S = [I || I <- O, F1(I)],
    [{source, Source}] = S,
    Source.

fmt(Fmt, Args) ->
    binary_to_list(iolist_to_binary(io_lib:fwrite(Fmt,Args))).

to_string(X) when is_atom(X) ->
    erlang:atom_to_list(X);

to_string(X) ->
    X.