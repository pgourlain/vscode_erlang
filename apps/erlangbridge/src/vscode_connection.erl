-module(vscode_connection).
-behaviour(gen_connection).

-export([start/0]).
-export([debugger_stacktrace/2, debugger_bindings/2, set_breakpoint/2]).

% export for gen_connection behaviour
-export([get_port/0, init/1, decode_request/1]).


%%called with "erl -s vscode_connection -vscode_port 1234"
start() ->
    gen_connection:start(?MODULE).

get_port() ->
    {ok, [[P]]}=init:get_argument(vscode_port),
    P.

init(Port) ->
    init_subscribe(Port),
    init_suspender_resumer().

decode_request(Data) ->
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    case parse_request(Data) of
    {debugger_continue, SPid_as_body} ->
        resume_if_suspended(SPid_as_body),
        int:continue(list_to_pid(SPid_as_body)),
        #{};
    {debugger_next, SPid_as_body} ->
        resume_if_suspended(SPid_as_body),
        int:next(list_to_pid(SPid_as_body)),
        #{};
    {debugger_stepin, SPid_as_body} ->
        resume_if_suspended(SPid_as_body),
        int:step(list_to_pid(SPid_as_body)),
        #{};
    {debugger_stepout, SPid_as_body} ->
        resume_if_suspended(SPid_as_body),
        int:step(list_to_pid(SPid_as_body)),
        #{};
    {debugger_pause, SPid_as_body} ->
        suspend(SPid_as_body),
        debugger_stacktrace(list_to_pid(SPid_as_body), -1);
    {debugger_stacktrace, SPid_as_body} ->
        debugger_stacktrace(list_to_pid(SPid_as_body), -1);
    {debugger_bindings, Body} ->
        Lines =  string:tokens(Body, "\r\n"),
        Pid = lists:nth(1, Lines),
        Sp = erlang:list_to_integer(lists:nth(2, Lines)),
        debugger_bindings(list_to_pid(Pid), Sp);
    {set_bp, Body} ->
        % first deserialize user breaks
        % take first bp to get modulename
        [TargetModuleNameString | Breakpoints] = string:tokens(Body, "\r\n"),
        TargetModuleName = list_to_atom(TargetModuleNameString),
        %interpret module - most likely interpreted already but no harm
        case int:all_breaks(TargetModuleName) of
            [] ->
                case int:interpretable(TargetModuleName) of
                    true -> int:ni(TargetModuleName);
                    _ -> error
                end;
            _ -> ok
        end,
        %delete all existing breakpoints
        lists:foreach(
            fun({{M,L}, _}) -> int:delete_break(M, L) end,
            int:all_breaks(TargetModuleName)),
        %set all incoming breakpoints
        lists:foreach(fun (BpString) ->
                BreakpointParams = list_to_tuple(lists:map(
                    fun (P) ->
                        case re:run(P, "^[0-9]+$") of
                            nomatch ->
                                list_to_atom(P);
                            _ ->
                                list_to_integer(P)
                        end
                    end,
                    string:tokens(BpString, " ")
                )),
                set_breakpoint(TargetModuleName, BreakpointParams)
            end,
            Breakpoints),
        #{}; 
    {debugger_eval, Body} ->
        [PidString, Sp, Expression] = string:tokens(Body, "\r\n"),
        Bindings = debugger_eval_bindings(PidString, Sp),
        {ok, Tokens, _} = erl_scan:string(Expression ++ "."),
        case erl_parse:parse_exprs(Tokens) of
            {ok, Exprs} ->
                try erl_eval:exprs(Exprs, orddict:from_list(Bindings)) of
                    {value, EvalResult, _} -> map_bindings({unused, EvalResult})
                catch
                    _:{Reason, What} ->
                        debugger_eval_error(io_lib:format("~p: ~p", [Reason, What]));
                    _:Exp ->
                        debugger_eval_error(io_lib:format("~p", [Exp]))
                end;
            {error,{_,erl_parse, Messages}} ->
                debugger_eval_error(lists:flatten(Messages))
        end;
    {debugger_exit, _Body} ->
        init:stop(0);
    _ ->
        unknown_command
    end.

debugger_eval_error(Message) ->
    #{value => iolist_to_binary(Message), type => <<"error">>}.

parse_request(Data) ->
    Content = binary_to_list(Data),
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    Lines = string:tokens(Content, "\r\n"),
    Command = list_to_atom(lists:nth(2, string:tokens(lists:nth(1, Lines), " "))),
    Body = string:join(lists:nthtail(5, Lines), "\r\n"),
    {Command, Body}.

set_breakpoint(Module, {line, Line}) ->
    case int:break(Module, Line) of
        ok -> ok;
        Error -> io:format("Cannot set brakepoint ~p:~p by ~p~n", [Module, Line, Error])
    end;
set_breakpoint(Module, {function, Name, Arity}) ->
    case int:break_in(Module, Name, Arity) of
        ok -> gen_connection:send_message_to_vscode(list_to_integer(get_port()), to_string(fbp_verified), #{module => Module, name => Name, arity => Arity});
        {error,function_not_found} -> function_not_found; %it will be signalled to the user as non-verified breakpoint
        Error -> io:format("Cannot set brakepoint ~p:~p/~p by ~p~n", [Module, Name, Arity, Error])
    end.

debugger_eval_bindings("<0.0.0>", _) ->
    [];
debugger_eval_bindings(PidString, Sp) ->
    {ok, Meta} = dbg_iserver:call({get_meta, list_to_pid(PidString)}),
    int:meta(Meta, bindings, erlang:list_to_integer(Sp)).

backtrace_item(I) ->
    {Sp, {M, F, Args, Frame}} = I,
    Line = get_line_from_frame(Frame),
    IndexedArgs = lists:zip(lists:seq(1, length(Args)), Args),
    #{
        sp => Sp,
        module => M,
        function => F,
        args => lists:map(fun({Index, El}) -> map_bindings({iolist_to_binary(io_lib:format("$~p", [Index])), El}) end, IndexedArgs),
        source => sourceFileOf(M),
        line => Line
    }.

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
    {ok, UnderlyingPid} = dbg_iserver:safe_call({get_meta,Pid}),
    BackTrace = int:meta(UnderlyingPid,backtrace,all),
    BTAndSF = add_framesinfo(UnderlyingPid, BackTrace, FirstLine),
    lists:map(fun backtrace_item/1, BTAndSF).

type_of_binding(Value) when is_list(Value) ->
    case lists:all(fun(X) when X >= 32, X < 127 -> true; (_) -> false end, Value) of
        true -> string;
        _ -> type_of_binding(basic, Value)
    end;
type_of_binding(Value) -> type_of_binding(basic, Value).  
type_of_binding(basic, Value) when is_binary(Value) -> binary; 
type_of_binding(basic, Value) when is_boolean(Value) -> boolean;    
type_of_binding(basic, Value) when is_integer(Value) -> integer;
type_of_binding(basic, Value) when is_float(Value) -> float;
type_of_binding(basic, Value) when is_atom(Value) -> atom;
type_of_binding(basic, Value) when is_list(Value) -> list;
type_of_binding(basic, Value) when is_map(Value) -> map;
type_of_binding(basic, Value) when is_tuple(Value) -> tuple;
type_of_binding(basic, Value) when is_function(Value) -> function;
type_of_binding(basic, _) -> unknown.

map_bindings({Name, Value}) ->
    T = type_of_binding(Value),
    H = #{
        name => Name,
        type => T,
        value => iolist_to_binary(io_lib:format("~p", [Value]))
    },
    V = case T of
        list -> 
            IndexedValue = lists:zip(lists:seq(1, length(Value)), Value),
            MapF = fun({Index, El}) -> map_bindings({iolist_to_binary(io_lib:format("~p", [Index])), El}) end,
            Ret = lists:map(MapF, IndexedValue),
            #{children => Ret};
        tuple ->
            List = tuple_to_list(Value),
            IndexedValue = lists:zip(lists:seq(1, length(List)), List),
            MapF = fun({Index, El}) -> map_bindings({iolist_to_binary(io_lib:format("~p", [Index])), El}) end,
            Ret = lists:map(MapF, IndexedValue),
            #{children => Ret};  
        map -> #{children => lists:map(fun map_bindings/1, maps:to_list(Value))};
        _ -> #{}
    end,
    maps:merge(V, H).

debugger_bindings(Pid, Sp) when is_pid(Pid) ->
    {ok, UnderlyingPid} = dbg_iserver:call({get_meta,Pid}),
    Bindings = int:meta(UnderlyingPid, bindings, Sp),
    lists:map(fun map_bindings/1, Bindings).

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

init_suspender_resumer() ->
    register(vscode_suspender_resumer, spawn(fun () -> suspender_resumer_loop() end)).

suspender_resumer_loop() ->
    receive
        {suspend, Pid} ->
            erlang:suspend_process(Pid),
            suspender_resumer_loop();
        {resume_if_suspended, Pid} ->
            case erlang:process_info(Pid, status) of
                {status, suspended} ->
                    erlang:resume_process(Pid);
                _ ->
                    ok
            end,
            suspender_resumer_loop()
    end.

suspend(SPid) ->
    vscode_suspender_resumer ! {suspend, list_to_pid(SPid)}.

resume_if_suspended(SPid) ->
    vscode_suspender_resumer ! {resume_if_suspended, list_to_pid(SPid)}.

decode_debugger_message(VsCodePort, M) ->
    %% samples of M
    %{interpret,mymodule_app}
    %{new_break,{{mymodule_app,15},[active,enable,null,null]}}
    %{new_process,
    %                              {<0.97.0>,
    %                               {mymodule_app,start,[normal,[]]},
    %                               running,{}}}
    case M of
    {new_process, {_Pid, {_Module, module_info, _Args}, _Status, _Other}} ->
        % ignore processes calling module_info started by debugger itself
        ok;
    {Verb, Data} ->
        gen_connection:send_message_to_vscode(VsCodePort,to_string(Verb), to_json(Verb, Data));
    {new_status,Pid,idle,_} ->
        gen_connection:send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, idle}));        
    {new_status,Pid,exit,_} ->
        gen_connection:send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, exit})); 
    {new_status,Pid,break,ModuleAndLine} ->
        %{new_status,<0.3.0>,break,{myapp,11}}   
        gen_connection:send_message_to_vscode(VsCodePort,to_string(on_break), to_json(on_break, {Pid, break, ModuleAndLine}));
    {new_status,Pid,running,_} ->
        %{new_status,<0.3.0>,running,{}}
        gen_connection:send_message_to_vscode(VsCodePort,to_string(new_status), to_json(new_status, {Pid, running}));
    {new_status,_,waiting,_} ->
        %{new_status,<0.3.0>,waiting,{}}: no output not to pollute the console
        ok;
    _ -> 
        io:format("decode debugger receive : ~p~n", [M])    
    end,
    ok.

to_json(new_break, {{Module, Line}, _Options}) ->
    #{ module => Module, line => Line };
to_json(interpret, Data) ->
    #{ module => Data };
to_json(new_process, {Pid, _Start, Status, _Other}) ->
    #{ process => list_to_binary(pid_to_list(Pid)), status => Status };
to_json(new_break, _) ->
    #{};
to_json(new_status, {Pid, Status}) ->
    #{ process => list_to_binary(pid_to_list(Pid)), status => Status };
to_json(new_status, {Pid, exit, normal}) ->
    #{ process => list_to_binary(pid_to_list(Pid)), status => exit, reason => normal };
to_json(new_status, {Pid, break, {Module, Line}}) ->
    #{ process => list_to_binary(pid_to_list(Pid)), status => break, reason => normal, module => Module, line => Line };
to_json(on_break, {Pid, break, {Module, Line}}) ->
    #{ process => list_to_binary(pid_to_list(Pid)), module => Module, line => Line, stacktrace => debugger_stacktrace(Pid, Line) };
to_json(_, _) ->
    #{}.

sourceFileOf(M) ->
    F = fun ({compile,_}) -> true ; (_) -> false end,
    C = [L || L <- M:module_info(), F(L)],
    [{compile, O}] = C,
    F1 = fun({source, _}) -> true ; (_) -> false end,
    S = [I || I <- O, F1(I)],
    [{source, Source}] = S,
    list_to_binary(Source).

to_string(X) when is_atom(X) ->
    erlang:atom_to_list(X);

to_string(X) ->
    X.

response_json(M) ->
    {ok, B} = vscode_jsone:encode(M),
    binary_to_list(iolist_to_binary(
      io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: application/json\nContent-Length: ~p\n\n~s",[byte_size(B), B]))).          