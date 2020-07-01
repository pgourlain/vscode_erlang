-module(vscode_connection).
-behaviour(gen_connection).

-export([start/0, set_breakpoint/2]).

% export for gen_connection behaviour
-export([get_port/0, init/1, decode_request/1]).


%%called with "erl -s vscode_connection -vscode_port 1234"
start() ->
    compile_argumentsfile(),
    gen_connection:start(?MODULE).

get_port() ->
    {ok, [[P]]}=init:get_argument(vscode_port),
    P.

%% before 0.6.2, int:ni(...) were put in -eval command line, but erlang crash in erl_scan and I don't known why...
compile_argumentsfile() ->
    case init:get_argument(compiled_args_file) of
    {ok, [[FileName]]} ->
        io:format("Compiling arguments file  ~p~n", [FileName]),
        %compile file to interpret int:ni(..), list of breapoints int:break(...)
         case compile:file(FileName, [binary]) of
            {ok, ModuleName, Binary} -> 
                io:format("Compile result: sucess ~n", []),
                case code:load_binary(ModuleName, lists:flatten(io_lib:format("~p.beam", [ModuleName])), Binary) of
                    {module, _} -> 
                       io:format("Module ~p loaded~n", [ModuleName]),
                        ModuleName:configure(), ok;
                    _ -> no_compiled_args_file
                end;
            Error -> 
                io:format("Compile result: failed ~p~n", [Error]),
                no_compiled_args_file
        end;
    _ ->
        no_compiled_args_file
    end.

init(Port) ->
    init_subscribe(Port).

decode_request(Data) ->
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    case parse_request(Data) of
    {debugger_continue, SPid_as_body} ->
        int:continue(list_to_pid(SPid_as_body)),
        #{};
    {debugger_next, SPid_as_body} ->
        int:next(list_to_pid(SPid_as_body)),
        #{};
    {debugger_stepin, SPid_as_body} ->
        int:step(list_to_pid(SPid_as_body)),
        #{};
    {debugger_stepout, SPid_as_body} ->
        int:finish(list_to_pid(SPid_as_body)),
        #{};
    {debugger_pause, SPid_as_body} ->
        debugger_pause(list_to_pid(SPid_as_body)),
        #{};
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

debugger_stacktrace(Pid, CurrentLine) when is_pid(Pid) ->
    {ok, UnderlyingPid} = dbg_iserver:safe_call({get_meta,Pid}),
    process_backtrace(UnderlyingPid, int:meta(UnderlyingPid, backtrace, all), [], CurrentLine).

process_backtrace(_UnderlyingPid, [], Frames, _Line) ->
    lists:reverse(Frames);
process_backtrace(UnderlyingPid, [{Sp, {Module, Function, Args}} | T], Frames, Line) ->
    Frame = #{
        sp => Sp,
        module => Module,
        func => list_to_binary(io_lib:format("~p/~p", [Function, arity(Args)])),
        source => sourceFileOf(Module),
        line => Line
    },
    process_backtrace(UnderlyingPid, T, [Frame | Frames], parent_line(UnderlyingPid, Sp, T)).

arity(Args) when is_list(Args) ->
    length(Args);
arity(_) ->
    0.

parent_line(_UnderlyingPid, _Sp, []) ->
    -1;
parent_line(UnderlyingPid, Sp, _StackFrames) ->
    case int:meta(UnderlyingPid, stack_frame, {up, Sp}) of
        {_Sp, {_Module, Line}, _Args} ->
            Line;
        _ ->
            -1
    end.

debugger_pause(Pid) ->
    {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
    dbg_icmd:stop(Meta),
    timer:sleep(100),
    case lists:keyfind(Pid, 1, int:snapshot()) of
        {_, _, break, _} ->
            ok;
        _ ->
            erlang:suspend_process(Pid),
            set_temp_breakpoint(debugger_stacktrace(Pid, -1)),
            erlang:resume_process(Pid)
    end.

set_temp_breakpoint([]) ->
    false;
set_temp_breakpoint([#{module := Module, line := Line} | T]) ->
    case lists:member(Module, int:interpreted()) of
        true ->
            case int:break(Module, Line) of
                ok ->
                    int:action_at_break(Module, Line, delete);
                _ ->
                    ok
            end;
        _ ->
            set_temp_breakpoint(T)
    end;
set_temp_breakpoint(_) ->
    false.

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
            #{children => lists:map(fun map_bindings_nested/1, IndexedValue)};
        tuple ->
            List = tuple_to_list(Value),
            IndexedValue = lists:zip(lists:seq(1, length(List)), List),
            #{children => lists:map(fun map_bindings_nested/1, IndexedValue)};  
        map ->
            #{children => lists:map(fun map_bindings_nested/1, maps:to_list(Value))};
        _ ->
            #{}
    end,
    maps:merge(V, H).

map_bindings_nested({Name, Value}) ->
    map_bindings({iolist_to_binary(io_lib:format("~p", [Name])), Value}).

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
    {break_options, _Data} ->
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

sourceFileOf(undefined) ->
    <<"<unknown.erl>">>;
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
