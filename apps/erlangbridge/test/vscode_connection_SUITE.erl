-module(vscode_connection_SUITE).

%% Task 7.6: attaching the debugger to a running node. The test node plays
%% the helper node (vscode_connection:attach/3) and the debug adapter's
%% event receiver (a fake HTTP server); a peer node is the attach target.

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [attach_interprets_and_detach_restores_target,
     detach_releases_process_waiting_at_breakpoint,
     attach_to_missing_node_fails].

init_per_suite(Config) ->
    case ensure_distributed() of
        ok -> Config;
        {error, Reason} -> {skip, {no_distribution, Reason}}
    end.

end_per_suite(_Config) ->
    ok.

%% The peer is distributed (the attach connects to it) but controlled over
%% stdio: controlling a peer over a dynamically started distribution fails
%% with noconnection on some hosts.
init_per_testcase(_Case, Config) ->
    {ok, Peer, Target} = peer:start_link(#{name => peer:random_name(?MODULE), connection => standard_io}),
    Src = copy_fixture(Config),
    {ok, attach_target} = compile:file(Src, [debug_info, {outdir, filename:dirname(Src)}]),
    true = peer:call(Peer, code, add_patha, [filename:dirname(Src)]),
    {Receiver, Port} = start_event_receiver(),
    [{peer, Peer}, {target, Target}, {src, Src}, {receiver, Receiver}, {port, Port} | Config].

end_per_testcase(_Case, Config) ->
    exit(?config(receiver, Config), kill),
    peer:stop(?config(peer, Config)).

%%%%%%%%%%%
%% cases %%
%%%%%%%%%%%

attach_interprets_and_detach_restores_target(Config) ->
    Target = ?config(target, Config),
    ok = vscode_connection:attach(Target, port_arg(Config), args_module(Config)),
    CmdPort = wait_listen(),

    ?assertEqual([attach_target], rpc:call(Target, int, interpreted, [])),
    ?assertMatch([{{attach_target, 5}, _}], rpc:call(Target, int, all_breaks, [attach_target])),
    ?assert(is_pid(rpc:call(Target, erlang, whereis, [vscode_connection]))),
    ?assert(is_pid(rpc:call(Target, erlang, whereis, [vscode_connection_command_server]))),
    %% the arguments module is dropped once it has run
    ?assertEqual(false, rpc:call(Target, code, is_loaded, [bp_attach_test])),

    ?assertEqual(<<"{}">>, post_command(CmdPort, "debugger_detach")),
    wait_until(fun() -> rpc:call(Target, erlang, whereis, [vscode_connection]) =:= undefined end),
    wait_until(fun() -> rpc:call(Target, erlang, whereis, [vscode_connection_command_server]) =:= undefined end),
    ?assertEqual([], rpc:call(Target, int, interpreted, [])),
    ?assertEqual([], rpc:call(Target, int, all_breaks, [])),
    ?assertEqual(undefined, rpc:call(Target, application, get_env, [vscode_debugger, port])),
    %% the node itself is still running, and its code runs compiled again
    ?assertEqual(3, rpc:call(Target, attach_target, add, [1, 2])).

detach_releases_process_waiting_at_breakpoint(Config) ->
    Target = ?config(target, Config),
    ok = vscode_connection:attach(Target, port_arg(Config), args_module(Config)),
    CmdPort = wait_listen(),

    Key = rpc:async_call(Target, attach_target, add, [1, 2]),
    wait_event(<<"/on_break">>),
    ?assertEqual(timeout, rpc:nb_yield(Key, 200)),

    ?assertEqual(<<"{}">>, post_command(CmdPort, "debugger_detach")),
    ?assertEqual({value, 3}, rpc:nb_yield(Key, 5000)).

attach_to_missing_node_fails(Config) ->
    [_, Host] = string:split(atom_to_list(node()), "@"),
    Missing = list_to_atom("vscode_no_such_node@" ++ Host),
    ?assertMatch({error, _}, vscode_connection:attach(Missing, port_arg(Config), no_compiled_args_file)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

ensure_distributed() ->
    case node() of
        nonode@nohost ->
            _ = os:cmd("epmd -daemon"),
            case net_kernel:start([list_to_atom("vscode_connection_suite_" ++ os:getpid()), shortnames]) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            ok
    end.

copy_fixture(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "attach"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Src = filename:join(Dir, "attach_target.erl"),
    {ok, _} = file:copy(filename:join(?config(data_dir, Config), "attach_target.erl"), Src),
    Src.

port_arg(Config) ->
    integer_to_list(?config(port, Config)).

%% Same shape as the module the debug adapter generates (ErlangShellDebugger.ts).
args_module(Config) ->
    Src = ?config(src, Config),
    Forms = io_lib:format(
        "-module(bp_attach_test).~n-export([configure/0]).~n"
        "configure() -> int:start(), int:ni(~p), int:break(attach_target, 5), ok.~n", [Src]),
    File = filename:join(filename:dirname(Src), "bp_attach_test.erl"),
    ok = file:write_file(File, Forms),
    {ok, bp_attach_test, Bin} = compile:file(File, [binary]),
    {bp_attach_test, Bin}.

wait_listen() ->
    Body = wait_event(<<"/listen">>),
    {match, [Port]} = re:run(Body, "\"port\":([0-9]+)", [{capture, all_but_first, list}]),
    list_to_integer(Port).

wait_event(Path) ->
    receive
        {event, Path, Body} -> Body
    after 10000 ->
        ct:fail({no_event, Path})
    end.

wait_until(Fun) ->
    wait_until(Fun, 50).
wait_until(_Fun, 0) ->
    ct:fail(condition_never_met);
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> timer:sleep(100), wait_until(Fun, N - 1)
    end.

%% Fake debug adapter event receiver: answers every POST and forwards
%% {event, Path, Body} to the test process.
start_event_receiver() ->
    Self = self(),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, http_bin}, {active, false},
                                     {ip, {127,0,0,1}}, {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    Pid = spawn(fun() -> accept_loop(LSock, Self) end),
    ok = gen_tcp:controlling_process(LSock, Pid),
    {Pid, Port}.

accept_loop(LSock, Test) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> serve(Sock, Test) end),
    accept_loop(LSock, Test).

serve(Sock, Test) ->
    {ok, {http_request, 'POST', {abs_path, Path}, _}} = gen_tcp:recv(Sock, 0, 5000),
    Len = content_length(Sock, 0),
    ok = inet:setopts(Sock, [{packet, raw}]),
    Body = case Len of
        0 -> <<>>;
        _ -> {ok, B} = gen_tcp:recv(Sock, Len, 5000), B
    end,
    Test ! {event, Path, Body},
    gen_tcp:send(Sock, <<"HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok">>),
    gen_tcp:close(Sock).

content_length(Sock, Len) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, {http_header, _, 'Content-Length', _, V}} -> content_length(Sock, binary_to_integer(V));
        {ok, {http_header, _, _, _, _}} -> content_length(Sock, Len);
        {ok, http_eoh} -> Len
    end.

%% Command as the debug adapter sends it (erlangConnection.ts post/3).
post_command(CmdPort, Verb) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, CmdPort, [binary, {active, false}], 5000),
    ok = gen_tcp:send(Sock, ["POST ", Verb, " HTTP/1.1\r\nContent-Type: plain/text\r\n"
                             "Content-Length: 0\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"]),
    Response = recv_all(Sock, <<>>),
    [_Headers, Body] = binary:split(Response, <<"\r\n\r\n">>),
    Body.

recv_all(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} -> recv_all(Sock, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.
