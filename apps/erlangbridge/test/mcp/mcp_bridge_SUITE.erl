-module(mcp_bridge_SUITE).

%% Debugger-bridge integration of the MCP inspector (T12): the inspector is
%% pushed to and started inside the *attached target node* only when the
%% adapter asks for it, reports the target's own state, is stopped by a detach,
%% and never touches the node running the helper (LSP/adapter side).
%%
%% Same shape as vscode_connection_SUITE: the test node plays the debug adapter
%% (event receiver + commands), a peer is the target, a second peer (started
%% with -vscode_mcp) or the test node itself is the attach helper.

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [inspector_is_pushed_started_and_stopped_with_the_attach,
     without_the_mcp_flag_nothing_is_pushed_or_started,
     start_is_refused_for_an_invalid_configuration,
     fixed_development_token_is_used_when_given].

init_per_suite(Config) ->
    case ensure_distributed() of
        ok -> Config;
        {error, Reason} -> {skip, {no_distribution, Reason}}
    end.

end_per_suite(_Config) -> ok.

init_per_testcase(_Case, Config) ->
    {ok, Peer, Target} = peer:start_link(#{name => peer:random_name(?MODULE), connection => standard_io}),
    Src = copy_fixture(Config),
    {ok, mcp_attach_target} = compile:file(Src, [debug_info, {outdir, filename:dirname(Src)}]),
    true = peer:call(Peer, code, add_patha, [filename:dirname(Src)]),
    {Receiver, Port} = start_event_receiver(),
    [{peer, Peer}, {target, Target}, {src, Src}, {receiver, Receiver}, {port, Port} | Config].

end_per_testcase(_Case, Config) ->
    exit(?config(receiver, Config), kill),
    catch peer:stop(?config(helper, Config)),
    peer:stop(?config(peer, Config)).

%% Helper started like the adapter does for an MCP-enabled attach: -vscode_mcp 1.
start_helper(Config) ->
    Paths = lists:append([["-pa", P] || P <- code:get_path()]),
    {ok, Helper, _} = peer:start_link(#{name => peer:random_name("mcp_helper"), connection => standard_io,
                                        args => ["-vscode_mcp", "1"] ++ Paths}),
    [{helper, Helper} | Config].

%%%%%%%%%%%
%% cases %%
%%%%%%%%%%%

inspector_is_pushed_started_and_stopped_with_the_attach(Config0) ->
    Config = start_helper(Config0),
    Target = ?config(target, Config),
    Helper = ?config(helper, Config),
    ?assertEqual(ok, peer:call(Helper, vscode_connection, attach,
                               [Target, port_arg(Config), args_module(Config)], 60000)),
    CmdPort = wait_listen(),
    %% pushed to the target, because -vscode_mcp was requested
    ?assertNotEqual(non_existing, rpc:call(Target, code, which, [mcp_sup])),
    ?assertNotEqual(non_existing, rpc:call(Target, code, which, [mcp_server])),
    %% nothing runs until the adapter asks
    ?assertEqual(false, rpc:call(Target, mcp_sup, active, [])),
    Start = decode(post_command(CmdPort, "mcp_start", start_body(#{}))),
    ?assertMatch(#{<<"ok">> := true, <<"host">> := <<"127.0.0.1">>, <<"path">> := <<"/mcp">>,
                   <<"port">> := P, <<"token">> := T, <<"sessionId">> := _} when is_integer(P) andalso P > 0 andalso is_binary(T),
                 Start),
    #{<<"port">> := McpPort, <<"token">> := Token} = Start,
    ?assert(byte_size(Token) >= 43),
    %% the inspector lives in the target, not in the adapter/helper/test nodes
    ?assertEqual(true, rpc:call(Target, mcp_sup, active, [])),
    ?assertEqual(false, mcp_sup:active()),
    ?assertEqual(undefined, whereis(mcp_server)),
    ?assertEqual(false, peer:call(Helper, mcp_sup, active, [])),
    %% it reports the target's own state (attach mode, interpreted module, breakpoint)
    Summary = tool(McpPort, Token, <<"runtime_summary">>, #{<<"redactNodeHost">> => false}),
    ?assertEqual(atom_to_binary(Target, utf8), maps:get(<<"node">>, Summary)),
    Debug = tool(McpPort, Token, <<"debug_session">>, #{}),
    ?assertEqual(<<"attach">>, maps:get(<<"mode">>, Debug)),
    ?assertEqual([<<"mcp_attach_target">>], maps:get(<<"interpretedModules">>, Debug)),
    ?assertEqual([#{<<"module">> => <<"mcp_attach_target">>, <<"line">> => 5}], maps:get(<<"breakpoints">>, Debug)),
    %% no source, environment or credentials
    Text = iolist_to_binary(io_lib:format("~p", [Debug])),
    ?assertEqual(nomatch, binary:match(Text, list_to_binary(?config(src, Config)))),
    ?assertEqual(nomatch, binary:match(Text, Token)),
    %% the token was not sent through the debug event channel
    ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~p", [events()])), Token)),
    %% the request journal reaches the adapter as /mcp_call events, without the token
    Journal = wait_event(<<"/mcp_call">>),
    %% (journal entries are sent asynchronously: any of the two calls may come first)
    ?assertMatch(#{<<"method">> := <<"tools/call">>, <<"status">> := <<"ok">>, <<"seq">> := S} when S >= 1,
                 decode(Journal)),
    ?assertEqual(nomatch, binary:match(Journal, Token)),
    %% lease renewal from the adapter
    ?assertMatch(#{<<"ok">> := true}, decode(post_command(CmdPort, "mcp_renew", <<>>))),
    %% a second start on the same node is refused
    ?assertMatch(#{<<"ok">> := false, <<"error">> := _}, decode(post_command(CmdPort, "mcp_start", start_body(#{})))),
    %% detach: the inspector is stopped, its endpoint closed, the target keeps running
    ?assertEqual(<<"{}">>, post_command(CmdPort, "debugger_detach", <<>>)),
    wait_until(fun() -> rpc:call(Target, mcp_sup, active, []) =:= false end),
    ?assertMatch({error, econnrefused}, gen_tcp:connect({127, 0, 0, 1}, McpPort, [binary], 1000)),
    ?assertEqual(3, rpc:call(Target, mcp_attach_target, add, [1, 2])),
    ?assert(is_pid(rpc:call(Target, erlang, whereis, [init]))).

without_the_mcp_flag_nothing_is_pushed_or_started(Config) ->
    Target = ?config(target, Config),
    ok = vscode_connection:attach(Target, port_arg(Config), args_module(Config)),
    CmdPort = wait_listen(),
    ?assertEqual(non_existing, rpc:call(Target, code, which, [mcp_sup])),
    ?assertEqual(non_existing, rpc:call(Target, code, which, [mcp_policy])),
    ?assertMatch(#{<<"ok">> := false, <<"error">> := <<"the MCP inspector is not available", _/binary>>},
                 decode(post_command(CmdPort, "mcp_start", start_body(#{})))),
    ?assertMatch(#{<<"ok">> := false}, decode(post_command(CmdPort, "mcp_renew", <<>>))),
    ?assertEqual(<<"{}">>, post_command(CmdPort, "debugger_detach", <<>>)),
    wait_until(fun() -> rpc:call(Target, erlang, whereis, [vscode_connection]) =:= undefined end).

start_is_refused_for_an_invalid_configuration(Config0) ->
    Config = start_helper(Config0),
    Target = ?config(target, Config),
    ok = peer:call(Helper = ?config(helper, Config), vscode_connection, attach,
                   [Target, port_arg(Config), args_module(Config)], 60000),
    _ = Helper,
    CmdPort = wait_listen(),
    [begin
         ?assertMatch(#{<<"ok">> := false, <<"error">> := _}, decode(post_command(CmdPort, "mcp_start", Body))),
         ?assertEqual(false, rpc:call(Target, mcp_sup, active, []))
     end || Body <- [start_body(#{<<"host">> => <<"0.0.0.0">>}),
                     start_body(#{<<"port">> => 70000}),
                     start_body(#{<<"authToken">> => <<"x">>}),
                     start_body(#{<<"required">> => true}),
                     <<"not json">>,
                     <<"[]">>]],
    ?assertEqual(<<"{}">>, post_command(CmdPort, "debugger_detach", <<>>)).

fixed_development_token_is_used_when_given(Config0) ->
    Config = start_helper(Config0),
    Target = ?config(target, Config),
    ok = peer:call(?config(helper, Config), vscode_connection, attach,
                   [Target, port_arg(Config), args_module(Config)], 60000),
    CmdPort = wait_listen(),
    Fixed = <<"dev-fixed-token-0123456789">>,
    #{<<"ok">> := true, <<"port">> := Port, <<"token">> := Fixed} =
        decode(post_command(CmdPort, "mcp_start", start_body(#{<<"authToken">> => Fixed}))),
    ?assertMatch(#{<<"sessionId">> := _}, tool(Port, Fixed, <<"runtime_summary">>, #{})),
    %% the fixed token is the only credential
    {ok, S} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
    ok = gen_tcp:send(S, ["POST /mcp HTTP/1.1\r\nHost: 127.0.0.1:", integer_to_list(Port),
                          "\r\nAuthorization: Bearer wrong\r\nContent-Type: application/json\r\nContent-Length: 2\r\n\r\n{}"]),
    ?assertMatch(<<"HTTP/1.1 401", _/binary>>, recv_all(S, <<>>)),
    ?assertEqual(<<"{}">>, post_command(CmdPort, "debugger_detach", <<>>)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

start_body(Override) ->
    Base = (mcp_policy:to_json_map(mcp_policy:defaults()))#{<<"mode">> => <<"attach">>},
    {ok, Json} = vscode_jsone:encode(maps:merge(Base, Override)),
    iolist_to_binary(Json).

decode(Bin) ->
    {ok, T, _} = vscode_jsone_decode:decode(Bin),
    T.

%% MCP tool call over the real endpoint -> structuredContent
tool(Port, Token, Name, Args) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"tools/call">>,
            <<"params">> => #{<<"name">> => Name, <<"arguments">> => Args}},
    {ok, Json} = vscode_jsone:encode(Req),
    Body = iolist_to_binary(Json),
    {ok, S} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
    ok = gen_tcp:send(S, ["POST /mcp HTTP/1.1\r\nHost: 127.0.0.1:", integer_to_list(Port),
                          "\r\nAuthorization: Bearer ", Token, "\r\nContent-Type: application/json\r\n"
                          "Content-Length: ", integer_to_list(byte_size(Body)), "\r\n\r\n", Body]),
    Raw = recv_all(S, <<>>),
    [_, RespBody] = binary:split(Raw, <<"\r\n\r\n">>),
    #{<<"result">> := #{<<"isError">> := false, <<"structuredContent">> := Structured}} = decode(RespBody),
    Structured.

events() ->
    receive {event, P, B} -> [{P, B} | events()] after 0 -> [] end.

ensure_distributed() ->
    case node() of
        nonode@nohost ->
            _ = os:cmd("epmd -daemon"),
            case net_kernel:start([list_to_atom("mcp_bridge_suite_" ++ os:getpid()), shortnames]) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ -> ok
    end.

copy_fixture(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "attach"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Src = filename:join(Dir, "mcp_attach_target.erl"),
    {ok, _} = file:copy(filename:join(?config(data_dir, Config), "mcp_attach_target.erl"), Src),
    Src.

port_arg(Config) -> integer_to_list(?config(port, Config)).

args_module(Config) ->
    Src = ?config(src, Config),
    Forms = io_lib:format(
        "-module(bp_attach_test).~n-export([configure/0]).~n"
        "configure() -> int:start(), int:ni(~p), int:break(mcp_attach_target, 5), ok.~n", [Src]),
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
    after 10000 -> ct:fail({no_event, Path})
    end.

wait_until(Fun) -> wait_until(Fun, 50).
wait_until(_Fun, 0) -> ct:fail(condition_never_met);
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> timer:sleep(100), wait_until(Fun, N - 1)
    end.

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

%% Command as the debug adapter sends it (erlangConnection.ts post/3), with a body.
post_command(CmdPort, Verb, Body) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, CmdPort, [binary, {active, false}], 5000),
    ok = gen_tcp:send(Sock, ["POST ", Verb, " HTTP/1.1\r\nContent-Type: plain/text\r\n"
                             "Content-Length: ", integer_to_list(byte_size(Body)),
                             "\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n", Body]),
    Response = recv_all(Sock, <<>>),
    [_Headers, RespBody] = binary:split(Response, <<"\r\n\r\n">>),
    RespBody.

recv_all(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} -> recv_all(Sock, <<Acc/binary, Data/binary>>);
        {error, _} -> Acc
    end.
