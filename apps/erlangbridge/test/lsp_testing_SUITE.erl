-module(lsp_testing_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 6.1/6.3/6.6: erlang/discoverTests and erlang/runTests (including
%% `cover`-based coverage collection).
%% Fixtures (lsp_testing_SUITE_data/):
%%  - sample_tests.erl: plain top-level EUnit tests (one passing, one
%%    failing, one generator, one non-test 0-arity function that must be
%%    filtered out).
%%  - sample_ifdef.erl: an EUnit test reachable only through the *dodged*
%%    syntax tree, guarded by `-ifdef(TEST). ... -endif.` - the normal,
%%    preprocessed tree drops it entirely since TEST isn't defined here.
%%  - sample_SUITE.erl: a Common Test suite whose `all/0` lists one passing
%%    and one failing test case, to exercise lsp_testing_ct_hook.erl.
%%  - codepath_dep_src/codepath_dep.erl + codepath_project/dep_user_tests.erl:
%%    a test module that calls a *separate* project module, to pin
%%    lsp_testing:add_project_ebin_paths/0 - see that test case's own comment.

all() -> [
    discover_tests_finds_eunit_tests_including_ifdef_guarded_ones,
    discover_tests_finds_ct_suite_testcases_excluding_callbacks,
    run_tests_reports_pass_and_fail_for_requested_eunit_tests,
    run_tests_streams_progress_notifications_over_the_socket,
    run_tests_reports_pass_and_fail_for_requested_ct_testcases,
    run_tests_with_coverage_reports_per_line_call_counts,
    run_tests_runs_ifdef_guarded_eunit_tests,
    run_tests_loads_dependency_modules_from_the_project_code_path
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%% This case gets its own root (codepath_project/, a subdirectory of
%% data_dir) and compiles its dependency (codepath_dep, whose *source* lives
%% outside that root, in codepath_dep_src/) into that root's own
%% _build/default/lib/codepath_dep/ebin - the only way lsp_testing:run_tests/2
%% can reach it is via lsp_testing:add_project_ebin_paths/0.
init_per_testcase(run_tests_loads_dependency_modules_from_the_project_code_path, Config) ->
    DataDir = ?config(data_dir, Config),
    Root = filename:join(DataDir, "codepath_project"),
    EbinDir = filename:join([Root, "_build", "default", "lib", "codepath_dep", "ebin"]),
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),
    Source = filename:join([DataDir, "codepath_dep_src", "codepath_dep.erl"]),
    {ok, codepath_dep} = compile:file(Source, [{outdir, EbinDir}, report_errors]),
    %% Whatever an earlier run of this case left behind.
    code:purge(codepath_dep), code:delete(codepath_dep), code:del_path(EbinDir),
    gen_lsp_config_server:update_config(root, Root),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    [{ebin_dir, EbinDir} | Config];
init_per_testcase(_TestCase, Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

discover_tests_finds_eunit_tests_including_ifdef_guarded_ones(_Config) ->
    #{modules := Modules} = lsp_testing:discover_tests(undefined, #{}),
    SampleTests = module_named(<<"sample_tests">>, Modules),
    ?assertMatch(#{kind := <<"eunit">>}, SampleTests),
    TestNames = lists:sort([N || #{name := N} <- maps:get(tests, SampleTests)]),
    ?assertEqual(lists:sort([<<"generator_test_">>, <<"passing_test">>, <<"failing_test">>]), TestNames),

    SampleIfdef = module_named(<<"sample_ifdef">>, Modules),
    ?assertMatch(#{kind := <<"eunit">>}, SampleIfdef),
    ?assertEqual([<<"add_test">>], [N || #{name := N} <- maps:get(tests, SampleIfdef)]).

discover_tests_finds_ct_suite_testcases_excluding_callbacks(_Config) ->
    #{modules := Modules} = lsp_testing:discover_tests(undefined, #{}),
    SampleSuite = module_named(<<"sample_SUITE">>, Modules),
    ?assertMatch(#{kind := <<"ct">>}, SampleSuite),
    TestNames = lists:sort([N || #{name := N} <- maps:get(tests, SampleSuite)]),
    ?assertEqual([<<"failing_case">>, <<"ok_case">>], TestNames).

run_tests_reports_pass_and_fail_for_requested_eunit_tests(_Config) ->
    {ServerSocket, ClientSocket} = open_socket_pair(),
    Params = #{tests => [
        #{module => <<"sample_tests">>, function => <<"passing_test">>},
        #{module => <<"sample_tests">>, function => <<"failing_test">>}
    ]},
    Result = lsp_testing:run_tests(ServerSocket, Params),
    drain(ClientSocket),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    ?assertMatch(#{summary := #{<<"passed">> := 1, <<"failed">> := 1}}, Result).

%% Task 6.6: `passing_test`/`failing_test` are requested (so both their
%% bodies - lines 6 and 9 - run at least once), `generator_test_` (line 12)
%% and `not_a_test_case` (line 15) are not, so they must come back with
%% zero calls.
run_tests_with_coverage_reports_per_line_call_counts(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "sample_tests.erl"),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    Params = #{tests => [
        #{module => <<"sample_tests">>, function => <<"passing_test">>},
        #{module => <<"sample_tests">>, function => <<"failing_test">>}
    ], coverage => true},
    Result = lsp_testing:run_tests(ServerSocket, Params),
    drain(ClientSocket),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    #{coverage := Coverage} = Result,
    FileCoverage = module_named_by_uri(lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)), Coverage),
    Statements = maps:from_list([{L, E} || #{line := L, executed := E} <- maps:get(statements, FileCoverage)]),
    ?assert(maps:get(6, Statements) >= 1),
    ?assert(maps:get(9, Statements) >= 1),
    ?assertEqual(0, maps:get(12, Statements)),
    ?assertEqual(0, maps:get(15, Statements)).

%% `add_test` in sample_ifdef.erl is only reachable at all because discovery
%% reads the *dodged* tree (see that fixture's own comment) - but *running*
%% it also needs `ensure_module_loaded/2` to compile with `{d,'TEST'}`, or
%% the loaded module simply doesn't have this function and the test fails
%% with `undef` instead of actually asserting anything.
run_tests_runs_ifdef_guarded_eunit_tests(_Config) ->
    {ServerSocket, ClientSocket} = open_socket_pair(),
    Params = #{tests => [#{module => <<"sample_ifdef">>, function => <<"add_test">>}]},
    Result = lsp_testing:run_tests(ServerSocket, Params),
    drain(ClientSocket),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    ?assertMatch(#{summary := #{<<"passed">> := 1, <<"failed">> := 0}}, Result).

%% Pins lsp_testing:add_project_ebin_paths/0 / remove_project_ebin_paths/1:
%% before they existed, the bridge node's code path had nothing of the
%% project under test on it beyond the extension's own app, so a test
%% calling a *different* module of its own project - which is the ordinary
%% shape of a real test suite, not a special case - died with `undef`. Every
%% other fixture in this data dir is self-contained, which is why none of
%% them caught it.
run_tests_loads_dependency_modules_from_the_project_code_path(Config) ->
    EbinDir = ?config(ebin_dir, Config),
    ?assertNot(lists:member(EbinDir, code:get_path())),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    Params = #{tests => [#{module => <<"dep_user_tests">>, function => <<"calls_dependency_test">>}]},
    Result = lsp_testing:run_tests(ServerSocket, Params),
    drain(ClientSocket),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    ?assertMatch(#{summary := #{<<"passed">> := 1, <<"failed">> := 0}}, Result),
    %% ... and the run cleans up after itself: this node is the language
    %% server, it must not keep the project's ebin dirs on its path, nor the
    %% modules it pulled in through them - or the next run would ignore a
    %% rebuild done in between.
    ?assertNot(lists:member(EbinDir, code:get_path())),
    ?assertEqual(false, code:is_loaded(codepath_dep)).

run_tests_streams_progress_notifications_over_the_socket(_Config) ->
    {ServerSocket, ClientSocket} = open_socket_pair(),
    Params = #{tests => [#{module => <<"sample_tests">>, function => <<"passing_test">>}]},
    spawn(fun () -> lsp_testing:run_tests(ServerSocket, Params) end),
    Messages = recv_messages_until(ClientSocket, <<"passed">>, 5000, []),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    ?assert(lists:any(fun (M) ->
        maps:get(method, M, undefined) =:= <<"erlang/testRunProgress">> andalso
        maps:get(status, maps:get(params, M), undefined) =:= <<"running">> andalso
        maps:get(function, maps:get(params, M), undefined) =:= <<"passing_test">>
    end, Messages)),
    ?assert(lists:any(fun (M) ->
        maps:get(method, M, undefined) =:= <<"erlang/testRunProgress">> andalso
        maps:get(status, maps:get(params, M), undefined) =:= <<"passed">> andalso
        maps:get(function, maps:get(params, M), undefined) =:= <<"passing_test">>
    end, Messages)).

%% `ct:run_test/1` refuses to run ("not possible to install CT while
%% running in interactive mode") when called from a process that is
%% itself already executing inside a running Common Test session - which
%% every test case in *this* suite is. So, like gen_lsp_doc_server_SUITE's
%% cache_mgmt_* tests, this one runs lsp_testing:run_tests/2 on a fresh
%% `peer` node instead, where CT has never been installed.
run_tests_reports_pass_and_fail_for_requested_ct_testcases(Config) ->
    AppDir = ?config(data_dir, Config),
    Params = #{tests => [
        #{module => <<"sample_SUITE">>, function => <<"ok_case">>},
        #{module => <<"sample_SUITE">>, function => <<"failing_case">>}
    ]},
    {ok, Peer, _Node} = peer:start_link(#{
        name => peer:random_name(?MODULE),
        args => ["-pa" | code:get_path()],
        connection => standard_io
    }),
    try
        Result = peer:call(Peer, ?MODULE, run_on_peer, [AppDir, Params]),
        ?assertMatch(#{summary := #{<<"passed">> := 1, <<"failed">> := 1}}, Result)
    after
        catch peer:stop(Peer)
    end.

%% Runs entirely on the peer node: sets up its own local socket pair (a
%% Socket - really a port() - isn't meaningfully transferable to a
%% different node, so it has to be created where it's used) and discards
%% whatever progress notifications land on the client end.
run_on_peer(AppDir, Params) ->
    application:start(vscode_lsp, permanent),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    timer:sleep(200),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    spawn(fun () -> drain(ClientSocket) end),
    Result = lsp_testing:run_tests(ServerSocket, Params),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    Result.

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

module_named(Name, Modules) ->
    case lists:filter(fun (#{module := M}) -> M =:= Name end, Modules) of
        [M | _] -> M;
        [] -> ?assert(false)
    end.

module_named_by_uri(Uri, CoverageEntries) ->
    case lists:filter(fun (#{uri := U}) -> U =:= Uri end, CoverageEntries) of
        [C | _] -> C;
        [] -> ?assert(false)
    end.

open_socket_pair() ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}], 2000),
    {ok, Server} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    {Server, Client}.

%% Content-Length framed reader, mirroring gen_lsp_server:handle_tcp_data/3
%% on the client side (same shape as lsp_syntax_SUITE's client helper).
recv_message(Socket, Timeout) ->
    recv_message(Socket, <<>>, undefined, Timeout).

recv_message(_Socket, Buffer, Length, _Timeout) when Length =/= undefined, byte_size(Buffer) >= Length ->
    Body = binary:part(Buffer, 0, Length),
    {ok, Term, _} = vscode_jsone_decode:decode(Body, [{keys, atom}]),
    {Term, binary:part(Buffer, Length, byte_size(Buffer) - Length)};
recv_message(Socket, Buffer, undefined, Timeout) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, Data} = gen_tcp:recv(Socket, 0, Timeout),
            recv_message(Socket, <<Buffer/binary, Data/binary>>, undefined, Timeout);
        {Pos, Len} ->
            {match, [_, {LStart, LLen}]} = re:run(Buffer, "Content-Length: *([0-9]+)"),
            Length = binary_to_integer(binary:part(Buffer, LStart, LLen)),
            BodyStart = Pos + Len,
            Rest = binary:part(Buffer, BodyStart, byte_size(Buffer) - BodyStart),
            recv_message(Socket, Rest, Length, Timeout)
    end;
recv_message(Socket, Buffer, Length, Timeout) ->
    {ok, Data} = gen_tcp:recv(Socket, 0, Timeout),
    recv_message(Socket, <<Buffer/binary, Data/binary>>, Length, Timeout).

%% Reads messages off Socket until one whose params.status equals
%% StopStatus has been seen (the run's final notification for the test we
%% care about), or Timeout is hit - whichever comes first.
recv_messages_until(Socket, StopStatus, Timeout, Acc) ->
    {Term, _Rest} = recv_message(Socket, Timeout),
    NewAcc = [Term | Acc],
    Status = maps:get(status, maps:get(params, Term, #{}), undefined),
    case Status of
        StopStatus -> lists:reverse(NewAcc);
        _ -> recv_messages_until(Socket, StopStatus, Timeout, NewAcc)
    end.

%% Drain and discard every progress notification, so a request that
%% completed synchronously (run_tests/2 calling gen_tcp:send on ServerSocket
%% for each notification) doesn't leave the socket buffer inspected by
%% nothing - not required for correctness, just avoids relying on TCP
%% buffering being large enough to never block the sender.
drain(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, _Data} -> drain(Socket)
    after 200 ->
        ok
    end.
