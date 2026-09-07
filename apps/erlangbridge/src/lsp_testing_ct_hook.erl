%% Task 6.3: the Common Test equivalent of lsp_testing_eunit_report.erl - a
%% ct_hooks callback module that streams `erlang/testRunProgress`
%% notifications per test case and records the final status in an ETS
%% table.
%%
%% Modeled on OTP's own cth_surefire.erl (same callback arities -
%% pre_init_per_testcase/4, post_end_per_testcase/5 - taken from
%% common_test's source, not guessed): `post_end_per_testcase/5` fires
%% unconditionally right after a test case returns and its own `Result`
%% argument does *not* tell you whether the test passed - cth_surefire
%% itself ignores it and always records "passed" there. A failure or skip
%% is reported through the *separate* `on_tc_fail/4` / `on_tc_skip/4`
%% callbacks, called an instant later - the same optimistic-then-corrected
%% ordering (and the same reason for it) as upstream.
-module(lsp_testing_ct_hook).

-export([init/2, pre_init_per_testcase/4, post_end_per_testcase/5, on_tc_fail/4, on_tc_skip/4, terminate/1]).

init(_Id, Opts) ->
    Table = proplists:get_value(result_table, Opts),
    [{'$socket', Socket}] = ets:lookup(Table, '$socket'),
    {ok, #{socket => Socket, table => Table}}.

pre_init_per_testcase(Suite, TestCase, Config, State) ->
    notify(State, Suite, TestCase, <<"running">>, undefined),
    {Config, State}.

post_end_per_testcase(Suite, TestCase, _Config, Result, State) ->
    record_and_notify(State, Suite, TestCase, <<"passed">>, undefined),
    {Result, State}.

on_tc_fail(Suite, TestCase, Result, State) ->
    Message = lsp_utils:to_binary(io_lib:format("~p", [Result])),
    record_and_notify(State, Suite, normalize_tc(TestCase), <<"failed">>, Message),
    State.

on_tc_skip(Suite, TestCase, Result, State) ->
    Message = lsp_utils:to_binary(io_lib:format("~p", [Result])),
    record_and_notify(State, Suite, normalize_tc(TestCase), <<"skipped">>, Message),
    State.

terminate(_State) ->
    ok.

%% `on_tc_skip/4` (like cth_surefire's own) can receive {ConfigFunc,
%% GroupName} instead of a plain test case name when a whole group is
%% skipped via its init_per_group.
normalize_tc({ConfigFunc, _GroupName}) -> ConfigFunc;
normalize_tc(TestCase) -> TestCase.

record_and_notify(State, Suite, TestCase, StatusBin, Message) ->
    record(State, Suite, TestCase, StatusBin, Message),
    notify(State, Suite, TestCase, StatusBin, Message).

record(#{table := Table}, Suite, TestCase, StatusBin, Message) ->
    ets:insert(Table, {{Suite, TestCase, 0}, StatusBin, Message, undefined}).

notify(#{socket := Socket}, Suite, TestCase, StatusBin, Message) ->
    gen_lsp_server:send_to_client(Socket, #{
        method => <<"erlang/testRunProgress">>,
        params => #{
            kind => <<"ct">>,
            module => lsp_utils:to_binary(Suite),
            function => lsp_utils:to_binary(TestCase),
            arity => 0,
            status => StatusBin,
            message => optional(Message),
            line => null
        }
    }).

optional(undefined) -> null;
optional(Value) -> Value.
