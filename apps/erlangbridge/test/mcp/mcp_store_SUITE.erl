-module(mcp_store_SUITE).

%% Session state (mcp_store): bounded identities, retained collections and signed cursors, worker slots.

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [identity_is_stable_and_bounded,
     collection_count_and_ttl_budgets,
     collection_byte_budget_truncates_with_non_resumable_omission,
     cursors_are_bound_to_session_collection_tool_and_arguments,
     slots_are_bounded_and_released_when_the_owner_dies,
     members_are_bounded].

init_per_testcase(Case, Config) ->
    L = (mcp_policy:ceilings())#{max_items => 2, max_collections => 2, collection_ttl_ms => 400,
                                max_result_bytes => 4096, max_collection_bytes => 4096,
                                max_binary_bytes => 512, max_concurrency => 2},
    Cfg = (mcp_policy:defaults())#{limits => L},
    {ok, Pid} = mcp_store:start_link(Cfg, launch),
    unlink(Pid),
    [{store, Pid}, {case_name, Case} | Config].

end_per_testcase(_, Config) ->
    mcp_store:stop(),
    _ = ?config(store, Config),
    ok.

meta() -> #{scope => #{}, started_at => 1, finished_at => 2, complete => true, truncated => false, omissions => []}.

identity_is_stable_and_bounded(_) ->
    A = mcp_store:entity_id(process, {process, self()}),
    ?assertEqual(A, mcp_store:entity_id(process, {process, self()})),
    ?assertMatch(<<"p_", _/binary>>, A),
    ?assertMatch(<<"t_", _/binary>>, mcp_store:entity_id(ets_table, {ets, x, 1})),
    ?assertMatch(<<"a_", _/binary>>, mcp_store:entity_id(application, {application, kernel})),
    ?assertMatch(<<"m_", _/binary>>, mcp_store:entity_id(module, {module, lists})),
    ?assertEqual({ok, {process, self()}}, mcp_store:resolve_id(A)),
    %% a recreated entity (different key) gets a different id
    ?assertNotEqual(A, mcp_store:entity_id(process, {process, spawn(fun() -> ok end)})),
    %% bookkeeping is bounded (max_items * 10 = 20): the oldest handles expire explicitly
    Ids = [mcp_store:entity_id(process, {process, {fake, I}}) || I <- lists:seq(1, 100)],
    ?assert(maps:get(ids, mcp_store:stats()) =< 20),
    ?assertEqual(expired, mcp_store:resolve_id(A)),
    ?assertEqual(expired, mcp_store:resolve_id(hd(Ids))),
    ?assertMatch({ok, _}, mcp_store:resolve_id(lists:last(Ids))),
    ?assertEqual(expired, mcp_store:resolve_id(<<"p_unknown">>)),
    ?assertEqual(expired, mcp_store:resolve_id(42)),
    ?assertEqual(expired, mcp_store:resolve_id(undefined)).

collection_count_and_ttl_budgets(_) ->
    Items = [{entity, #{<<"id">> => <<"x">>}}],
    {C1, _, _} = mcp_store:put_collection(<<"t">>, <<"h">>, meta(), Items, #{}),
    {C2, _, _} = mcp_store:put_collection(<<"t">>, <<"h">>, meta(), Items, #{}),
    ?assertMatch({ok, _}, mcp_store:fetch_collection(C1, <<"t">>, <<"h">>)),
    {C3, _, _} = mcp_store:put_collection(<<"t">>, <<"h">>, meta(), Items, #{}),
    %% count budget (2): the oldest is evicted
    ?assertEqual({error, cursor_expired}, mcp_store:fetch_collection(C1, <<"t">>, <<"h">>)),
    ?assertMatch({ok, _}, mcp_store:fetch_collection(C2, <<"t">>, <<"h">>)),
    %% bound to tool and args
    ?assertEqual({error, cursor_expired}, mcp_store:fetch_collection(C3, <<"other">>, <<"h">>)),
    ?assertEqual({error, cursor_expired}, mcp_store:fetch_collection(C3, <<"t">>, <<"other">>)),
    %% TTL (400 ms)
    timer:sleep(600),
    ?assertEqual({error, cursor_expired}, mcp_store:fetch_collection(C3, <<"t">>, <<"h">>)),
    ?assertEqual(0, maps:get(collections, mcp_store:stats())),
    ?assertEqual(0, maps:get(collection_bytes, mcp_store:stats())).

collection_byte_budget_truncates_with_non_resumable_omission(_) ->
    Items = [{entity, #{<<"id">> => integer_to_binary(I), <<"pad">> => binary:copy(<<"p">>, 300)}} || I <- lists:seq(1, 100)],
    {C, Meta, Stored} = mcp_store:put_collection(<<"t">>, <<"h">>, meta(), Items, #{}),
    ?assert(length(Stored) < 100),
    ?assert(length(Stored) > 0),
    ?assertEqual(false, maps:get(complete, Meta)),
    ?assertEqual(true, maps:get(truncated, Meta)),
    [Om] = maps:get(omissions, Meta),
    ?assertMatch(#{<<"reason">> := <<"limit_reached">>, <<"resumable">> := false}, Om),
    ?assertEqual(100 - length(Stored), maps:get(<<"count">>, Om)),
    ?assert(maps:get(collection_bytes, mcp_store:stats()) =< 4096),
    {ok, #{items := Kept}} = mcp_store:fetch_collection(C, <<"t">>, <<"h">>),
    ?assertEqual(Stored, Kept).

cursors_are_bound_to_session_collection_tool_and_arguments(_) ->
    Cur = mcp_store:make_cursor(<<"c_1">>, <<"tool">>, <<"args">>, 10),
    ?assertEqual({ok, <<"c_1">>, 10}, mcp_store:parse_cursor(Cur, <<"tool">>, <<"args">>)),
    ?assertEqual({error, invalid_cursor}, mcp_store:parse_cursor(Cur, <<"tool2">>, <<"args">>)),
    ?assertEqual({error, invalid_cursor}, mcp_store:parse_cursor(Cur, <<"tool">>, <<"args2">>)),
    Other = mcp_store:make_cursor(<<"c_2">>, <<"tool">>, <<"args">>, 10),
    [_, _, _, Sig1] = binary:split(Cur, <<".">>, [global]),
    [_, _, _, Sig2] = binary:split(Other, <<".">>, [global]),
    ?assertNotEqual(Sig1, Sig2),
    %% a different offset with the same signature is rejected
    Forged = binary:replace(Cur, <<".10.">>, <<".11.">>),
    ?assertEqual({error, invalid_cursor}, mcp_store:parse_cursor(Forged, <<"tool">>, <<"args">>)),
    [?assertEqual({error, invalid_cursor}, mcp_store:parse_cursor(Bad, <<"tool">>, <<"args">>))
     || Bad <- [<<>>, <<"c1">>, <<"c1.a.b.c">>, <<"c1.a.-5.sig">>, <<"c2.c_1.10.", Sig1/binary>>,
                binary:copy(<<"a">>, 5000), <<"c1.", (binary:copy(<<"a">>, 100))/binary, ".1.sig">>]],
    ?assertEqual({error, invalid_cursor}, mcp_store:parse_cursor(not_binary, <<"tool">>, <<"args">>)),
    %% a new session (new secret) rejects it
    mcp_store:stop(),
    {ok, Pid} = mcp_store:start_link((mcp_policy:defaults()), launch),
    unlink(Pid),
    ?assertEqual({error, invalid_cursor}, mcp_store:parse_cursor(Cur, <<"tool">>, <<"args">>)).

slots_are_bounded_and_released_when_the_owner_dies(_) ->
    P1 = spawn(fun() -> receive stop -> ok end end),
    P2 = spawn(fun() -> receive stop -> ok end end),
    ?assertEqual(ok, mcp_store:acquire_slot(P1)),
    ?assertEqual(ok, mcp_store:acquire_slot(P2)),
    ?assertEqual(busy, mcp_store:acquire_slot(self())),
    ?assertEqual(2, maps:get(slots, mcp_store:stats())),
    P1 ! stop,
    wait(fun() -> maps:get(slots, mcp_store:stats()) =:= 1 end),
    ?assertEqual(ok, mcp_store:acquire_slot(self())),
    mcp_store:release_slot(self()),
    wait(fun() -> maps:get(slots, mcp_store:stats()) =:= 1 end),
    exit(P2, kill),
    wait(fun() -> maps:get(slots, mcp_store:stats()) =:= 0 end).

members_are_bounded(_) ->
    [mcp_store:note_member(list_to_pid("<0.1." ++ integer_to_list(I) ++ ">"), app, I) || I <- lists:seq(1, 6000)],
    ?assert(maps:get(members, mcp_store:stats()) =< 5000),
    ?assertMatch({app, _}, mcp_store:member(list_to_pid("<0.1.6000>"))),
    ?assertEqual(undefined, mcp_store:member(list_to_pid("<0.1.1>"))).

wait(F) -> wait(F, 50).
wait(_, 0) -> ct:fail(never);
wait(F, N) -> case F() of true -> ok; false -> timer:sleep(50), wait(F, N - 1) end.
