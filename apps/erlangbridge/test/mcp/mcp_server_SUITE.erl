-module(mcp_server_SUITE).

%% End-to-end tests of the embedded MCP inspector over its real HTTP endpoint,
%% against a deterministic OTP fixture (mcp_fixture_sup) whose expected
%% topology is defined independently of the inspector output.
%%
%% The fixture contains SENTINEL_* strings in every place the inspector must
%% never read (process state/dictionary/mailbox, ETS objects, child start
%% arguments, application environment): no result may contain "SENTINEL".

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [unauthenticated_and_bad_tokens_rejected,
     host_and_origin_validated,
     method_content_type_and_size_enforced,
     initialize_negotiates_and_lists_only_readonly_tools,
     allowlist_enforced,
     arguments_are_strict,
     runtime_summary_redacts_node_host,
     application_overview_discovers_fixture_without_names,
     supervision_tree_matches_independent_expected_graph,
     linked_unsupervised_process_is_not_supervised,
     process_info_inputs_are_safe,
     registered_processes_membership_evidence,
     ets_tables_metadata_only_for_approved_tables,
     top_processes_ranks_and_is_bounded,
     supervision_tree_reports_child_counts,
     mermaid_format_renders_the_returned_edges,
     paused_process_is_reported_from_debugger_evidence,
     restart_changes_identity,
     pagination_cursors_and_retention,
     collection_byte_budget_is_partial_and_non_resumable,
     blocked_supervisor_yields_partial_result,
     concurrency_is_bounded,
     stop_closes_endpoint_and_invalidates_credentials,
     fixed_port_conflict_disables_without_rebinding,
     renewal_keeps_the_session_alive_and_lease_expiry_stops_it,
     no_forbidden_data_no_atoms_no_stacktraces,
     logs_never_contain_token_or_runtime_data,
     lsp_application_never_starts_the_inspector,
     journal_reports_request_metadata_only,
     agent_summary_is_compact].

init_per_suite(Config) ->
    ok = mcp_fixture_sup:start_fixture(),
    Mailbox = spawn(fun() -> register(mcp_fx_mailbox, self()), receive never -> ok end end),
    Mailbox ! "SENTINEL_MAILBOX",
    [{mailbox, Mailbox} | Config].

end_per_suite(Config) ->
    exit(?config(mailbox, Config), kill),
    mcp_fixture_sup:stop_fixture(),
    ok.

init_per_testcase(lsp_application_never_starts_the_inspector, Config) -> Config;
init_per_testcase(fixed_port_conflict_disables_without_rebinding, Config) -> Config;
init_per_testcase(renewal_keeps_the_session_alive_and_lease_expiry_stops_it, Config) -> Config;
init_per_testcase(Case, Config) ->
    Cfg = test_config(Case),
    Self = self(),
    Opts = case Case of
               journal_reports_request_metadata_only -> #{notify => fun(M) -> Self ! {journal, M} end};
               _ -> #{}
           end,
    {ok, Info} = mcp_sup:start_session(Cfg, launch, undefined, Opts),
    [{cfg, Cfg}, {info, Info}, {port, maps:get(port, Info)}, {token, maps:get(token, Info)} | Config].

end_per_testcase(_Case, _Config) ->
    catch sys:resume(mcp_fx_sub_sup),
    mcp_sup:stop_session(),
    ok.

test_config(Case) ->
    Base = mcp_policy:defaults(),
    L = maps:get(limits, Base),
    Ets = [<<"mcp_fx_orders">>, <<"mcp_fx_private">>, <<"mcp_fx_missing_table_zzz">>],
    Base1 = Base#{allowed_ets_tables => Ets},
    case Case of
        allowlist_enforced ->
            Base1#{allowed_tools => [<<"runtime_summary">>, <<"ets_tables">>]};
        pagination_cursors_and_retention ->
            Base1#{limits => L#{max_items => 10, max_collections => 2, collection_ttl_ms => 1500}};
        collection_byte_budget_is_partial_and_non_resumable ->
            Base1#{limits => L#{max_result_bytes => 4096, max_collection_bytes => 4096, max_binary_bytes => 512}};
        blocked_supervisor_yields_partial_result ->
            Base1#{limits => L#{request_timeout_ms => 800}};
        concurrency_is_bounded ->
            Base1#{limits => L#{request_timeout_ms => 1500, max_concurrency => 1}};
        _ -> Base1
    end.

%%%%%%%%%%%%%%%%%%%%
%% HTTP client    %%
%%%%%%%%%%%%%%%%%%%%

%% -> {Code, HeadersMap, Body}
http(Port, Method, Headers, Body) ->
    http(Port, Method, "/mcp", Headers, Body, undefined).

http(Port, Method, Path, Headers, Body, ContentLength) ->
    {ok, S} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    Len = case ContentLength of undefined -> byte_size(Body); N -> N end,
    Lines = [[K, ": ", V, "\r\n"] || {K, V} <- Headers],
    LenLine = case Len of skip -> []; _ -> ["Content-Length: ", integer_to_list(Len), "\r\n"] end,
    ok = gen_tcp:send(S, [Method, " ", Path, " HTTP/1.1\r\n", Lines, LenLine, "\r\n", Body]),
    Raw = recv_all(S, <<>>),
    gen_tcp:close(S),
    parse_response(Raw).

recv_all(S, Acc) ->
    case gen_tcp:recv(S, 0, 10000) of
        {ok, D} -> recv_all(S, <<Acc/binary, D/binary>>);
        {error, _} -> Acc
    end.

parse_response(<<>>) -> {0, #{}, <<>>};
parse_response(Raw) ->
    [Head, Body] = case binary:split(Raw, <<"\r\n\r\n">>) of
                       [H, B] -> [H, B];
                       [H] -> [H, <<>>]
                   end,
    [Status | HLines] = binary:split(Head, <<"\r\n">>, [global]),
    [_, Code | _] = binary:split(Status, <<" ">>, [global]),
    Hs = maps:from_list([case binary:split(L, <<": ">>) of
                             [K, V] -> {string:lowercase(K), V};
                             [K] -> {string:lowercase(K), <<>>}
                         end || L <- HLines]),
    {binary_to_integer(Code), Hs, Body}.

auth(Config) -> {"Authorization", ["Bearer ", ?config(token, Config)]}.

std_headers(Config) ->
    Port = ?config(port, Config),
    [{"Host", ["127.0.0.1:", integer_to_list(Port)]}, auth(Config),
     {"Content-Type", "application/json"}].

%% JSON-RPC over the real endpoint -> decoded response map
rpc(Config, Method, Params) ->
    rpc(Config, Method, Params, 1).

rpc(Config, Method, Params, Id) ->
    Req = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"method">> => Method, <<"params">> => Params},
    {ok, Json} = vscode_jsone:encode(Req),
    {200, _, Body} = http(?config(port, Config), "POST", std_headers(Config), iolist_to_binary(Json)),
    decode(Body).

decode(Bin) ->
    {ok, T, <<>>} = vscode_jsone_decode:decode(Bin),
    T.

%% tools/call -> {ok, Structured} | {tool_error, Structured} | {rpc_error, Code, Message}
%% The contract tests check every field, so graph tools are called with detail=full
%% unless the case chooses the detail itself (see agent_summary_is_compact).
call(Config, Tool, Args0) when is_map(Args0) ->
    Graph = [<<"application_overview">>, <<"supervision_tree">>, <<"registered_processes">>,
             <<"process_info">>, <<"ets_tables">>, <<"top_processes">>],
    Args = case lists:member(Tool, Graph) andalso not maps:is_key(<<"detail">>, Args0) of
               true -> Args0#{<<"detail">> => <<"full">>};
               false -> Args0
           end,
    raw_call(Config, Tool, Args);
call(Config, Tool, Args) ->
    raw_call(Config, Tool, Args).

raw_call(Config, Tool, Args) ->
    case rpc(Config, <<"tools/call">>, #{<<"name">> => Tool, <<"arguments">> => Args}) of
        #{<<"result">> := #{<<"isError">> := false, <<"structuredContent">> := S, <<"content">> := [#{<<"text">> := Text}]}} ->
            %% the text content is the same JSON as the structured content
            ?assertEqual(S, decode(Text)),
            {ok, S};
        #{<<"result">> := #{<<"isError">> := true, <<"structuredContent">> := S}} ->
            {tool_error, S};
        #{<<"error">> := #{<<"code">> := C, <<"message">> := M}} ->
            {rpc_error, C, M}
    end.

ok_call(Config, Tool, Args) ->
    {ok, S} = call(Config, Tool, Args),
    S.

%% every page of a paginated tool call
all_pages(Config, Tool, Args) ->
    S = ok_call(Config, Tool, Args),
    collect_pages(Config, Tool, Args, S, [S]).

collect_pages(Config, Tool, Args, #{<<"nextCursor">> := Cursor}, Acc) when is_binary(Cursor) ->
    S = ok_call(Config, Tool, Args#{<<"cursor">> => Cursor}),
    collect_pages(Config, Tool, Args, S, [S | Acc]);
collect_pages(_, _, _, _, Acc) ->
    lists:reverse(Acc).

entities(Pages) -> lists:append([maps:get(<<"entities">>, P) || P <- Pages]).
relationships(Pages) -> lists:append([maps:get(<<"relationships">>, P) || P <- Pages]).

by_name(Name, Ents) ->
    [E || #{<<"name">> := N} = E <- Ents, N =:= Name].

ent(Name, Ents) ->
    [E] = by_name(Name, Ents),
    E.

label(#{<<"childId">> := C}) -> C;
label(#{<<"name">> := N}) when is_binary(N) -> N;
label(#{<<"pid">> := P}) -> P.

id_of(#{<<"id">> := I}) -> I.

fixture_app_id(Config) ->
    S = ok_call(Config, <<"application_overview">>, #{}),
    id_of(ent(<<"mcp_fixture_app">>, maps:get(<<"entities">>, S))).

fixture_root_id(Config) ->
    S = ok_call(Config, <<"application_overview">>, #{}),
    [RootId] = maps:get(<<"roots">>, ent(<<"mcp_fixture_app">>, maps:get(<<"entities">>, S))),
    RootId.

%% {ParentLabel, ChildLabel} of every supervises edge
supervises_edges(Pages) ->
    Ents = entities(Pages),
    Labels = maps:from_list([{id_of(E), label(E)} || E <- Ents]),
    lists:sort([{maps:get(F, Labels), maps:get(T, Labels)}
                || #{<<"type">> := <<"supervises">>, <<"from">> := F, <<"to">> := T} <- relationships(Pages)]).

%%%%%%%%%%%
%% cases %%
%%%%%%%%%%%

unauthenticated_and_bad_tokens_rejected(Config) ->
    Port = ?config(port, Config),
    Tok = ?config(token, Config),
    Body = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>,
    Base = [{"Host", ["127.0.0.1:", integer_to_list(Port)]}, {"Content-Type", "application/json"}],
    {401, H, B} = http(Port, "POST", Base, Body),
    ?assertEqual(<<"Bearer">>, maps:get(<<"www-authenticate">>, H)),
    ?assertEqual(nomatch, binary:match(B, Tok)),
    [?assertMatch({401, _, _}, http(Port, "POST", Base ++ [{"Authorization", V}], Body))
     || V <- ["Bearer wrong", ["Bearer ", Tok, "x"], ["Bearer ", binary:part(Tok, 0, byte_size(Tok) - 1)],
              ["Basic ", Tok], Tok, "Bearer ", ["Bearer  ", Tok]]],
    %% the scheme is case-insensitive, the token is not
    ?assertMatch({200, _, _}, http(Port, "POST", Base ++ [{"Authorization", ["bearer ", Tok]}], Body)),
    ?assertMatch({401, _, _}, http(Port, "POST", Base ++ [{"Authorization", ["Bearer ", string:lowercase(Tok)]}], Body)),
    %% duplicated Authorization header is refused
    ?assertMatch({400, _, _}, http(Port, "POST", Base ++ [auth(Config), auth(Config)], Body)),
    %% no request is processed before authentication: a body that is not JSON is 401, not 400
    ?assertMatch({401, _, _}, http(Port, "POST", Base, <<"not json">>)),
    %% token entropy: at least 32 random bytes
    ?assert(byte_size(Tok) >= 43).

host_and_origin_validated(Config) ->
    Port = ?config(port, Config),
    Body = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>,
    Ok = [auth(Config), {"Content-Type", "application/json"}],
    HostOk = {"Host", ["127.0.0.1:", integer_to_list(Port)]},
    ?assertMatch({200, _, _}, http(Port, "POST", [HostOk | Ok], Body)),
    ?assertMatch({200, _, _}, http(Port, "POST", [{"Host", ["localhost:", integer_to_list(Port)]} | Ok], Body)),
    [?assertMatch({403, _, _}, http(Port, "POST", [{"Host", H} | Ok], Body))
     || H <- ["evil.example.com", "evil.example.com:80", ["127.0.0.1:", integer_to_list(Port + 1)], "127.0.0.1",
              ["0.0.0.0:", integer_to_list(Port)], ["[::1]:", integer_to_list(Port)]]],
    ?assertMatch({403, _, _}, http(Port, "POST", Ok, Body)),
    %% browser-origin attacks: any Origin that is not this loopback endpoint is refused
    [?assertMatch({403, _, _}, http(Port, "POST", [HostOk, {"Origin", O} | Ok], Body))
     || O <- ["http://evil.example.com", "https://127.0.0.1", "null", "http://127.0.0.1:1", "file://"]],
    ?assertMatch({200, _, _}, http(Port, "POST", [HostOk, {"Origin", ["http://127.0.0.1:", integer_to_list(Port)]} | Ok], Body)),
    %% CORS is never enabled
    {200, H, _} = http(Port, "POST", [HostOk | Ok], Body),
    ?assertEqual([], [K || K <- maps:keys(H), binary:match(K, <<"access-control">>) =/= nomatch]).

method_content_type_and_size_enforced(Config) ->
    Port = ?config(port, Config),
    Std = std_headers(Config),
    Body = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>,
    [?assertMatch({405, _, _}, http(Port, M, Std, <<>>)) || M <- ["GET", "PUT", "DELETE", "PATCH", "OPTIONS"]],
    ?assertMatch({404, _, _}, http(Port, "POST", "/other", Std, Body, undefined)),
    ?assertMatch({404, _, _}, http(Port, "POST", "/mcp/", Std, Body, undefined)),
    ?assertMatch({404, _, _}, http(Port, "POST", "/mcp?x=1", Std, Body, undefined)),
    Bad = [{"Host", ["127.0.0.1:", integer_to_list(Port)]}, auth(Config)],
    ?assertMatch({415, _, _}, http(Port, "POST", Bad ++ [{"Content-Type", "text/plain"}], Body)),
    ?assertMatch({415, _, _}, http(Port, "POST", Bad, Body)),
    ?assertMatch({200, _, _}, http(Port, "POST", Bad ++ [{"Content-Type", "application/json; charset=utf-8"}], Body)),
    %% body cap is enforced before the body is read
    ?assertMatch({413, _, _}, http(Port, "POST", "/mcp", Std, <<>>, 16385)),
    ?assertMatch({413, _, _}, http(Port, "POST", "/mcp", Std, <<>>, 999999999)),
    ?assertMatch({400, _, _}, http(Port, "POST", "/mcp", Std, <<>>, -1)),
    ?assertMatch({411, _, _}, http(Port, "POST", "/mcp", Std ++ [{"Transfer-Encoding", "chunked"}], Body, skip)),
    ?assertMatch({411, _, _}, http(Port, "POST", "/mcp", Std, Body, skip)),
    %% malformed payloads
    ?assertMatch({400, _, _}, http(Port, "POST", Std, <<"{not json">>)),
    ?assertMatch({400, _, _}, http(Port, "POST", Std, <<"{\"jsonrpc\":\"2.0\"} trailing">>)),
    {200, _, B1} = http(Port, "POST", Std, <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}]">>),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32600}}, decode(B1)),
    {200, _, B2} = http(Port, "POST", Std, <<"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"ping\"}">>),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32600}}, decode(B2)),
    %% a notification is accepted without a body
    ?assertMatch({202, _, <<>>}, http(Port, "POST", Std, <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}">>)),
    %% slow / truncated body: the server does not hang and answers
    ?assertMatch({400, _, _}, http(Port, "POST", "/mcp", Std, <<"{\"jsonrpc\"">>, 100)).

initialize_negotiates_and_lists_only_readonly_tools(Config) ->
    #{<<"result">> := Init} = rpc(Config, <<"initialize">>, #{<<"protocolVersion">> => <<"2025-06-18">>,
                                                               <<"capabilities">> => #{},
                                                               <<"clientInfo">> => #{<<"name">> => <<"t">>, <<"version">> => <<"1">>}}),
    ?assertEqual(<<"2025-06-18">>, maps:get(<<"protocolVersion">>, Init)),
    ?assertEqual([<<"tools">>], maps:keys(maps:get(<<"capabilities">>, Init))),
    %% an older client version is answered with the implemented revision
    #{<<"result">> := Init2} = rpc(Config, <<"initialize">>, #{<<"protocolVersion">> => <<"2024-11-05">>}),
    ?assertEqual(<<"2025-06-18">>, maps:get(<<"protocolVersion">>, Init2)),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32602}}, rpc(Config, <<"initialize">>, #{})),
    ?assertMatch(#{<<"result">> := #{}}, rpc(Config, <<"ping">>, #{})),
    #{<<"result">> := #{<<"tools">> := Tools}} = rpc(Config, <<"tools/list">>, #{}),
    Names = lists:sort([maps:get(<<"name">>, T) || T <- Tools]),
    ?assertEqual(lists:sort(mcp_policy:all_tools()), Names),
    ?assertEqual(8, length(Tools)),
    [begin
         ?assertMatch(#{<<"inputSchema">> := #{<<"type">> := <<"object">>, <<"additionalProperties">> := false},
                        <<"outputSchema">> := #{<<"type">> := <<"object">>},
                        <<"description">> := _,
                        <<"annotations">> := #{<<"readOnlyHint">> := true, <<"destructiveHint">> := false}}, T),
         ?assert(byte_size(maps:get(<<"description">>, T)) > 40)
     end || T <- Tools],
    %% nothing dangerous is advertised or callable, and no prompts/resources exist
    [?assertNotEqual(nomatch, re:run(N, "^[a-z_]+$")) || N <- Names],
    [?assertEqual(nomatch, re:run(N, "eval|exec|rpc|shell|lookup|trace|kill|load|write|send", [{capture, none}]))
     || N <- Names],
    [?assertMatch(#{<<"error">> := #{<<"code">> := -32601}}, rpc(Config, M, #{}))
     || M <- [<<"resources/list">>, <<"prompts/list">>, <<"resources/read">>, <<"tools/eval">>,
              <<"completion/complete">>, <<"logging/setLevel">>]],
    [?assertMatch({rpc_error, -32602, _}, call(Config, T, #{}))
     || T <- [<<"eval">>, <<"ets_lookup">>, <<"rpc_call">>, <<"sys_get_state">>, <<"shell">>, <<"trace">>,
              <<"kill_process">>, <<"set_breakpoint">>, <<"load_code">>]].

allowlist_enforced(Config) ->
    #{<<"result">> := #{<<"tools">> := Tools}} = rpc(Config, <<"tools/list">>, #{}),
    ?assertEqual([<<"ets_tables">>, <<"runtime_summary">>], lists:sort([maps:get(<<"name">>, T) || T <- Tools])),
    ?assertMatch({ok, _}, call(Config, <<"runtime_summary">>, #{})),
    [?assertMatch({rpc_error, -32602, _}, call(Config, T, #{}))
     || T <- [<<"application_overview">>, <<"supervision_tree">>, <<"registered_processes">>,
              <<"process_info">>, <<"debug_session">>]].

arguments_are_strict(Config) ->
    [?assertMatch({rpc_error, -32602, _}, call(Config, T, A))
     || {T, A} <- [{<<"runtime_summary">>, #{<<"extra">> => 1}},
                   {<<"runtime_summary">>, #{<<"redactNodeHost">> => <<"yes">>}},
                   {<<"application_overview">>, #{<<"application">> => 5}},
                   {<<"application_overview">>, #{<<"application">> => binary:copy(<<"a">>, 65)}},
                   {<<"application_overview">>, #{<<"cursor">> => binary:copy(<<"a">>, 201)}},
                   {<<"application_overview">>, #{<<"includeModules">> => 1}},
                   {<<"supervision_tree">>, #{}},
                   {<<"supervision_tree">>, #{<<"id">> => <<"x">>, <<"supervisor">> => <<"y">>}},
                   {<<"supervision_tree">>, #{<<"id">> => <<"x">>, <<"maxDepth">> => 0}},
                   {<<"supervision_tree">>, #{<<"id">> => <<"x">>, <<"maxDepth">> => 17}},
                   {<<"process_info">>, #{}},
                   {<<"process_info">>, #{<<"name">> => <<"a">>, <<"pid">> => <<"<0.1.0>">>}},
                   {<<"process_info">>, #{<<"name">> => binary:copy(<<"a">>, 256)}},
                   {<<"ets_tables">>, #{<<"table">> => <<"mcp_fx_orders">>}},
                   {<<"debug_session">>, #{<<"anything">> => true}}]],
    %% params must be an object
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32602}},
                 rpc(Config, <<"tools/call">>, #{<<"name">> => <<"ets_tables">>, <<"arguments">> => [1]})).

runtime_summary_redacts_node_host(Config) ->
    S = ok_call(Config, <<"runtime_summary">>, #{}),
    ?assertEqual(true, maps:get(<<"nodeRedacted">>, S)),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"node">>, S), <<"@<redacted>">>)),
    Full = ok_call(Config, <<"runtime_summary">>, #{<<"redactNodeHost">> => false}),
    ?assertEqual(atom_to_binary(node(), utf8), maps:get(<<"node">>, Full)),
    ?assertEqual(list_to_binary(erlang:system_info(otp_release)), maps:get(<<"otpRelease">>, S)),
    ?assertEqual(erlang:system_info(schedulers), maps:get(<<"schedulers">>, S)),
    ?assert(maps:get(<<"processCount">>, S) > 10),
    ?assert(maps:get(<<"uptimeMs">>, S) >= 0),
    ?assert(maps:get(<<"runQueue">>, S) >= 0),
    ?assertEqual(erlang:system_info(schedulers_online), maps:get(<<"schedulersOnline">>, S)),
    #{<<"processes">> := #{<<"count">> := PC, <<"limit">> := PL, <<"usedPercent">> := PP},
      <<"atoms">> := #{<<"count">> := AC, <<"limit">> := AL}} = maps:get(<<"resources">>, S),
    ?assert(PC > 10 andalso PL >= PC andalso PP >= 0 andalso PP < 100),
    ?assert(AC > 0 andalso AL > AC),
    ?assertMatch(#{<<"code">> := C} when is_integer(C), maps:get(<<"memory">>, S)),
    ?assertMatch(#{<<"unit">> := <<"bytes">>, <<"total">> := T} when is_integer(T), maps:get(<<"memory">>, S)),
    %% opaque, non-secret session id shared with the map contract
    Sid = maps:get(<<"sessionId">>, S),
    ?assertEqual(Sid, maps:get(<<"sessionId">>, ok_call(Config, <<"ets_tables">>, #{}))),
    ?assertEqual(Sid, maps:get(sessionId, ?config(info, Config))).

application_overview_discovers_fixture_without_names(Config) ->
    [S] = all_pages(Config, <<"application_overview">>, #{}),
    %% MAP-05 envelope
    [?assert(maps:is_key(K, S)) || K <- [<<"schemaVersion">>, <<"sessionId">>, <<"collectionId">>, <<"startedAt">>,
                                        <<"finishedAt">>, <<"scope">>, <<"entities">>, <<"relationships">>,
                                        <<"complete">>, <<"truncated">>, <<"omissions">>, <<"nextCursor">>]],
    ?assertEqual(<<"1.0">>, maps:get(<<"schemaVersion">>, S)),
    ?assertEqual(null, maps:get(<<"nextCursor">>, S)),
    ?assertEqual(true, maps:get(<<"complete">>, S)),
    ?assertEqual(false, maps:get(<<"truncated">>, S)),
    ?assert(maps:get(<<"startedAt">>, S) =< maps:get(<<"finishedAt">>, S)),
    Ents = maps:get(<<"entities">>, S),
    App = ent(<<"mcp_fixture_app">>, Ents),
    ?assertEqual(<<"application">>, maps:get(<<"kind">>, App)),
    ?assertEqual(<<"9.9.9">>, maps:get(<<"version">>, App)),
    ?assertEqual([<<"kernel">>, <<"stdlib">>], lists:sort(maps:get(<<"dependencies">>, App))),
    ?assertEqual(<<"discovered">>, maps:get(<<"rootStatus">>, App)),
    [RootId] = maps:get(<<"roots">>, App),
    Root = [E || E <- Ents, id_of(E) =:= RootId],
    ?assertMatch([#{<<"name">> := <<"mcp_fx_root">>, <<"role">> := <<"supervisor">>, <<"kind">> := <<"process">>}], Root),
    %% a library application has no start module: explained, not an error
    ?assertEqual(<<"no_start_module">>, maps:get(<<"rootStatus">>, ent(<<"stdlib">>, Ents))),
    ?assertEqual([], maps:get(<<"roots">>, ent(<<"stdlib">>, Ents))),
    %% dependencies are declarations, with evidence and confidence
    Deps = [R || #{<<"type">> := <<"depends_on">>, <<"from">> := F} = R <- maps:get(<<"relationships">>, S),
                 F =:= id_of(App)],
    ?assertEqual(2, length(Deps)),
    [?assertMatch(#{<<"confidence">> := <<"confirmed">>, <<"evidence">> := <<"declared", _/binary>>,
                    <<"observedAt">> := _}, D) || D <- Deps],
    ?assertMatch(#{<<"type">> := <<"belongs_to">>, <<"from">> := RootId, <<"to">> := _},
                 hd([R || #{<<"type">> := <<"belongs_to">>, <<"from">> := F} = R <- maps:get(<<"relationships">>, S), F =:= RootId])),
    %% MAP-04: coverage limitations are explicit
    ?assert(length(maps:get(<<"limitations">>, maps:get(<<"scope">>, S))) >= 3),
    %% selection by discovered id; unknown ids are refused
    Only = ok_call(Config, <<"application_overview">>, #{<<"application">> => id_of(App)}),
    ?assertEqual(<<"discovered">>, maps:get(<<"rootStatus">>, ent(<<"mcp_fixture_app">>, maps:get(<<"entities">>, Only)))),
    ?assertEqual([], by_name(<<"ssl">>, maps:get(<<"entities">>, Only))),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"unknown_or_expired_id">>}}},
                 call(Config, <<"application_overview">>, #{<<"application">> => <<"a_deadbeef">>})),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_id">>}}},
                 call(Config, <<"application_overview">>, #{<<"application">> => RootId})),
    %% modules: names and behaviours only when already loaded, never loaded by the inspector
    M = ok_call(Config, <<"application_overview">>, #{<<"application">> => id_of(App), <<"includeModules">> => true}),
    Mods = [E || #{<<"kind">> := <<"module">>} = E <- maps:get(<<"entities">>, M)],
    ?assertEqual([<<"mcp_fixture_sup">>, <<"mcp_fixture_worker">>], lists:sort([maps:get(<<"name">>, E) || E <- Mods])),
    ?assertEqual([<<"gen_server">>], maps:get(<<"behaviours">>, hd(by_name(<<"mcp_fixture_worker">>, Mods)))),
    ?assert(lists:all(fun(E) -> is_boolean(maps:get(<<"loaded">>, E)) end, Mods)),
    %% no compile path, no environment
    ?assertEqual(nomatch, re:run(iolist_to_binary(io_lib:format("~p", [S])), "SENTINEL|\\.beam|/ebin")).

supervision_tree_matches_independent_expected_graph(Config) ->
    RootId = fixture_root_id(Config),
    Pages = all_pages(Config, <<"supervision_tree">>, #{<<"id">> => RootId}),
    Expected = lists:sort([{<<"mcp_fx_root">>, <<"worker_a">>},
                           {<<"mcp_fx_root">>, <<"mcp_fx_sub_sup">>},
                           {<<"mcp_fx_root">>, <<"mcp_fx_dyn_sup">>},
                           {<<"mcp_fx_sub_sup">>, <<"worker_b">>},
                           {<<"mcp_fx_sub_sup">>, <<"worker_c">>}]),
    ?assertEqual(Expected, supervises_edges(Pages)),
    Ents = entities(Pages),
    ?assertEqual(<<"supervisor">>, maps:get(<<"role">>, ent(<<"mcp_fx_sub_sup">>, [E || E <- Ents, maps:get(<<"childId">>, E, undefined) =:= <<"mcp_fx_sub_sup">>]))),
    Wa = ent(<<"mcp_fx_worker_a">>, Ents),
    ?assertMatch(#{<<"role">> := <<"worker">>, <<"callbackModule">> := <<"mcp_fixture_worker">>,
                   <<"modules">> := [<<"mcp_fixture_worker">>], <<"behaviour">> := [<<"gen_server">>],
                   <<"restart">> := <<"permanent">>, <<"shutdown">> := 1000, <<"specAvailable">> := true}, Wa),
    Wb = hd([E || #{<<"childId">> := <<"worker_b">>} = E <- Ents]),
    ?assertMatch(#{<<"role">> := <<"worker">>, <<"restart">> := <<"transient">>, <<"name">> := null}, Wb),
    %% typed edges carry evidence, observation time and confidence
    [?assertMatch(#{<<"evidence">> := <<"supervisor:which_children/1">>, <<"confidence">> := <<"confirmed">>,
                    <<"observedAt">> := _}, R)
     || #{<<"type">> := <<"supervises">>} = R <- relationships(Pages)],
    %% never start arguments, dictionary, state
    ?assertEqual(nomatch, re:run(iolist_to_binary(io_lib:format("~p", [Pages])), "SENTINEL")),
    ?assertNot(lists:any(fun(E) -> maps:is_key(<<"start">>, E) orelse maps:is_key(<<"args">>, E) end, Ents)),
    %% no link/monitor edge in a supervision map, and complete
    ?assertEqual([], [T || #{<<"type">> := T} <- relationships(Pages),
                           T =:= <<"linked_to">> orelse T =:= <<"monitors">>]),
    [?assertEqual(true, maps:get(<<"complete">>, P)) || P <- Pages],
    %% starting from an application id reaches the same tree; depth is bounded and explained
    AppId = fixture_app_id(Config),
    ?assertEqual(Expected, supervises_edges(all_pages(Config, <<"supervision_tree">>, #{<<"id">> => AppId}))),
    Shallow = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => RootId, <<"maxDepth">> => 1}),
    ?assertEqual(lists:sort([{<<"mcp_fx_root">>, <<"worker_a">>}, {<<"mcp_fx_root">>, <<"mcp_fx_dyn_sup">>},
                             {<<"mcp_fx_root">>, <<"mcp_fx_sub_sup">>}]), supervises_edges([Shallow])),
    ?assertEqual(false, maps:get(<<"complete">>, Shallow)),
    ?assertEqual(true, maps:get(<<"truncated">>, Shallow)),
    Om = [O || #{<<"reason">> := <<"limit_reached">>} = O <- maps:get(<<"omissions">>, Shallow)],
    ?assertEqual(2, length(Om)),
    %% progressive expansion: the omission's scope is a usable id
    SubId = maps:get(<<"scope">>, hd(Om)),
    Sub = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => SubId, <<"maxDepth">> => 1}),
    ?assert(length(supervises_edges([Sub])) =:= 2 orelse length(supervises_edges([Sub])) =:= 0),
    %% an explicitly supplied local supervisor
    Named = ok_call(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"mcp_fx_sub_sup">>}),
    ?assertEqual([{<<"mcp_fx_sub_sup">>, <<"worker_b">>}, {<<"mcp_fx_sub_sup">>, <<"worker_c">>}], supervises_edges([Named])),
    %% a plain worker is not treated as a supervisor (never called with supervisor APIs)
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"not_a_supervisor">>}}},
                 call(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"mcp_fx_worker_a">>})),
    ?assert(is_process_alive(whereis(mcp_fx_worker_a))),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"not_found">>}}},
                 call(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"no_such_registered_name_zz">>})),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"not_a_supervisor">>}}},
                 call(Config, <<"supervision_tree">>, #{<<"id">> => id_of(hd([E || #{<<"kind">> := <<"process">>, <<"role">> := <<"worker">>} = E <- Ents]))})).

linked_unsupervised_process_is_not_supervised(Config) ->
    Orphan = whereis(mcp_fx_orphan),
    ?assert(is_pid(Orphan)),
    Pages = all_pages(Config, <<"supervision_tree">>, #{<<"id">> => fixture_root_id(Config)}),
    ?assertEqual([], by_name(<<"mcp_fx_orphan">>, entities(Pages))),
    P = ok_call(Config, <<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_a">>}),
    Ents = maps:get(<<"entities">>, P),
    Wa = ent(<<"mcp_fx_worker_a">>, Ents),
    Or = ent(<<"mcp_fx_orphan">>, Ents),
    Links = [R || #{<<"type">> := <<"linked_to">>} = R <- maps:get(<<"relationships">>, P)],
    ?assert(lists:any(fun(#{<<"from">> := F, <<"to">> := T}) -> F =:= id_of(Wa) andalso T =:= id_of(Or) end, Links)),
    [?assertMatch(#{<<"confidence">> := <<"confirmed">>, <<"evidence">> := <<"process_info links">>}, L) || L <- Links],
    %% a link is never a supervision edge
    ?assertEqual([], [R || #{<<"type">> := <<"supervises">>} = R <- maps:get(<<"relationships">>, P)]).

process_info_inputs_are_safe(Config) ->
    P = ok_call(Config, <<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_c">>}),
    Wc = ent(<<"mcp_fx_worker_c">>, maps:get(<<"entities">>, P)),
    Allowed = [<<"id">>, <<"kind">>, <<"pid">>, <<"name">>, <<"alive">>, <<"role">>, <<"status">>,
               <<"currentFunction">>, <<"initialCall">>, <<"reductions">>, <<"memory">>, <<"memoryUnit">>,
               <<"messageQueueLen">>, <<"application">>, <<"membership">>, <<"callbackModule">>, <<"behaviour">>],
    ?assertEqual([], maps:keys(Wc) -- Allowed),
    ?assertMatch(#{<<"status">> := _, <<"reductions">> := R, <<"memory">> := M, <<"messageQueueLen">> := 0,
                   <<"memoryUnit">> := <<"bytes">>, <<"initialCall">> := _, <<"currentFunction">> := _}
                   when is_integer(R) andalso is_integer(M), Wc),
    ?assertMatch(#{<<"confidence">> := _, <<"evidence">> := _}, maps:get(<<"membership">>, Wc)),
    %% callback/behaviour are unknown unless safe metadata is available (no dictionary, no sys:get_state)
    ?assertEqual(<<"unknown">>, maps:get(<<"callbackModule">>, Wc)),
    %% same process by id and by pid text
    Id = id_of(Wc),
    ?assertEqual(Id, id_of(ent(<<"mcp_fx_worker_c">>, maps:get(<<"entities">>, ok_call(Config, <<"process_info">>, #{<<"id">> => Id}))))),
    PidText = maps:get(<<"pid">>, Wc),
    ?assertEqual(Id, id_of(ent(<<"mcp_fx_worker_c">>, maps:get(<<"entities">>, ok_call(Config, <<"process_info">>, #{<<"pid">> => PidText}))))),
    %% hostile identifiers: nothing is created, nothing crosses the local node
    Atoms = erlang:system_info(atom_count),
    Hostile = [<<"<1.2.3>">>, <<"<0.99999999999.0>">>, <<"<0.1.0>x">>, <<"<0.1.0> ">>, <<"<0.0.0.0>">>,
               <<"#Port<0.1>">>, <<"<0.1>">>, <<"<node@host.1.2>">>],
    [?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := C}}} when C =:= <<"invalid_pid">> orelse C =:= <<"not_found">>,
                  call(Config, <<"process_info">>, #{<<"pid">> => H})) || H <- Hostile],
    [?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"not_found">>}}},
                  call(Config, <<"process_info">>, #{<<"name">> => <<"unique_hostile_name_", (integer_to_binary(I))/binary>>}))
     || I <- lists:seq(1, 50)],
    ?assertEqual(Atoms, erlang:system_info(atom_count)),
    %% an exited process is reported, not invented
    Gone = spawn(fun() -> ok end),
    timer:sleep(50),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"not_found">>}}},
                 call(Config, <<"process_info">>, #{<<"pid">> => list_to_binary(pid_to_list(Gone))})),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"unknown_or_expired_id">>}}},
                 call(Config, <<"process_info">>, #{<<"id">> => <<"p_0000000000000000">>})),
    %% an application id is not a process id
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_id">>}}},
                 call(Config, <<"process_info">>, #{<<"id">> => fixture_app_id(Config)})).

registered_processes_membership_evidence(Config) ->
    AppId = fixture_app_id(Config),
    Pages = all_pages(Config, <<"registered_processes">>, #{<<"application">> => AppId}),
    Ents = entities(Pages),
    Names = lists:sort([N || #{<<"name">> := N} <- Ents, is_binary(N)]),
    ?assertEqual([<<"mcp_fx_dyn_sup">>, <<"mcp_fx_orphan">>, <<"mcp_fx_root">>, <<"mcp_fx_sub_sup">>,
                  <<"mcp_fx_worker_a">>, <<"mcp_fx_worker_c">>], Names),
    Conf = fun(N) -> maps:get(<<"confidence">>, maps:get(<<"membership">>, ent(N, Ents))) end,
    %% found in the supervision tree: confirmed
    [?assertEqual(<<"confirmed">>, Conf(N)) || N <- [<<"mcp_fx_root">>, <<"mcp_fx_sub_sup">>,
                                                    <<"mcp_fx_worker_a">>, <<"mcp_fx_worker_c">>, <<"mcp_fx_dyn_sup">>]],
    %% the unsupervised orphan shares the group leader only: never "confirmed"
    ?assertEqual(<<"inferred">>, Conf(<<"mcp_fx_orphan">>)),
    %% unrelated registered names are not silently assigned to the application
    ?assertEqual([], by_name(<<"kernel_sup">>, Ents)),
    ?assertEqual([], by_name(<<"mcp_fx_mailbox">>, Ents)),
    ?assert(lists:any(fun(#{<<"reason">> := R}) -> R =:= <<"unknown_membership">> end,
                      lists:append([maps:get(<<"omissions">>, P) || P <- Pages]))),
    [?assertEqual(false, maps:get(<<"complete">>, P)) || P <- Pages],
    %% without a filter: bounded, membership stated per entry, coverage limitations explicit
    All = all_pages(Config, <<"registered_processes">>, #{}),
    AllEnts = entities(All),
    ?assert(length(by_name(<<"kernel_sup">>, AllEnts)) =:= 1),
    ?assertEqual(<<"unknown">>, maps:get(<<"confidence">>, maps:get(<<"membership">>, ent(<<"mcp_fx_mailbox">>, AllEnts)))),
    %% inspector processes are never listed as application processes
    ?assertEqual([], [N || #{<<"name">> := N} <- AllEnts, is_binary(N), binary:match(N, <<"mcp_server">>) =/= nomatch
                                orelse binary:match(N, <<"mcp_store">>) =/= nomatch orelse binary:match(N, <<"mcp_holder">>) =/= nomatch
                                orelse binary:match(N, <<"mcp_sup">>) =/= nomatch]),
    ?assert(length(maps:get(<<"limitations">>, maps:get(<<"scope">>, hd(All)))) >= 3),
    %% mailbox contents are never read: only the queue length
    MB = ent(<<"mcp_fx_mailbox">>, AllEnts),
    ?assertEqual(1, maps:get(<<"messageQueueLen">>, MB)),
    ?assertEqual(nomatch, re:run(iolist_to_binary(io_lib:format("~p", [All])), "SENTINEL")).

ets_tables_metadata_only_for_approved_tables(Config) ->
    S = ok_call(Config, <<"ets_tables">>, #{}),
    Ents = maps:get(<<"entities">>, S),
    Tabs = [E || #{<<"kind">> := <<"ets_table">>} = E <- Ents],
    %% only the approved, non-private, existing table
    ?assertMatch([#{<<"name">> := <<"mcp_fx_orders">>, <<"protection">> := <<"public">>, <<"type">> := <<"set">>,
                    <<"size">> := 1, <<"memoryUnit">> := <<"words">>}], Tabs),
    [Tab] = Tabs,
    ?assert(is_integer(maps:get(<<"memory">>, Tab))),
    ?assertEqual(maps:get(<<"memory">>, Tab) * erlang:system_info(wordsize), maps:get(<<"memoryBytes">>, Tab)),
    %% owner correlation: joinable with process entities via an owns_table edge
    Owner = ent(<<"mcp_fx_worker_c">>, Ents),
    ?assertEqual(id_of(Owner), maps:get(<<"owner">>, Tab)),
    ?assertMatch([#{<<"from">> := _, <<"to">> := _, <<"evidence">> := <<"ets:info owner">>}],
                 [R || #{<<"type">> := <<"owns_table">>} = R <- maps:get(<<"relationships">>, S)]),
    %% private and missing approved tables are counted without disclosing names
    Reasons = lists:sort([{maps:get(<<"reason">>, O), maps:get(<<"count">>, O)} || O <- maps:get(<<"omissions">>, S)]),
    ?assertEqual([{<<"disappeared">>, 1}, {<<"policy_denied">>, 1}], Reasons),
    Text = iolist_to_binary(io_lib:format("~p", [maps:get(<<"omissions">>, S)])),
    ?assertEqual(nomatch, binary:match(Text, <<"mcp_fx_private">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"zzz">>)),
    %% no node-wide inventory: an unapproved table is invisible
    ?assertEqual([], by_name(<<"ac_tab">>, Ents)),
    %% keys/objects/values are never read
    ?assertEqual(nomatch, re:run(iolist_to_binary(io_lib:format("~p", [S])), "SENTINEL")),
    ?assertEqual(false, maps:get(<<"complete">>, S)),
    %% a recreated table has a different identity even with the same name
    OldId = id_of(Tab),
    ets:delete(mcp_fx_orders),
    Owner2 = spawn(fun() -> ets:new(mcp_fx_orders, [named_table, public, set]), receive stop -> ok end end),
    timer:sleep(100),
    S2 = ok_call(Config, <<"ets_tables">>, #{}),
    [Tab2] = [E || #{<<"kind">> := <<"ets_table">>} = E <- maps:get(<<"entities">>, S2)],
    ?assertNotEqual(OldId, id_of(Tab2)),
    ?assertEqual(0, maps:get(<<"size">>, Tab2)),
    exit(Owner2, kill),
    %% a disappeared table is reported, not invented
    timer:sleep(50),
    S3 = ok_call(Config, <<"ets_tables">>, #{}),
    ?assertEqual([], [E || #{<<"kind">> := <<"ets_table">>} = E <- maps:get(<<"entities">>, S3)]),
    ?assertEqual(2, lists:sum([maps:get(<<"count">>, O) || #{<<"reason">> := <<"disappeared">>} = O <- maps:get(<<"omissions">>, S3)])),
    %% restore the fixture table (its owner is restarted by its supervisor)
    OldOwner = whereis(mcp_fx_worker_c),
    exit(OldOwner, kill),
    wait_until(fun() -> is_pid(whereis(mcp_fx_worker_c)) andalso whereis(mcp_fx_worker_c) =/= OldOwner end),
    wait_until(fun() -> ets:info(mcp_fx_orders, size) =:= 1 end).

restart_changes_identity(Config) ->
    RootId = fixture_root_id(Config),
    Before = entities(all_pages(Config, <<"supervision_tree">>, #{<<"id">> => RootId})),
    Old = hd([E || #{<<"childId">> := <<"worker_c">>} = E <- Before]),
    OldPid = whereis(mcp_fx_worker_c),
    exit(OldPid, kill),
    wait_until(fun() -> is_pid(whereis(mcp_fx_worker_c)) andalso whereis(mcp_fx_worker_c) =/= OldPid end),
    After = entities(all_pages(Config, <<"supervision_tree">>, #{<<"id">> => RootId})),
    New = hd([E || #{<<"childId">> := <<"worker_c">>} = E <- After]),
    ?assertNotEqual(id_of(Old), id_of(New)),
    %% unchanged entities keep their id across collections (join across tools)
    ?assertEqual(id_of(hd([E || #{<<"childId">> := <<"worker_a">>} = E <- Before])),
                 id_of(hd([E || #{<<"childId">> := <<"worker_a">>} = E <- After]))),
    %% the previous identity is stale, never resolved to the new process
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"not_found">>}}},
                 call(Config, <<"process_info">>, #{<<"id">> => id_of(Old)})),
    %% absence of the old id from a later collection does not mean anything else was invented
    ?assertEqual(supervises_edges([#{<<"entities">> => Before, <<"relationships">> => []}]), []),
    %% a supervisor entry that is restarting/absent is preserved without a live pid: terminate_child keeps the spec
    ok = supervisor:terminate_child(mcp_fx_sub_sup, worker_b),
    Slots = entities(all_pages(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"mcp_fx_sub_sup">>})),
    Wb = hd([E || #{<<"childId">> := <<"worker_b">>} = E <- Slots]),
    ?assertMatch(#{<<"alive">> := false, <<"pid">> := null, <<"childState">> := <<"not_running">>}, Wb),
    {ok, _} = supervisor:restart_child(mcp_fx_sub_sup, worker_b).

pagination_cursors_and_retention(Config) ->
    ok = mcp_fixture_sup:add_dynamic(30),
    RootId = fixture_root_id(Config),
    Args = #{<<"id">> => RootId},
    First = ok_call(Config, <<"supervision_tree">>, Args),
    %% max_items counts entities and relationships together per page
    ?assert(length(maps:get(<<"entities">>, First)) + length(maps:get(<<"relationships">>, First)) =< 10),
    Cursor = maps:get(<<"nextCursor">>, First),
    ?assert(is_binary(Cursor)),
    Pages = collect_pages(Config, <<"supervision_tree">>, Args, First, [First]),
    ?assert(length(Pages) > 5),
    [?assert(length(maps:get(<<"entities">>, P)) + length(maps:get(<<"relationships">>, P)) =< 10) || P <- Pages],
    %% every page belongs to the same retained collection: no restart, no duplicates
    ?assertEqual(1, length(lists:usort([maps:get(<<"collectionId">>, P) || P <- Pages]))),
    Ids = [id_of(E) || E <- entities(Pages)],
    ?assertEqual(length(Ids), length(lists:usort(Ids))),
    %% 1 root + 3 children + 2 (sub) + 30 dynamic workers, 35 supervises edges
    ?assertEqual(36, length(Ids)),
    ?assertEqual(35, length(supervises_edges(Pages))),
    %% each page is bounded, the last has no cursor
    ?assertEqual(null, maps:get(<<"nextCursor">>, lists:last(Pages))),
    %% cross-page edges reference safe ids, flagged as outside the page
    ?assert(lists:any(fun(#{<<"fromInPage">> := F, <<"toInPage">> := T}) -> not (F andalso T) end, relationships(Pages))),
    %% cursors are bound to tool, arguments and session
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_cursor">>}}},
                 call(Config, <<"supervision_tree">>, #{<<"id">> => fixture_app_id(Config), <<"cursor">> => Cursor})),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_cursor">>}}},
                 call(Config, <<"registered_processes">>, #{<<"cursor">> => Cursor})),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_cursor">>}}},
                 call(Config, <<"supervision_tree">>, Args#{<<"maxDepth">> => 3, <<"cursor">> => Cursor})),
    [?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_cursor">>}}},
                  call(Config, <<"supervision_tree">>, Args#{<<"cursor">> => Bad}))
     || Bad <- [<<"c1.x.0.deadbeef">>, <<"garbage">>, <<>>, <<"c1..0.">>, <<"c1.c_0000.-1.aa">>,
                flip_last(Cursor)]],
    %% retention: the count budget evicts the oldest collection (max_collections = 2)
    _ = ok_call(Config, <<"registered_processes">>, #{}),
    _ = ok_call(Config, <<"ets_tables">>, #{}),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"cursor_expired">>}}},
                 call(Config, <<"supervision_tree">>, Args#{<<"cursor">> => Cursor})),
    %% retention: TTL (1.5 s) - an expired collection is never silently restarted
    Fresh = ok_call(Config, <<"supervision_tree">>, Args),
    Cursor2 = maps:get(<<"nextCursor">>, Fresh),
    ?assertMatch({ok, _}, call(Config, <<"supervision_tree">>, Args#{<<"cursor">> => Cursor2})),
    timer:sleep(1800),
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"cursor_expired">>}}},
                 call(Config, <<"supervision_tree">>, Args#{<<"cursor">> => Cursor2})),
    %% bounded identity bookkeeping and retained state
    Stats = mcp_store:stats(),
    ?assert(maps:get(collections, Stats) =< 2),
    wait_until(fun() -> maps:get(slots, mcp_store:stats()) =:= 0 end),
    lists:foreach(fun({_, P, _, _}) -> supervisor:terminate_child(mcp_fx_dyn_sup, P) end,
                  supervisor:which_children(mcp_fx_dyn_sup)).

collection_byte_budget_is_partial_and_non_resumable(Config) ->
    ok = mcp_fixture_sup:add_dynamic(60),
    Args = #{<<"supervisor">> => <<"mcp_fx_root">>},
    S = ok_call(Config, <<"supervision_tree">>, Args),
    ?assertEqual(false, maps:get(<<"complete">>, S)),
    ?assertEqual(true, maps:get(<<"truncated">>, S)),
    [Om | _] = [O || #{<<"reason">> := <<"limit_reached">>, <<"resumable">> := false} = O <- maps:get(<<"omissions">>, S)],
    ?assert(maps:get(<<"count">>, Om) > 0),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"message">>, Om), <<"narrow">>)),
    %% pages of the truncated collection still terminate and never claim completeness
    Pages = collect_pages(Config, <<"supervision_tree">>, Args, S, [S]),
    [?assertEqual(false, maps:get(<<"complete">>, P)) || P <- Pages],
    ?assertEqual(null, maps:get(<<"nextCursor">>, lists:last(Pages))),
    ?assert(maps:get(collection_bytes, mcp_store:stats()) =< 2 * 4096),
    %% the whole response envelope stays under max_result_bytes
    {200, _, Raw} = http(?config(port, Config), "POST", std_headers(Config),
                         iolist_to_binary(element(2, vscode_jsone:encode(
                             #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 9, <<"method">> => <<"tools/call">>,
                               <<"params">> => #{<<"name">> => <<"supervision_tree">>,
                                                 <<"arguments">> => Args}})))),
    ?assert(byte_size(Raw) =< 4096),
    lists:foreach(fun({_, P, _, _}) -> supervisor:terminate_child(mcp_fx_dyn_sup, P) end,
                  supervisor:which_children(mcp_fx_dyn_sup)).

blocked_supervisor_yields_partial_result(Config) ->
    RootId = fixture_root_id(Config),
    ok = sys:suspend(mcp_fx_sub_sup),
    T0 = erlang:monotonic_time(millisecond),
    S = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => RootId}),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    ?assert(Elapsed < 3000),
    ?assertEqual(false, maps:get(<<"complete">>, S)),
    [Om] = [O || #{<<"reason">> := <<"timeout">>} = O <- maps:get(<<"omissions">>, S)],
    %% no "paused" claim without debugger evidence
    ?assertEqual([], [O || #{<<"reason">> := <<"unavailable_while_paused">>} = O <- maps:get(<<"omissions">>, S)]),
    Ents = maps:get(<<"entities">>, S),
    SubId = maps:get(<<"scope">>, Om),
    ?assertEqual(<<"mcp_fx_sub_sup">>, maps:get(<<"childId">>, hd([E || E <- Ents, id_of(E) =:= SubId]))),
    %% the healthy branches are still mapped, the blocked subtree is missing rather than invented
    Edges = supervises_edges([S]),
    ?assert(lists:member({<<"mcp_fx_root">>, <<"worker_a">>}, Edges)),
    ?assertEqual([], [E || {<<"mcp_fx_sub_sup">>, _} = E <- Edges]),
    %% the inspector resumed nothing and altered nothing
    {status, _, _, [_, SysState | _]} = sys:get_status(mcp_fx_sub_sup),
    ?assertEqual(suspended, SysState),
    ok = sys:resume(mcp_fx_sub_sup),
    %% inspector-owned workers and slots are back to baseline
    wait_until(fun() -> maps:get(slots, mcp_store:stats()) =:= 0 end),
    S2 = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => RootId}),
    ?assertEqual(true, maps:get(<<"complete">>, S2)),
    ?assertEqual(5, length(supervises_edges([S2]))).

concurrency_is_bounded(Config) ->
    RootId = fixture_root_id(Config),
    ok = sys:suspend(mcp_fx_sub_sup),
    Self = self(),
    spawn(fun() -> Self ! {first, call(Config, <<"supervision_tree">>, #{<<"id">> => RootId})} end),
    wait_until(fun() -> maps:get(slots, mcp_store:stats()) =:= 1 end),
    %% excess work is rejected with a clear MCP error, not queued
    ?assertMatch({rpc_error, -32000, <<"inspector busy", _/binary>>}, call(Config, <<"runtime_summary">>, #{})),
    receive {first, R} -> ?assertMatch({ok, _}, R) after 6000 -> ct:fail(no_first_result) end,
    ok = sys:resume(mcp_fx_sub_sup),
    wait_until(fun() -> maps:get(slots, mcp_store:stats()) =:= 0 end),
    ?assertMatch({ok, _}, call(Config, <<"runtime_summary">>, #{})).

stop_closes_endpoint_and_invalidates_credentials(Config) ->
    Port = ?config(port, Config),
    Tok = ?config(token, Config),
    Cursor = maps:get(<<"nextCursor">>, ok_call_small(Config)),
    ?assert(is_binary(Cursor)),
    ?assertMatch({200, _, _}, http(Port, "POST", std_headers(Config), <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>)),
    ok = mcp_sup:stop_session(),
    ?assertNot(mcp_sup:active()),
    ?assertEqual(undefined, whereis(mcp_server)),
    ?assertEqual(undefined, whereis(mcp_store)),
    ?assertEqual(undefined, whereis(mcp_sup)),
    ?assertMatch({error, econnrefused}, gen_tcp:connect({127, 0, 0, 1}, Port, [binary], 1000)),
    %% a new session has a fresh token and identity; old credentials and cursors are worthless
    {ok, Info2} = mcp_sup:start_session(?config(cfg, Config), launch),
    ?assertNotEqual(Tok, maps:get(token, Info2)),
    ?assertNotEqual(maps:get(sessionId, ?config(info, Config)), maps:get(sessionId, Info2)),
    Cfg2 = [{port, maps:get(port, Info2)}, {token, Tok}],
    ?assertMatch({401, _, _}, http(maps:get(port, Info2), "POST", std_headers(Cfg2), <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>)),
    New = [{port, maps:get(port, Info2)}, {token, maps:get(token, Info2)}],
    ?assertMatch({tool_error, #{<<"error">> := #{<<"code">> := <<"invalid_cursor">>}}},
                 call(New, <<"application_overview">>, #{<<"cursor">> => Cursor})),
    %% concurrent start is refused: one inspector per node
    ?assertMatch({error, <<"an MCP inspector is already running", _/binary>>}, mcp_sup:start_session(?config(cfg, Config), launch)),
    ok = mcp_sup:stop_session().

ok_call_small(Config) ->
    %% a call whose result spans several pages under the default limits: many registered names
    Names = [list_to_atom("mcp_fx_pad_" ++ integer_to_list(I)) || I <- lists:seq(1, 600)],
    Pids = [begin P = spawn(fun() -> receive stop -> ok end end), register(N, P), P end || N <- Names],
    S = ok_call(Config, <<"registered_processes">>, #{}),
    [exit(P, kill) || P <- Pids],
    S.

fixed_port_conflict_disables_without_rebinding(_Config) ->
    {ok, Busy} = gen_tcp:listen(0, [binary, {ip, {127, 0, 0, 1}}, {reuseaddr, false}]),
    {ok, Port} = inet:port(Busy),
    Cfg = (mcp_policy:defaults())#{port => Port},
    {error, Msg} = mcp_sup:start_session(Cfg, launch),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"already in use">>)),
    %% nothing partially initialized is left running, no other port was chosen
    ?assertNot(mcp_sup:active()),
    [?assertEqual(undefined, whereis(N)) || N <- [mcp_holder, mcp_sup, mcp_server, mcp_store]],
    gen_tcp:close(Busy),
    %% once the port is free the same fixed port can be used, and it is really that port
    {ok, Info} = mcp_sup:start_session(Cfg, launch),
    ?assertEqual(Port, maps:get(port, Info)),
    ok = mcp_sup:stop_session().

renewal_keeps_the_session_alive_and_lease_expiry_stops_it(_Config) ->
    {ok, Info} = mcp_sup:start_session(mcp_policy:defaults(), launch),
    Port = maps:get(port, Info),
    %% renewals (as the adapter sends them) keep the session beyond the lease duration
    [begin timer:sleep(2000), ok = mcp_sup:renew() end || _ <- lists:seq(1, 6)],
    ?assert(mcp_sup:active()),
    ?assertMatch({ok, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [binary], 1000)),
    %% without renewal the inspector stops within the documented bound (10 s)
    T0 = erlang:monotonic_time(millisecond),
    wait_until(fun() -> not mcp_sup:active() end, 12000),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    ct:pal("lease expired after ~p ms without renewal", [Elapsed]),
    ?assert(Elapsed =< 10000),
    ?assertMatch({error, econnrefused}, gen_tcp:connect({127, 0, 0, 1}, Port, [binary], 1000)),
    [?assertEqual(undefined, whereis(N)) || N <- [mcp_holder, mcp_sup, mcp_server, mcp_store]],
    %% the application node itself is untouched
    ?assert(is_pid(whereis(mcp_fx_root))).

no_forbidden_data_no_atoms_no_stacktraces(Config) ->
    %% warm up runtime dependencies (and the atoms they need), then measure
    RootId = fixture_root_id(Config),
    AppId = fixture_app_id(Config),
    Calls = [{<<"runtime_summary">>, #{}}, {<<"debug_session">>, #{}},
             {<<"application_overview">>, #{<<"includeModules">> => true}},
             {<<"supervision_tree">>, #{<<"id">> => RootId, <<"includeModules">> => true}},
             {<<"registered_processes">>, #{}}, {<<"registered_processes">>, #{<<"application">> => AppId}},
             {<<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_a">>}},
             {<<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_c">>}},
             {<<"ets_tables">>, #{}}],
    Results = [call(Config, T, A) || {T, A} <- Calls],
    Atoms = erlang:system_info(atom_count),
    %% hostile input, repeated with unique strings
    Hostile = lists:flatten(
                [[call(Config, <<"process_info">>, #{<<"name">> => <<"hostile_", (integer_to_binary(I))/binary>>}),
                  call(Config, <<"process_info">>, #{<<"pid">> => <<"<0.", (integer_to_binary(I + 100000))/binary, ".0>">>}),
                  call(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"sup_", (integer_to_binary(I))/binary>>}),
                  call(Config, <<"application_overview">>, #{<<"application">> => <<"a_", (integer_to_binary(I))/binary>>}),
                  call(Config, list_to_binary("no_such_tool_" ++ integer_to_list(I)), #{})]
                 || I <- lists:seq(1, 60)]),
    _ = [http(?config(port, Config), "POST", std_headers(Config), <<"{\"a\":", (integer_to_binary(I))/binary>>) || I <- lists:seq(1, 20)],
    _ = [http(?config(port, Config), "POST", [{"Host", ["h", integer_to_list(I)]}], <<>>) || I <- lists:seq(1, 20)],
    %% >340 unique hostile inputs: allow only unrelated background noise
    ?assert(erlang:system_info(atom_count) - Atoms < 20),
    Text = iolist_to_binary(io_lib:format("~p", [Results ++ Hostile])),
    %% forbidden data (state, dictionary, mailbox, ETS values, start args, env) is never returned
    ?assertEqual(nomatch, binary:match(Text, <<"SENTINEL">>)),
    %% no stack traces, paths or internal terms in errors
    [?assertEqual(nomatch, re:run(Text, P, [{capture, none}]))
     || P <- ["badmatch", "function_clause", "case_clause", "\\.erl", "/Users/", "/home/", "stacktrace"]],
    %% every advertised tool answered with a well-formed result
    [?assertMatch({ok, _}, R) || R <- Results].

logs_never_contain_token_or_runtime_data(Config) ->
    Self = self(),
    HandlerId = mcp_capture_handler,
    ok = logger:add_handler(HandlerId, mcp_capture_logger, #{config => #{owner => Self}, level => all}),
    PrevLevel = maps:get(level, logger:get_primary_config()),
    ok = logger:set_primary_config(level, all),
    try
        Tok = ?config(token, Config),
        _ = call(Config, <<"runtime_summary">>, #{}),
        _ = call(Config, <<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_a">>}),
        _ = call(Config, <<"application_overview">>, #{}),
        _ = http(?config(port, Config), "POST", [{"Host", ["127.0.0.1:", integer_to_list(?config(port, Config))]},
                                                 {"Authorization", "Bearer wrongtoken_SENTINEL_BAD"}], <<"{}">>),
        _ = http(?config(port, Config), "POST", [{"Host", "evil"}], <<"{}">>),
        timer:sleep(200),
        Logged = collect_logs([]),
        ct:pal("captured ~p log events", [length(Logged)]),
        Text = iolist_to_binary(io_lib:format("~p", [Logged])),
        ?assertNotEqual([], Logged),
        ?assertEqual(nomatch, binary:match(Text, Tok)),
        ?assertEqual(nomatch, binary:match(Text, <<"wrongtoken">>)),
        ?assertEqual(nomatch, binary:match(Text, <<"SENTINEL">>)),
        ?assertEqual(nomatch, binary:match(Text, <<"mcp_fx_worker_a">>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"tool_call">>))
    after
        logger:remove_handler(HandlerId),
        logger:set_primary_config(level, PrevLevel)
    end.

collect_logs(Acc) ->
    receive {mcp_log, E} -> collect_logs([E | Acc]) after 100 -> lists:reverse(Acc) end.

lsp_application_never_starts_the_inspector(_Config) ->
    %% enabling nothing: the LSP application (and its settings) never start MCP
    ?assertNot(mcp_sup:active()),
    ok = application:start(vscode_lsp, permanent),
    try
        ?assertNot(mcp_sup:active()),
        [?assertEqual(undefined, whereis(N)) || N <- [mcp_holder, mcp_sup, mcp_server, mcp_store]],
        %% the LSP application supervisor knows no MCP child
        Kids = supervisor:which_children(vscode_lsp_app_sup),
        ?assertEqual([], [K || {Id, _, _, _} = K <- Kids, lists:prefix("mcp_", lists:flatten(io_lib:format("~w", [Id])))]),
        %% the entry module's hot-loaded module list does not include MCP
        {ok, Beam} = file:read_file(code:where_is_file("vscode_lsp_entry.beam")),
        ?assertEqual(nomatch, binary:match(Beam, <<"mcp_sup">>))
    after
        application:stop(vscode_lsp)
    end.

flip_last(Bin) ->
    Size = byte_size(Bin) - 1,
    <<Head:Size/binary, Last>> = Bin,
    New = case Last of $a -> $b; _ -> $a end,
    <<Head/binary, New>>.

journal_reports_request_metadata_only(Config) ->
    Port = ?config(port, Config),
    Tok = ?config(token, Config),
    #{<<"result">> := _} = rpc(Config, <<"initialize">>, #{<<"protocolVersion">> => <<"2025-06-18">>}),
    #{<<"result">> := _} = rpc(Config, <<"tools/list">>, #{}),
    ok = mcp_fixture_sup:add_dynamic(5),
    First = ok_call(Config, <<"registered_processes">>, #{}),
    _ = call(Config, <<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_a">>}),
    _ = call(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"SENTINEL_no_such_sup">>}),
    _ = call(Config, <<"SENTINEL_tool">>, #{}),
    {401, _, _} = http(Port, "POST", [{"Host", ["127.0.0.1:", integer_to_list(Port)]},
                                     {"Content-Type", "application/json"}], <<"{}">>),
    Entries = lists:sort(fun(A, B) -> maps:get(seq, A) =< maps:get(seq, B) end, collect_journal(7)),
    ?assertEqual(lists:seq(1, 7), [maps:get(seq, E) || E <- Entries]),
    [Init, List, Reg, Pinfo, Tree, Unknown, Http] = Entries,
    ?assertMatch(#{method := <<"initialize">>, status := <<"ok">>, durationMs := _, bytes := _}, Init),
    ?assertMatch(#{method := <<"tools/list">>, status := <<"ok">>}, List),
    ?assertMatch(#{method := <<"tools/call">>, tool := <<"registered_processes">>, status := <<"ok">>,
                   entities := N, relationships := 0, offset := 0, cursor := false, complete := true}
                   when N > 0, Reg),
    ?assertEqual(maps:get(<<"nextCursor">>, First) =/= null, maps:get(more, Reg)),
    ?assertMatch(#{tool := <<"process_info">>, status := <<"ok">>, entities := _}, Pinfo),
    ?assertMatch(#{tool := <<"supervision_tree">>, status := <<"not_found">>}, Tree),
    %% unknown tool names never reach the journal as text
    ?assertMatch(#{tool := <<"?">>, status := <<"-32602">>}, Unknown),
    ?assertMatch(#{method := <<"http">>, status := <<"401">>}, Http),
    %% metadata only: no argument, result content or credential
    Text = iolist_to_binary(io_lib:format("~p", [Entries])),
    [?assertEqual(nomatch, binary:match(Text, Forbidden))
     || Forbidden <- [Tok, <<"SENTINEL">>, <<"mcp_fx_worker_a">>, <<"kernel_sup">>]],
    Allowed = [seq, method, tool, cursor, status, durationMs, bytes, entities, relationships,
               omissions, complete, more, offset],
    ?assertEqual([], lists:usort(lists:append([maps:keys(E) || E <- Entries])) -- Allowed),
    ?assertEqual(7, maps:get(seq, mcp_store:stats())),
    lists:foreach(fun({_, P, _, _}) -> supervisor:terminate_child(mcp_fx_dyn_sup, P) end,
                  supervisor:which_children(mcp_fx_dyn_sup)).

agent_summary_is_compact(Config) ->
    Summary = ok_call(Config, <<"application_overview">>, #{<<"detail">> => <<"summary">>}),
    Full = ok_call(Config, <<"application_overview">>, #{<<"detail">> => <<"full">>}),
    ?assertEqual(<<"summary">>, maps:get(<<"detail">>, Summary)),
    ?assertEqual(<<"full">>, maps:get(<<"detail">>, Full)),
    %% same graph (ids, kinds, edges), fewer fields
    Ids = fun(S) -> lists:sort([maps:get(<<"id">>, E) || E <- maps:get(<<"entities">>, S)]) end,
    ?assertEqual(Ids(Full), Ids(Summary)),
    ?assertEqual(length(maps:get(<<"relationships">>, Full)), length(maps:get(<<"relationships">>, Summary))),
    {ok, J1} = vscode_jsone:encode(Summary),
    {ok, J2} = vscode_jsone:encode(Full),
    ?assert(iolist_size(J1) < iolist_size(J2)),
    App = ent(<<"mcp_fixture_app">>, maps:get(<<"entities">>, Summary)),
    ?assertNot(maps:is_key(<<"description">>, App)),
    ?assertMatch(#{<<"roots">> := [_], <<"version">> := <<"9.9.9">>, <<"rootStatus">> := <<"discovered">>}, App),
    [?assertNot(maps:is_key(<<"observedAt">>, R)) || R <- maps:get(<<"relationships">>, Summary)],
    [?assertMatch(#{<<"evidence">> := _, <<"confidence">> := _}, R) || R <- maps:get(<<"relationships">>, Summary)],
    %% node-wide module lists are refused in summary (explained), allowed per application
    NoMods = ok_call(Config, <<"application_overview">>, #{<<"detail">> => <<"summary">>, <<"includeModules">> => true}),
    ?assertEqual([], [E || #{<<"kind">> := <<"module">>} = E <- maps:get(<<"entities">>, NoMods)]),
    ?assertMatch([#{<<"reason">> := <<"limit_reached">>}], maps:get(<<"omissions">>, NoMods)),
    Mods = ok_call(Config, <<"application_overview">>, #{<<"detail">> => <<"summary">>, <<"includeModules">> => true,
                                                         <<"application">> => maps:get(<<"id">>, App)}),
    ?assertEqual(2, length([E || #{<<"kind">> := <<"module">>} = E <- maps:get(<<"entities">>, Mods)])),
    %% agent pages: 100 items by default, pageSize honoured, limitations only on the first page
    ok = mcp_fixture_sup:add_dynamic(120),
    Args = #{<<"supervisor">> => <<"mcp_fx_root">>, <<"detail">> => <<"summary">>},
    P1 = ok_call(Config, <<"supervision_tree">>, Args),
    ?assertEqual(100, length(maps:get(<<"entities">>, P1)) + length(maps:get(<<"relationships">>, P1))),
    ?assert(maps:is_key(<<"limitations">>, maps:get(<<"scope">>, P1))),
    P2 = ok_call(Config, <<"supervision_tree">>, Args#{<<"cursor">> => maps:get(<<"nextCursor">>, P1)}),
    ?assertNot(maps:is_key(<<"limitations">>, maps:get(<<"scope">>, P2))),
    Small = ok_call(Config, <<"supervision_tree">>, Args#{<<"pageSize">> => 7}),
    ?assertEqual(7, length(maps:get(<<"entities">>, Small)) + length(maps:get(<<"relationships">>, Small))),
    Big = ok_call(Config, <<"supervision_tree">>, #{<<"supervisor">> => <<"mcp_fx_root">>, <<"detail">> => <<"full">>}),
    ?assert(length(maps:get(<<"entities">>, Big)) + length(maps:get(<<"relationships">>, Big)) > 100),
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"supervision_tree">>, Args#{<<"detail">> => <<"verbose">>})),
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"supervision_tree">>, Args#{<<"pageSize">> => 0})),
    lists:foreach(fun({_, P, _, _}) -> supervisor:terminate_child(mcp_fx_dyn_sup, P) end,
                  supervisor:which_children(mcp_fx_dyn_sup)).

top_processes_ranks_and_is_bounded(Config) ->
    Hot = spawn(fun() -> register(mcp_fx_hot, self()), receive never -> ok end end),
    wait_until(fun() -> whereis(mcp_fx_hot) =:= Hot end),
    [Hot ! {msg, I} || I <- lists:seq(1, 600)],
    S = ok_call(Config, <<"top_processes">>, #{<<"limit">> => 3}),
    Ents = maps:get(<<"entities">>, S),
    ?assertEqual(3, length(Ents)),
    ?assertEqual([1, 2, 3], [maps:get(<<"rank">>, E) || E <- Ents]),
    [First | _] = Ents,
    ?assertMatch(#{<<"name">> := <<"mcp_fx_hot">>, <<"messageQueueLen">> := Q, <<"rankedBy">> := <<"message_queue_len">>}
                   when Q >= 600, First),
    Qs = [maps:get(<<"messageQueueLen">>, E) || E <- Ents],
    ?assertEqual(lists:reverse(lists:sort(Qs)), Qs),
    ?assertMatch(#{<<"sortBy">> := <<"message_queue_len">>, <<"limit">> := 3, <<"processCount">> := _},
                 maps:get(<<"scope">>, S)),
    %% metadata only: no mailbox content, no relationships, no inspector processes
    ?assertEqual([], maps:get(<<"relationships">>, S)),
    ?assertEqual(nomatch, re:run(iolist_to_binary(io_lib:format("~p", [S])), "SENTINEL|\\{msg")),
    [?assertEqual(false, lists:member(maps:get(<<"name">>, E), [<<"mcp_server">>, <<"mcp_store">>, <<"mcp_sup">>]))
     || E <- Ents],
    %% the other criteria rank by their own value
    lists:foreach(fun({By, Field}) ->
                          R = ok_call(Config, <<"top_processes">>, #{<<"sortBy">> => By, <<"limit">> => 5}),
                          Es = maps:get(<<"entities">>, R),
                          ?assertEqual(5, length(Es)),
                          Vs = [maps:get(Field, E) || E <- Es],
                          ?assertEqual(lists:reverse(lists:sort(Vs)), Vs),
                          [?assertEqual(By, maps:get(<<"rankedBy">>, E)) || E <- Es]
                  end, [{<<"reductions">>, <<"reductions">>}, {<<"memory">>, <<"memory">>}]),
    %% a default call is bounded to 10 and the ids join with other tools
    D = ok_call(Config, <<"top_processes">>, #{}),
    ?assertEqual(10, length(maps:get(<<"entities">>, D))),
    HotId = id_of(ent(<<"mcp_fx_hot">>, maps:get(<<"entities">>, D))),
    ?assertEqual(HotId, id_of(ent(<<"mcp_fx_hot">>, maps:get(<<"entities">>,
                                  ok_call(Config, <<"process_info">>, #{<<"id">> => HotId}))))),
    %% strict arguments
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"top_processes">>, #{<<"limit">> => 51})),
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"top_processes">>, #{<<"limit">> => 0})),
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"top_processes">>, #{<<"sortBy">> => <<"links">>})),
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"top_processes">>, #{<<"mailbox">> => true})),
    exit(Hot, kill).

supervision_tree_reports_child_counts(Config) ->
    Pages = all_pages(Config, <<"supervision_tree">>, #{<<"id">> => fixture_root_id(Config)}),
    Ents = entities(Pages),
    Sub = hd([E || #{<<"childId">> := <<"mcp_fx_sub_sup">>} = E <- Ents]),
    ?assertMatch(#{<<"children">> := #{<<"specs">> := 2, <<"active">> := 2,
                                       <<"supervisors">> := 0, <<"workers">> := 2}}, Sub),
    Root = hd([E || #{<<"name">> := <<"mcp_fx_root">>} = E <- Ents]),
    ?assertMatch(#{<<"children">> := #{<<"specs">> := 3, <<"active">> := 3,
                                       <<"supervisors">> := 2, <<"workers">> := 1}}, Root),
    %% workers carry no counts
    ?assertEqual([], [E || #{<<"role">> := <<"worker">>} = E <- Ents, maps:is_key(<<"children">>, E)]).

mermaid_format_renders_the_returned_edges(Config) ->
    RootId = fixture_root_id(Config),
    Plain = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => RootId}),
    ?assertEqual(false, maps:is_key(<<"mermaid">>, Plain)),
    S = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => RootId, <<"format">> => <<"mermaid">>,
                                                 <<"detail">> => <<"summary">>}),
    ?assertEqual(false, maps:get(<<"truncated">>, S)),
    Mermaid = maps:get(<<"mermaid">>, S),
    [<<"graph TD">> | Lines] = binary:split(Mermaid, <<"\n">>, [global, trim_all]),
    Edges = [L || L <- Lines, binary:match(L, <<"-->">>) =/= nomatch],
    ?assertEqual(5, length(Edges)),
    [?assertNotEqual(nomatch, binary:match(E, <<"|supervises|">>)) || E <- Edges],
    ?assertEqual(5, length(supervises_edges([S]))),
    [?assertNotEqual(nomatch, binary:match(Mermaid, N)) || N <- [<<"mcp_fx_root">>, <<"mcp_fx_sub_sup">>, <<"supervisor">>]],
    %% only safe characters: nothing from the VM can break out of a label
    ?assertEqual(nomatch, re:run(binary:replace(Mermaid, <<"<br/>">>, <<" ">>, [global]), "[<>\"{}`;]\\w*[<>{}`;]",
                                 [{capture, none}])),
    ?assertEqual(nomatch, binary:match(Mermaid, <<"SENTINEL">>)),
    %% a relationship whose endpoint is on another page still gets a node
    Small = ok_call(Config, <<"supervision_tree">>, #{<<"id">> => RootId, <<"format">> => <<"mermaid">>,
                                                     <<"pageSize">> => 3}),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"mermaid">>, Small), <<"graph TD">>)),
    ?assertMatch({rpc_error, -32602, _}, call(Config, <<"supervision_tree">>, #{<<"id">> => RootId, <<"format">> => <<"dot">>})),
    %% other graph tools accept it too
    ?assertMatch(#{<<"mermaid">> := _}, ok_call(Config, <<"top_processes">>, #{<<"format">> => <<"mermaid">>})).

paused_process_is_reported_from_debugger_evidence(Config) ->
    Quiet = ok_call(Config, <<"debug_session">>, #{}),
    ?assertEqual([], maps:get(<<"pausedProcesses">>, Quiet)),
    Mod = mcp_fixture_paused,
    {module, Mod} = code:ensure_loaded(Mod),
    case catch int:i(Mod) of
        {module, Mod} ->
            try
                ok = int:auto_attach([break], {Mod, handler, []}),
                ok = int:break(Mod, Mod:break_line()),
                Pid = spawn(fun() -> Mod:run() end),
                Line = Mod:break_line(),
                wait_until(fun() ->
                                   case ok_call(Config, <<"debug_session">>, #{}) of
                                       #{<<"pausedProcesses">> := [_ | _]} -> true;
                                       _ -> false
                                   end
                           end),
                #{<<"pausedProcesses">> := [Paused]} = ok_call(Config, <<"debug_session">>, #{}),
                ?assertMatch(#{<<"status">> := <<"break">>, <<"module">> := <<"mcp_fixture_paused">>,
                               <<"line">> := Line, <<"id">> := _}, Paused),
                %% the same process id joins with process_info, which names the breakpoint
                Info = ok_call(Config, <<"process_info">>,
                               #{<<"pid">> => list_to_binary(pid_to_list(Pid))}),
                PidText = list_to_binary(pid_to_list(Pid)),
                [E] = [X || #{<<"pid">> := T} = X <- maps:get(<<"entities">>, Info), T =:= PidText],
                ?assertEqual(maps:get(<<"id">>, Paused), id_of(E)),
                ?assertMatch(#{<<"debugger">> := #{<<"status">> := <<"break">>,
                                                   <<"module">> := <<"mcp_fixture_paused">>,
                                                   <<"line">> := Line}}, E),
                %% an ordinary process has no debugger field
                Other = ok_call(Config, <<"process_info">>, #{<<"name">> => <<"mcp_fx_worker_c">>}),
                ?assertEqual(false, maps:is_key(<<"debugger">>, ent(<<"mcp_fx_worker_c">>, maps:get(<<"entities">>, Other)))),
                exit(Pid, kill)
            after
                catch int:auto_attach(false),
                catch int:no_break(Mod),
                catch int:n(Mod)
            end;
        Other2 ->
            {skip, {debugger_unavailable, Other2}}
    end.

collect_journal(0) -> [];
collect_journal(N) ->
    receive {journal, M} -> [M | collect_journal(N - 1)]
    after 3000 -> ct:fail({missing_journal_entries, N})
    end.

wait_until(Fun) -> wait_until(Fun, 5000).

wait_until(Fun, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_loop(Fun, Deadline).

wait_loop(Fun, Deadline) ->
    case Fun() of
        true -> ok;
        _ ->
            case erlang:monotonic_time(millisecond) > Deadline of
                true -> ct:fail(wait_until_timeout);
                false -> timer:sleep(50), wait_loop(Fun, Deadline)
            end
    end.
