-module(mcp_policy_SUITE).

%% Settings + rebar.config policy resolution (mcp_policy): defaults, validation,
%% loopback enforcement, project lookup boundaries, non-evaluation, ownership.

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [disabled_by_default_and_when_not_true,
     untrusted_workspace_disables,
     defaults_without_project_file,
     loopback_hosts_accepted,
     non_loopback_hosts_rejected,
     port_range_validated,
     project_policy_normalized,
     explicit_empty_allowlist_enables_no_tool,
     project_cannot_set_activation_or_binding,
     project_limits_cannot_exceed_ceilings,
     developer_tier_is_opt_in_and_ets_all_is_accepted,
     default_keyword_follows_the_built_in_tool_set,
     non_ascii_workspace_path_still_finds_the_project_policy,
     token_with_trailing_newline_is_rejected,
     invalid_and_duplicate_project_terms,
     profile_only_policy_reported,
     malformed_project_file_disables,
     script_is_never_executed,
     umbrella_uses_enclosing_project_file,
     lookup_never_leaves_the_workspace_folder,
     oversized_project_file_rejected,
     json_round_trip_and_authoritative_validation,
     only_project_policy_is_returned,
     fixed_token_is_validated].

init_per_testcase(Case, Config) ->
    Dir = filename:join(?config(priv_dir, Config), atom_to_list(Case)),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    [{root, Dir} | Config].

%%%%%%%%%%%
%% cases %%
%%%%%%%%%%%

input(Config, Extra) ->
    maps:merge(#{<<"enabled">> => true, <<"trusted">> => true,
                 <<"cwd">> => list_to_binary(?config(root, Config)),
                 <<"root">> => list_to_binary(?config(root, Config))}, Extra).

write_rebar(Config, Text) ->
    ok = file:write_file(filename:join(?config(root, Config), "rebar.config"), Text).

disabled_by_default_and_when_not_true(Config) ->
    ?assertEqual(disabled, mcp_policy:resolve(#{})),
    ?assertEqual(disabled, mcp_policy:resolve(input(Config, #{<<"enabled">> => false}))),
    ?assertEqual(disabled, mcp_policy:resolve(input(Config, #{<<"enabled">> => <<"true">>}))),
    ?assertEqual(disabled, mcp_policy:resolve(input(Config, #{<<"enabled">> => 1}))).

untrusted_workspace_disables(Config) ->
    ?assertMatch({error, "MCP is disabled: the workspace is not trusted"},
                 mcp_policy:resolve(input(Config, #{<<"trusted">> => false}))).

defaults_without_project_file(Config) ->
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual("127.0.0.1", maps:get(host, C)),
    ?assertEqual(0, maps:get(port, C)),
    ?assertEqual(mcp_policy:default_tools(), maps:get(allowed_tools, C)),
    ?assertEqual([], [T || T <- [<<"process_state">>, <<"mailbox_sample">>, <<"ets_sample">>], lists:member(T, maps:get(allowed_tools, C))]),
    ?assertEqual([], maps:get(allowed_ets_tables, C)),
    ?assertEqual(mcp_policy:default_limits(), maps:get(limits, C)),
    ?assertEqual(13, length(maps:get(allowed_tools, C))),
    %% a project without an mcp term uses the same built-in policy
    write_rebar(Config, "{erl_opts, [debug_info]}.\n{deps, []}.\n"),
    ?assertEqual({ok, C}, mcp_policy:resolve(input(Config, #{}))).

loopback_hosts_accepted(Config) ->
    [?assertMatch({ok, #{host := Expected}}, mcp_policy:resolve(input(Config, #{<<"host">> => Host})))
     || {Host, Expected} <- [{<<"127.0.0.1">>, "127.0.0.1"},
                             {<<"127.0.0.2">>, "127.0.0.2"},
                             {<<"::1">>, "::1"}]],
    %% a name is resolved and every address must be loopback
    ?assertMatch({ok, #{host := _}}, mcp_policy:resolve(input(Config, #{<<"host">> => <<"localhost">>}))).

non_loopback_hosts_rejected(Config) ->
    [?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{<<"host">> => Host})))
     || Host <- [<<"0.0.0.0">>, <<"::">>, <<"192.168.1.10">>, <<"10.0.0.1">>, <<"8.8.8.8">>,
                 <<"">>, <<"*">>, <<"[::]">>, 42, null]].

port_range_validated(Config) ->
    [?assertMatch({ok, #{port := P}}, mcp_policy:resolve(input(Config, #{<<"port">> => P})))
     || P <- [0, 1, 65535]],
    [?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{<<"port">> => P})))
     || P <- [-1, 65536, <<"80">>, 1.5, null]].

project_policy_normalized(Config) ->
    write_rebar(Config,
                "{erl_opts, [debug_info]}.\n"
                "{mcp, [{allowed_tools, [<<\"runtime_summary\">>, <<\"ets_tables\">>]},\n"
                "       {allowed_ets_tables, [<<\"orders_index\">>]},\n"
                "       {limits, [{max_items, 10}, {request_timeout_ms, 500}]}]}.\n"),
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual([<<"runtime_summary">>, <<"ets_tables">>], maps:get(allowed_tools, C)),
    ?assertEqual([<<"orders_index">>], maps:get(allowed_ets_tables, C)),
    ?assertEqual(10, mcp_policy:limit(max_items, C)),
    ?assertEqual(500, mcp_policy:limit(request_timeout_ms, C)),
    %% untouched limits keep their defaults
    ?assertEqual(16384, mcp_policy:limit(max_request_bytes, C)),
    %% settings own activation/binding
    ?assertMatch({ok, #{host := "127.0.0.1", port := 4711}},
                 mcp_policy:resolve(input(Config, #{<<"port">> => 4711}))).

explicit_empty_allowlist_enables_no_tool(Config) ->
    write_rebar(Config, "{mcp, [{allowed_tools, []}]}.\n"),
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual([], maps:get(allowed_tools, C)),
    [?assertNot(mcp_policy:tool_allowed(T, C)) || T <- mcp_policy:all_tools()],
    ?assertEqual([], mcp_tools:list(C)).

project_cannot_set_activation_or_binding(Config) ->
    [begin
         write_rebar(Config, lists:flatten(io_lib:format("{mcp, [~s]}.\n", [Term]))),
         ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{})), Term)
     end || Term <- ["{enabled, true}", "{required, true}", "{host, <<\"0.0.0.0\">>}", "{port, 1234}",
                     "{transport, streamable_http}", "{auth_token, <<\"x\">>}", "{token, <<\"x\">>}",
                     "{credentials, []}", "{cookie, abc}"]].

project_limits_cannot_exceed_ceilings(Config) ->
    [begin
         write_rebar(Config, lists:flatten(io_lib:format("{mcp, [{limits, [~s]}]}.\n", [Term]))),
         ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{})), Term)
     end || Term <- ["{max_items, 5001}", "{max_items, 0}", "{max_items, -1}", "{max_items, 1.5}",
                     "{max_result_bytes, 2097153}", "{request_timeout_ms, 15001}", "{max_concurrency, 9}",
                     "{unknown_limit, 1}", "{max_items, <<\"5\">>}",
                     %% unusable combinations: error envelopes must fit
                     "{max_result_bytes, 100}", "{max_request_bytes, 100}",
                     "{max_result_bytes, 8192}, {max_collection_bytes, 4096}"]],
    write_rebar(Config, "{mcp, [{limits, [{max_items, 1}, {max_result_bytes, 4096}, {max_collection_bytes, 4096}, {max_binary_bytes, 1024}]}]}.\n"),
    ?assertMatch({ok, _}, mcp_policy:resolve(input(Config, #{}))),
    %% raising a limit above its default is allowed up to the ceiling
    write_rebar(Config, "{mcp, [{limits, [{max_items, 5000}, {max_result_bytes, 2097152}, {max_collection_bytes, 8388608},"
                        " {request_timeout_ms, 15000}, {collection_ttl_ms, 600000}, {max_collections, 16}]}]}.\n"),
    {ok, Raised} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual(5000, mcp_policy:limit(max_items, Raised)),
    ?assertEqual(600000, mcp_policy:limit(collection_ttl_ms, Raised)).

developer_tier_is_opt_in_and_ets_all_is_accepted(Config) ->
    Dev = [<<"process_state">>, <<"mailbox_sample">>, <<"ets_sample">>],
    {ok, Default} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual([], [T || T <- Dev, mcp_policy:tool_allowed(T, Default)]),
    ?assert(lists:all(fun(T) -> lists:member(T, mcp_policy:all_tools()) end, Dev)),
    write_rebar(Config, "{mcp, [{allowed_tools, [<<\"process_state\">>, <<\"ets_sample\">>]}, {allowed_ets_tables, all}]}.\n"),
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    ?assert(mcp_policy:tool_allowed(<<"process_state">>, C)),
    ?assertNot(mcp_policy:tool_allowed(<<"mailbox_sample">>, C)),
    ?assertEqual(all, maps:get(allowed_ets_tables, C)),
    ?assert(mcp_policy:ets_table_allowed(<<"anything">>, C)),
    %% crosses to the target and back unchanged
    ?assertEqual({ok, C}, mcp_policy:from_json_map(mcp_policy:to_json_map(C))),
    ?assertNot(mcp_policy:ets_table_allowed(<<"x">>, Default)),
    [begin
         write_rebar(Config, Text),
         ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{})), Text)
     end || Text <- ["{mcp, [{allowed_ets_tables, everything}]}.\n", "{mcp, [{allowed_tools, [<<\"process_dump\">>]}]}.\n"]].

default_keyword_follows_the_built_in_tool_set(Config) ->
    Default = mcp_policy:default_tools(),
    Tools = fun(Text) ->
                    write_rebar(Config, "{mcp, [{allowed_tools, " ++ Text ++ "}]}.\n"),
                    {ok, C} = mcp_policy:resolve(input(Config, #{})),
                    maps:get(allowed_tools, C)
            end,
    ?assertEqual(Default, Tools("default")),
    ?assertEqual(Default, Tools("[default]")),
    %% the built-in set plus a developer tool; an explicit tool is never listed twice
    ?assertEqual(lists:sort([<<"process_state">> | Default]), lists:sort(Tools("[default, <<\"process_state\">>]"))),
    ?assertEqual(lists:sort(Default), lists:sort(Tools("[default, <<\"runtime_summary\">>]"))),
    [begin
         write_rebar(Config, Text),
         ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{})), Text)
     end || Text <- ["{mcp, [{allowed_tools, [default, default]}]}.\n", "{mcp, [{allowed_tools, [defaults]}]}.\n",
                     "{mcp, [{allowed_tools, all}]}.\n"]].

non_ascii_workspace_path_still_finds_the_project_policy(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "José_проект"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    ok = file:write_file(filename:join(Dir, "rebar.config"), "{mcp, [{allowed_tools, [<<\"runtime_summary\">>]}]}.\n"),
    Bin = unicode:characters_to_binary(Dir),
    {ok, C} = mcp_policy:resolve(#{<<"enabled">> => true, <<"trusted">> => true, <<"cwd">> => Bin, <<"root">> => Bin}),
    ?assertEqual([<<"runtime_summary">>], maps:get(allowed_tools, C)).

token_with_trailing_newline_is_rejected(_Config) ->
    ?assertEqual(ok, mcp_policy:validate_token(<<"abcdefghijklmnop">>)),
    ?assertMatch({error, _}, mcp_policy:validate_token(<<"abcdefghijklmnop\n">>)).

invalid_and_duplicate_project_terms(Config) ->
    [begin
         write_rebar(Config, Text),
         ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{})), Text)
     end || Text <- ["{mcp, [{allowed_tools, [<<\"eval\">>]}]}.\n",
                     "{mcp, [{allowed_tools, [<<\"runtime_summary\">>, <<\"runtime_summary\">>]}]}.\n",
                     "{mcp, [{allowed_tools, [runtime_summary]}]}.\n",
                     "{mcp, [{allowed_tools, []}, {allowed_tools, []}]}.\n",
                     "{mcp, [{allowed_ets_tables, [foo]}]}.\n",
                     "{mcp, [{allowed_ets_tables, [<<\"a\">>, <<\"a\">>]}]}.\n",
                     "{mcp, [{whatever, 1}]}.\n",
                     "{mcp, [{limits, [{max_items, 5}, {max_items, 6}]}]}.\n",
                     "{mcp, notalist}.\n",
                     "{mcp, [{allowed_tools, []}]}.\n{mcp, [{allowed_tools, []}]}.\n"]].

profile_only_policy_reported(Config) ->
    write_rebar(Config, "{profiles, [{test, [{mcp, [{allowed_tools, []}]}]}]}.\n"),
    {error, Msg} = mcp_policy:resolve(input(Config, #{})),
    ?assertNotEqual(nomatch, string:find(Msg, "profile")),
    %% a top-level term wins: the profile term is not merged
    write_rebar(Config, "{mcp, []}.\n{profiles, [{test, [{mcp, [{allowed_tools, []}]}]}]}.\n"),
    ?assertMatch({ok, #{allowed_tools := [_ | _]}}, mcp_policy:resolve(input(Config, #{}))).

malformed_project_file_disables(Config) ->
    [begin
         write_rebar(Config, Text),
         ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{})), Text)
     end || Text <- ["{mcp, [{allowed_tools, [}.\n",
                     "{mcp, [].\n",
                     "not a term at all\n",
                     "{mcp, [{limits, [{max_items, 1 + 1}]}]}.\n",
                     "{mcp, foo(bar)}.\n",
                     <<255, 254, 253>>]].

script_is_never_executed(Config) ->
    Marker = filename:join(?config(root, Config), "script_ran"),
    write_rebar(Config, "{mcp, [{allowed_tools, [<<\"runtime_summary\">>]}]}.\n"),
    ok = file:write_file(filename:join(?config(root, Config), "rebar.config.script"),
                         io_lib:format("file:write_file(~p, <<\"ran\">>), CONFIG.~n", [Marker])),
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual([<<"runtime_summary">>], maps:get(allowed_tools, C)),
    ?assertNot(filelib:is_file(Marker)),
    %% the parser does not evaluate function calls placed in the file either
    write_rebar(Config, io_lib:format("{mcp, [{allowed_tools, [list_to_binary(\"x\")]}]}.~n{x, file:write_file(~p, <<\"x\">>)}.~n", [Marker])),
    ?assertMatch({error, _}, mcp_policy:resolve(input(Config, #{}))),
    ?assertNot(filelib:is_file(Marker)).

umbrella_uses_enclosing_project_file(Config) ->
    Root = ?config(root, Config),
    App = filename:join([Root, "apps", "myapp"]),
    ok = filelib:ensure_dir(filename:join(App, "x")),
    write_rebar(Config, "{mcp, [{allowed_tools, [<<\"debug_session\">>]}]}.\n"),
    Input = (input(Config, #{}))#{<<"cwd">> => list_to_binary(App)},
    ?assertMatch({ok, #{allowed_tools := [<<"debug_session">>]}}, mcp_policy:resolve(Input)),
    %% the application's own file is the nearest one
    ok = file:write_file(filename:join(App, "rebar.config"), "{mcp, [{allowed_tools, [<<\"ets_tables\">>]}]}.\n"),
    ?assertMatch({ok, #{allowed_tools := [<<"ets_tables">>]}}, mcp_policy:resolve(Input)).

lookup_never_leaves_the_workspace_folder(Config) ->
    Base = ?config(root, Config),
    Root = filename:join(Base, "folder"),
    Sibling = filename:join(Base, "folder2"),
    Outside = Base,
    [ok = filelib:ensure_dir(filename:join(D, "x")) || D <- [Root, Sibling]],
    %% a permissive-looking file above the folder must not be consulted
    ok = file:write_file(filename:join(Outside, "rebar.config"), "{mcp, [{allowed_tools, []}]}.\n"),
    Input = #{<<"enabled">> => true, <<"trusted">> => true,
              <<"cwd">> => list_to_binary(Root), <<"root">> => list_to_binary(Root)},
    ?assertMatch({ok, #{allowed_tools := [_, _ | _]}}, mcp_policy:resolve(Input)),
    %% cwd outside the folder (also a string-prefix sibling) disables MCP
    ?assertMatch({error, _}, mcp_policy:resolve(Input#{<<"cwd">> => list_to_binary(Sibling)})),
    ?assertMatch({error, _}, mcp_policy:resolve(Input#{<<"cwd">> => list_to_binary(Outside)})),
    ?assertMatch({error, _}, mcp_policy:resolve(Input#{<<"cwd">> => list_to_binary(filename:join(Root, "../folder2"))})),
    ?assertMatch({error, _}, mcp_policy:resolve(Input#{<<"cwd">> => <<>>})).

oversized_project_file_rejected(Config) ->
    Big = lists:duplicate(300000, $\s),
    write_rebar(Config, ["{mcp, []}.\n", Big]),
    ?assertMatch({error, "rebar.config is too large" ++ _}, mcp_policy:resolve(input(Config, #{}))).

json_round_trip_and_authoritative_validation(Config) ->
    {ok, C} = mcp_policy:resolve(input(Config, #{<<"port">> => 5000})),
    Json = mcp_policy:to_json_map(C),
    {ok, Encoded} = vscode_jsone:encode(Json),
    {ok, Decoded, _} = vscode_jsone_decode:decode(iolist_to_binary(Encoded)),
    ?assertEqual({ok, C}, mcp_policy:from_json_map(Decoded)),
    %% the target validates again: tampered configuration is refused
    [?assertMatch({error, _}, mcp_policy:from_json_map(Bad))
     || Bad <- [Decoded#{<<"host">> => <<"0.0.0.0">>},
                Decoded#{<<"port">> => 70000},
                Decoded#{<<"authToken">> => <<"x">>},
                Decoded#{<<"required">> => true},
                Decoded#{<<"allowedTools">> => [<<"eval">>]},
                Decoded#{<<"limits">> => (maps:get(<<"limits">>, Decoded))#{<<"maxItems">> => 100000}},
                Decoded#{<<"limits">> => (maps:get(<<"limits">>, Decoded))#{<<"extra">> => 1}},
                maps:remove(<<"limits">>, Decoded),
                #{}]],
    ?assertMatch({error, _}, mcp_sup:start_session(C#{host => "0.0.0.0"}, launch)),
    ?assertNot(mcp_sup:active()).

only_project_policy_is_returned(Config) ->
    %% other project terms never leak into the normalized policy
    write_rebar(Config, "{deps, [{secret_dep, \"SENTINEL_DEP\"}]}.\n{mcp, []}.\n"),
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    ?assertEqual(nomatch, re:run(io_lib:format("~p", [C]), "SENTINEL")),
    ?assertEqual([allowed_ets_tables, allowed_tools, host, limits, port], lists:sort(maps:keys(C))).

fixed_token_is_validated(Config) ->
    {ok, C} = mcp_policy:resolve(input(Config, #{})),
    Base = mcp_policy:to_json_map(C),
    ?assertEqual(ok, mcp_policy:validate_token(<<"dev-token_0123456789.AZ~">>)),
    [?assertMatch({error, _}, mcp_policy:validate_token(T))
     || T <- [<<"short">>, <<>>, binary:copy(<<"a">>, 129), <<"has space 0123456789">>,
              <<"quote\"0123456789abc">>, <<"sl/ash0123456789abc">>, 42, undefined]],
    ?assertEqual({ok, C}, mcp_policy:from_json_map(Base#{<<"authToken">> => <<"dev-token_0123456789">>})),
    ?assertMatch({error, _}, mcp_policy:from_json_map(Base#{<<"authToken">> => <<"x">>})).
