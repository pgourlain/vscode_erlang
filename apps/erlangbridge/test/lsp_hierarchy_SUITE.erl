-module(lsp_hierarchy_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 4.5 (call hierarchy): hier_gs.erl's handle_call/3 calls helper/1
%% locally (unqualified); hier_caller.erl calls both start_link/0 and
%% helper/1 remotely (module-qualified, cross-file) - together they
%% exercise both reference-finding paths incoming_calls/1 combines
%% (gen_lsp_doc_server's project-wide cache and local_function_references).
%%
%% Task 4.6 (type hierarchy): hier_behaviour.erl/hier_impl.erl are a
%% deliberately project-local, made-up behaviour pair (not a real OTP
%% behaviour like gen_server) so supertypes/subtypes never depend on
%% whether this environment happens to have OTP's own stdlib *source*
%% available on disk.

all() -> [
    prepare_call_hierarchy_resolves_from_definition_or_call_site,
    incoming_calls_finds_local_and_cross_file_callers,
    outgoing_calls_finds_only_project_local_targets,
    prepare_type_hierarchy_resolves_to_the_files_own_module,
    supertypes_finds_the_declared_behaviour,
    subtypes_finds_the_implementing_module
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    lists:foreach(fun open_and_parse/1, [
        filename:join(AppDir, "hier_gs.erl"),
        filename:join(AppDir, "hier_caller.erl"),
        filename:join(AppDir, "hier_behaviour.erl"),
        filename:join(AppDir, "hier_impl.erl")
    ]),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

open_and_parse(File) ->
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File).

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

prepare_call_hierarchy_resolves_from_definition_or_call_site(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hier_gs.erl"),
    {ok, Content} = file:read_file(File),
    {DefLine, _} = position_of(Content, "helper(X) ->"),
    {CallLine, CallCol} = position_of(Content, "helper(State)"),
    [FromDef] = lsp_hierarchy:prepare_call_hierarchy(File, DefLine, 1),
    [FromCall] = lsp_hierarchy:prepare_call_hierarchy(File, CallLine, CallCol),
    ?assertEqual(<<"helper/1">>, maps:get(name, FromDef)),
    ?assertEqual(FromDef, FromCall).

%% helper/1 is called from handle_call/3 (same file, unqualified) and
%% hier_caller:go/0 (a different file, module-qualified) - one incoming
%% call group per caller, each with exactly its own call site as fromRanges.
incoming_calls_finds_local_and_cross_file_callers(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hier_gs.erl"),
    {ok, Content} = file:read_file(File),
    {DefLine, _} = position_of(Content, "helper(X) ->"),
    [Item] = lsp_hierarchy:prepare_call_hierarchy(File, DefLine, 1),
    Incoming = lsp_hierarchy:incoming_calls(round_trip(Item)),
    Names = lists:sort([maps:get(name, maps:get(from, C)) || C <- Incoming]),
    ?assertEqual([<<"go/0">>, <<"handle_call/3">>], Names),
    [HandleCallEntry] = [C || C <- Incoming, maps:get(name, maps:get(from, C)) =:= <<"handle_call/3">>],
    ?assertEqual(1, length(maps:get(fromRanges, HandleCallEntry))).

%% handle_call/3's own first clause calls helper/1 (project-local,
%% resolvable) - its second clause calls nothing. gen_server:start_link/0
%% (in start_link/0, a different function entirely) is deliberately not
%% part of this function's own outgoing calls.
outgoing_calls_finds_only_project_local_targets(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hier_gs.erl"),
    {ok, Content} = file:read_file(File),
    {DefLine, _} = position_of(Content, "handle_call(get, _From, State) ->"),
    [Item] = lsp_hierarchy:prepare_call_hierarchy(File, DefLine, 1),
    Outgoing = lsp_hierarchy:outgoing_calls(round_trip(Item)),
    ?assertMatch([#{to := #{name := <<"helper/1">>}, fromRanges := [_]}], Outgoing).

prepare_type_hierarchy_resolves_to_the_files_own_module(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hier_impl.erl"),
    [Item] = lsp_hierarchy:prepare_type_hierarchy(File, 1, 1),
    ?assertEqual(<<"hier_impl">>, maps:get(name, Item)).

supertypes_finds_the_declared_behaviour(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hier_impl.erl"),
    [Item] = lsp_hierarchy:prepare_type_hierarchy(File, 1, 1),
    Supertypes = lsp_hierarchy:supertypes(round_trip(Item)),
    ?assertEqual([<<"hier_behaviour">>], [maps:get(name, S) || S <- Supertypes]).

subtypes_finds_the_implementing_module(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hier_behaviour.erl"),
    [Item] = lsp_hierarchy:prepare_type_hierarchy(File, 1, 1),
    Subtypes = lsp_hierarchy:subtypes(round_trip(Item)),
    ?assertEqual([<<"hier_impl">>], [maps:get(name, S) || S <- Subtypes]).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

%% incoming_calls/1 and friends receive the CallHierarchyItem/
%% TypeHierarchyItem back from the client exactly as the server produced
%% it, JSON round-tripped - so its `data` map's atom/module/function
%% values come back as binaries, same as everywhere else in this codebase
%% that stores atoms in a code action/hierarchy item's own `data` field.
round_trip(Term) ->
    {ok, Json} = vscode_jsone:encode(Term),
    {ok, Decoded, _} = vscode_jsone_decode:decode(Json, [{keys, atom}]),
    Decoded.

%% 1-based {Line, Column} of the first character of Marker.
position_of(Content, Marker) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_position(Lines, list_to_binary(Marker), 1).

find_position([Line | Rest], MarkerBin, LineNo) ->
    case binary:match(Line, MarkerBin) of
        {Start, _} -> {LineNo, Start + 1};
        nomatch -> find_position(Rest, MarkerBin, LineNo + 1)
    end.
