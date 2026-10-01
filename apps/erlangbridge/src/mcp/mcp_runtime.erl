%% Read-only runtime inspection behind the seven MCP tools.
%%
%% Everything here is metadata: no process dictionary, no sys:get_state, no
%% mailbox, no ETS keys/objects, no application callbacks, no code loading.
%% Callers run `call/3` inside a monitored, time-bounded worker (mcp_server).
%%
%% Mapping contract (schemaVersion 1.0): entities (application, process,
%% module, ets_table), typed relationships (depends_on, supervises, owns_table,
%% linked_to, monitors, belongs_to, uses_module) with evidence, observation
%% time and confidence, retained as a bounded *collection* and served in pages
%% through signed cursors. A collection is an observation interval, never an
%% atomic snapshot.
%%
%% Cost notes (documented residual risk): application:which_applications/1,
%% erlang:processes/0 (top_processes, capped at ?MAX_SCANNED entries), registered/0, supervisor:which_children/1 and supervisor:count_children/1
%% materialize lists in the target VM and cannot be interrupted once started;
%% killing the worker only discards the late reply.
-module(mcp_runtime).

-export([call/3, bounded/2, args_hash/1]).

-define(SV, <<"1.0">>).
-define(MAX_LISTED_CHILDREN, 20000).
-define(MAX_SPEC_LOOKUPS, 200).
%% default page size for agents (detail=summary); detail=full pages use max_items
-define(SUMMARY_PAGE, 100).
-define(MAX_SCANNED, 200000).
-define(MAX_TOP, 50).
-define(MAX_ETS_SCAN, 50000).
-define(MAX_MAILBOX_COPY, 20000).
-define(MAX_GROUPS, 5000).
-define(MAX_SAMPLE, 20).

%%------------------------------------------------------------------------------
%% Entry point
%%------------------------------------------------------------------------------

%% Ctx: #{config, timeout_ms}
%% -> {ok, Structured} | {error, Code :: binary(), Message :: binary()}
call(Tool, Args, Ctx0) ->
    Config = maps:get(config, Ctx0),
    Timeout = maps:get(timeout_ms, Ctx0),
    MaxItems = mcp_policy:limit(max_items, Config),
    Detail = case maps:get(<<"detail">>, Args, <<"summary">>) of
                 <<"full">> -> full;
                 _ -> summary
             end,
    %% agents get small pages by default; detail=full is meant for exports
    PageSize = case {maps:get(<<"pageSize">>, Args, undefined), Detail} of
                   {undefined, summary} -> min(?SUMMARY_PAGE, MaxItems);
                   {undefined, full} -> MaxItems;
                   {N, _} -> min(N, MaxItems)
               end,
    Format = case maps:get(<<"format">>, Args, <<"json">>) of
                 <<"mermaid">> -> mermaid;
                 _ -> json
             end,
    Ctx = Ctx0#{format => Format,
                enc => maps:with([max_depth, max_items, max_binary_bytes], maps:get(limits, Config)),
                soft_deadline => now_mono() + (Timeout * 7) div 10,
                detail => Detail, page_size => PageSize},
    case maps:find(<<"cursor">>, Args) of
        {ok, Cursor} -> page_from_cursor(Tool, Args, Cursor, Ctx);
        error -> run(Tool, Args, Ctx)
    end.

run(<<"runtime_summary">>, Args, Ctx) -> runtime_summary(Args, Ctx);
run(<<"debug_session">>, _Args, Ctx) -> debug_session(Ctx);
run(<<"application_overview">>, Args, Ctx) -> graph_tool(<<"application_overview">>, builder(<<"application_overview">>), Args, Ctx);
run(<<"supervision_tree">>, Args, Ctx) -> graph_tool(<<"supervision_tree">>, builder(<<"supervision_tree">>), Args, Ctx);
run(<<"registered_processes">>, Args, Ctx) -> graph_tool(<<"registered_processes">>, builder(<<"registered_processes">>), Args, Ctx);
run(<<"process_info">>, Args, Ctx) -> tool_process_info(Args, Ctx);
run(<<"ets_tables">>, Args, Ctx) -> graph_tool(<<"ets_tables">>, builder(<<"ets_tables">>), Args, Ctx);
run(<<"process_state">>, Args, Ctx) -> process_state(Args, Ctx);
run(<<"process_groups">>, Args, Ctx) -> process_groups(Args, Ctx);
run(<<"mailbox_sample">>, Args, Ctx) -> mailbox_sample(Args, Ctx);
run(<<"ets_sample">>, Args, Ctx) -> ets_sample(Args, Ctx);
run(<<"top_ports">>, Args, Ctx) -> graph_tool(<<"top_ports">>, builder(<<"top_ports">>), Args, Ctx);
run(<<"ets_summary">>, Args, Ctx) -> graph_tool(<<"ets_summary">>, builder(<<"ets_summary">>), Args, Ctx);
run(<<"changes_since">>, Args, Ctx) -> graph_tool(<<"changes_since">>, builder(<<"changes_since">>), Args, Ctx);
run(<<"topology_overview">>, Args, Ctx) -> graph_tool(<<"topology_overview">>, builder(<<"topology_overview">>), Args, Ctx);
run(<<"top_processes">>, Args, Ctx) -> graph_tool(<<"top_processes">>, builder(<<"top_processes">>), Args, Ctx);
run(_, _, _) -> {error, <<"unknown_tool">>, <<"unknown tool">>}.

builder(<<"application_overview">>) -> fun application_overview_build/2;
builder(<<"supervision_tree">>) -> fun supervision_tree_build/2;
builder(<<"registered_processes">>) -> fun registered_processes_build/2;
builder(<<"ets_tables">>) -> fun ets_tables_build/2;
builder(<<"top_processes">>) -> fun top_processes_build/2;
builder(<<"topology_overview">>) -> fun topology_overview_build/2;
builder(<<"changes_since">>) -> fun changes_since_build/2;
builder(<<"top_ports">>) -> fun top_ports_build/2;
builder(<<"ets_summary">>) -> fun ets_summary_build/2.

%% Graph tools build a Builder (entities, relationships, omissions) and a scope;
%% finish_graph retains it as a collection and renders the first page.
graph_tool(Tool, Build, Args, Ctx) ->
    Started = mcp_store:now_ms(),
    case Build(Args, Ctx) of
        {ok, Scope, B} -> finish_graph(Tool, Args, Scope, B, Started, Ctx);
        {error, _, _} = E -> E
    end.

%% A deadline-bounded call in a monitored worker. On timeout the worker is
%% killed (an inspector-owned process) and any late reply is discarded.
%% -> {ok, Result} | timeout | {error, Reason}
bounded(Fun, Timeout) when Timeout > 0 ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(
                    fun() ->
                            Res = try {ok, Fun()} catch C:R -> {error, {C, R}} end,
                            Parent ! {Ref, Res}
                    end),
    receive
        {Ref, Res} ->
            erlang:demonitor(MRef, [flush]),
            Res;
        {'DOWN', MRef, process, Pid, Reason} ->
            receive {Ref, Res} -> Res after 0 -> {error, {down, Reason}} end
    after Timeout ->
            exit(Pid, kill),
            erlang:demonitor(MRef, [flush]),
            receive {Ref, _} -> ok after 0 -> ok end,
            timeout
    end;
bounded(_, _) ->
    timeout.

now_mono() -> erlang:monotonic_time(millisecond).

remaining(#{soft_deadline := D}) -> max(0, D - now_mono()).

now_iso() -> mcp_encoder:iso8601(mcp_store:now_ms()).

args_hash(Args) ->
    Sorted = lists:sort(maps:to_list(Args)),
    <<H:8/binary, _/binary>> = crypto:hash(sha256, term_to_binary(Sorted)),
    mcp_encoder:hex(H).

%%------------------------------------------------------------------------------
%% runtime_summary / debug_session (plain structured results)
%%------------------------------------------------------------------------------

runtime_summary(Args, _Ctx) ->
    Redact = maps:get(<<"redactNodeHost">>, Args, true),
    {Uptime, _} = erlang:statistics(wall_clock),
    Mem = erlang:memory(),
    {ok, #{<<"schemaVersion">> => ?SV,
           <<"sessionId">> => mcp_store:session_id(),
           <<"observedAt">> => now_iso(),
           <<"otpRelease">> => list_to_binary(erlang:system_info(otp_release)),
           <<"ertsVersion">> => list_to_binary(erlang:system_info(version)),
           <<"node">> => node_text(Redact),
           <<"nodeRedacted">> => Redact,
           <<"uptimeMs">> => Uptime,
           <<"schedulers">> => erlang:system_info(schedulers),
           <<"processCount">> => erlang:system_info(process_count),
           <<"connectedNodes">> => connected_nodes(Redact),
           <<"schedulersOnline">> => erlang:system_info(schedulers_online),
           <<"runQueue">> => erlang:statistics(run_queue),
           <<"resources">> => #{<<"processes">> => resource(process_count, process_limit),
                                <<"atoms">> => resource(atom_count, atom_limit),
                                <<"ports">> => resource(port_count, port_limit),
                                <<"ets">> => resource(ets_count, ets_limit)},
           <<"memory">> => #{<<"unit">> => <<"bytes">>,
                             <<"total">> => proplists:get_value(total, Mem),
                             <<"code">> => proplists:get_value(code, Mem),
                             <<"processes">> => proplists:get_value(processes, Mem),
                             <<"system">> => proplists:get_value(system, Mem),
                             <<"atom">> => proplists:get_value(atom, Mem),
                             <<"binary">> => proplists:get_value(binary, Mem),
                             <<"ets">> => proplists:get_value(ets, Mem)}}}.

%% Names of the visible connected nodes (hidden nodes such as the debugger helper are
%% not listed by nodes/0); the host part is redacted like the node's own name.
connected_nodes(Redact) ->
    Nodes = nodes(),
    #{<<"count">> => length(Nodes),
      <<"names">> => [peer_text(N, Redact) || N <- lists:sublist(lists:sort(Nodes), 50)]}.

peer_text(Node, false) -> atom_to_binary(Node, utf8);
peer_text(Node, true) ->
    case binary:split(atom_to_binary(Node, utf8), <<"@">>) of
        [Name, _Host] -> <<Name/binary, "@<redacted>">>;
        [Name] -> Name
    end.

%% {count, limit, usedPercent}: how close the node is to a VM limit; absent
%% (null) on an OTP release that does not report it.
resource(CountKey, LimitKey) ->
    try {erlang:system_info(CountKey), erlang:system_info(LimitKey)} of
        {C, L} when is_integer(C), is_integer(L), L > 0 ->
            #{<<"count">> => C, <<"limit">> => L, <<"usedPercent">> => round(C * 1000 / L) / 10};
        _ -> null
    catch _:_ -> null
    end.

node_text(false) -> atom_to_binary(node(), utf8);
node_text(true) ->
    case binary:split(atom_to_binary(node(), utf8), <<"@">>) of
        [Name, _Host] -> <<Name/binary, "@<redacted>">>;
        [Name] -> Name
    end.

%% Safe debugger metadata only: mode, connection state, interpreted modules and
%% breakpoint locations (module + line). Source and options are not returned.
debug_session(#{enc := Enc} = Ctx) ->
    Max = maps:get(max_items, Enc),
    %% three debugger calls share the request budget instead of 1.5 s each
    Budget = max(100, min(1500, remaining(Ctx) div 3)),
    {Interpreted, IUnavailable} = debug_call(fun() -> int:interpreted() end, [], Budget),
    {Breaks, BUnavailable} = debug_call(fun() -> int:all_breaks() end, [], Budget),
    {Snapshot, SUnavailable} = debug_call(fun() -> int:snapshot() end, [], Budget),
    Paused = paused_list(Snapshot),
    Mods = [atom_to_binary(M, utf8) || M <- lists:sublist(lists:sort(Interpreted), Max), is_atom(M)],
    Bps = [#{<<"module">> => atom_to_binary(M, utf8), <<"line">> => L}
           || {{M, L}, _} <- lists:sublist(lists:sort(Breaks), Max), is_atom(M), is_integer(L)],
    {ok, #{<<"schemaVersion">> => ?SV,
           <<"sessionId">> => mcp_store:session_id(),
           <<"observedAt">> => now_iso(),
           <<"mode">> => mode_text(mcp_store:mode()),
           <<"nodeConnected">> => true,
           <<"interpretedModules">> => Mods,
           <<"breakpoints">> => Bps,
           <<"pausedProcesses">> => [#{<<"id">> => pid_id(P),
                                       <<"name">> => case erlang:process_info(P, registered_name) of
                                                         {registered_name, N} -> bin(N);
                                                         _ -> null
                                                     end,
                                       <<"status">> => <<"break">>,
                                       <<"module">> => atom_to_binary(M, utf8),
                                       <<"line">> => L}
                                     || {P, M, L} <- lists:sublist(Paused, Max)],
           <<"debuggerMetadata">> => case IUnavailable orelse BUnavailable orelse SUnavailable of
                                         true -> <<"unavailable">>;
                                         false -> <<"available">>
                                     end,
           <<"truncated">> => length(Interpreted) > Max orelse length(Breaks) > Max
                                  orelse length(Paused) > Max}}.

debug_call(Fun, Default, Budget) ->
    case bounded(Fun, Budget) of
        {ok, R} when is_list(R) -> {R, false};
        _ -> {Default, true}
    end.

%% int:snapshot/0 -> [{Pid, Mod, Line}] of the interpreted processes stopped at a
%% breakpoint (status `break`). Only local pids and module/line are kept.
paused_list(Snapshot) ->
    lists:sort([{P, M, L} || {P, _Init, break, {M, L}} <- Snapshot,
                             is_pid(P), node(P) =:= node(), is_atom(M), is_integer(L)]).

%% -> {ok, #{Pid => {Mod, Line}}} | unavailable   (debugger evidence only)
paused_map() ->
    case bounded(fun() -> int:snapshot() end, 500) of
        {ok, L} when is_list(L) -> {ok, maps:from_list([{P, {M, Line}} || {P, M, Line} <- paused_list(L)])};
        _ -> unavailable
    end.

paused_at(Pid) ->
    case paused_map() of
        {ok, M} -> maps:find(Pid, M);
        unavailable -> error
    end.

mode_text(launch) -> <<"launch">>;
mode_text(attach) -> <<"attach">>;
mode_text(_) -> <<"unknown">>.

%%------------------------------------------------------------------------------
%% Builder: entities, relationships and omissions of one collection
%%------------------------------------------------------------------------------

new_b() -> #{ents => [], ent_ids => #{}, rels => [], rel_keys => #{}, oms => [], pids => #{}}.

add_entity(#{ent_ids := Ids, ents := Es} = B, Id, Entity) ->
    case maps:is_key(Id, Ids) of
        true -> B;
        false -> B#{ents := [Entity#{<<"id">> => Id} | Es], ent_ids := Ids#{Id => true}}
    end.

%% add fields to an entity already in the builder (add_entity keeps the first one)
merge_entity(#{ents := Es} = B, Id, Fields) ->
    B#{ents := [case E of
                    #{<<"id">> := Id} -> maps:merge(E, Fields);
                    _ -> E
                end || E <- Es]}.

add_rel(#{rel_keys := Keys, rels := Rs} = B, Type, From, To, Evidence, Confidence) ->
    Key = {Type, From, To},
    case maps:is_key(Key, Keys) of
        true -> B;
        false ->
            R = #{<<"type">> => Type, <<"from">> => From, <<"to">> => To,
                  <<"evidence">> => Evidence, <<"observedAt">> => now_iso(),
                  <<"confidence">> => Confidence},
            B#{rel_keys := Keys#{Key => true}, rels := [R | Rs]}
    end.

add_omission(#{oms := Oms} = B, Reason, Count, Scope, Message, Resumable) ->
    O0 = #{<<"reason">> => Reason, <<"count">> => Count,
           <<"resumable">> => Resumable, <<"message">> => Message},
    O = case Scope of
            undefined -> O0;
            _ -> O0#{<<"scope">> => Scope}
        end,
    B#{oms := [O | Oms]}.

note_pid(#{pids := P} = B, Pid) -> B#{pids := P#{Pid => true}}.

%%------------------------------------------------------------------------------
%% Collections and pages
%%------------------------------------------------------------------------------

finish_graph(Tool, Args, Scope, B, Started, Ctx) ->
    Oms = lists:reverse(maps:get(oms, B)),
    Truncated = lists:any(fun(#{<<"reason">> := R}) -> R =:= <<"limit_reached">> end, Oms),
    Meta = #{scope => Scope, started_at => Started, finished_at => mcp_store:now_ms(),
             complete => Oms =:= [], truncated => Truncated, omissions => Oms},
    Items = builder_items(B),
    Hash = args_hash(maps:remove(<<"cursor">>, Args)),
    {CollId, Meta1, Stored} = mcp_store:put_collection(Tool, Hash, Meta, Items, #{args => Args, detail => maps:get(detail, Ctx, summary)}),
    render_page(Tool, Hash, CollId, Meta1, Stored, 0, Ctx).

builder_items(B) ->
    [{entity, E} || E <- lists:reverse(maps:get(ents, B))]
        ++ [{relationship, R} || R <- lists:reverse(maps:get(rels, B))].

page_from_cursor(Tool, Args, Cursor, Ctx) ->
    Hash = args_hash(maps:remove(<<"cursor">>, Args)),
    case mcp_store:parse_cursor(Cursor, Tool, Hash) of
        {error, _} ->
            {error, <<"invalid_cursor">>, <<"the cursor is invalid for this tool, arguments or session">>};
        {ok, CollId, Offset} ->
            case mcp_store:fetch_collection(CollId, Tool, Hash) of
                {ok, #{meta := Meta, items := Items}} ->
                    render_page(Tool, Hash, CollId, Meta, Items, Offset, Ctx);
                {error, cursor_expired} ->
                    {error, <<"cursor_expired">>,
                     <<"the collection is no longer retained; call the tool again (results are not silently restarted)">>}
            end
    end.

render_page(Tool, Hash, CollId, Meta, Items, Offset, #{config := Config} = Ctx) ->
    Rest = safe_nthtail(Offset, Items),
    MaxItems = maps:get(page_size, Ctx),
    MaxResult = mcp_policy:limit(max_result_bytes, Config),
    fit_page(Tool, Hash, CollId, Meta, Rest, Offset, min(MaxItems, max(1, length(Rest))), MaxResult, Ctx).

safe_nthtail(N, L) when N >= length(L) -> [];
safe_nthtail(N, L) -> lists:nthtail(N, L).

fit_page(Tool, Hash, CollId, Meta, Rest, Offset, N, MaxResult, Ctx) ->
    Page = lists:sublist(Rest, N),
    Env = envelope(Tool, Hash, CollId, Meta, Page, Offset, length(Rest) > N, Ctx),
    {ok, Json0} = vscode_jsone:encode(Env),
    Json = iolist_to_binary(Json0),
    %% the response carries the structured result twice: as structuredContent and
    %% as escaped JSON text in content[0].text; plus the JSON-RPC framing
    {ok, Text} = vscode_jsone:encode(Json),
    case byte_size(Json) + iolist_size(Text) + 1024 =< MaxResult of
        true -> {ok, Env};
        false when N > 1 ->
            fit_page(Tool, Hash, CollId, Meta, Rest, Offset, N div 2, MaxResult, Ctx);
        false ->
            {error, <<"result_too_large">>,
             <<"a single item exceeds max_result_bytes; the policy limits are too small for this node">>}
    end.

envelope(Tool, Hash, CollId, Meta, Page, Offset, More, Ctx) ->
    Detail = maps:get(detail, Ctx, full),
    Entities0 = [E || {entity, E} <- Page],
    InPage = maps:from_list([{maps:get(<<"id">>, E), true} || E <- Entities0]),
    Rels0 = [R#{<<"fromInPage">> => maps:is_key(maps:get(<<"from">>, R), InPage),
                <<"toInPage">> => maps:is_key(maps:get(<<"to">>, R), InPage)}
             || {relationship, R} <- Page],
    {Entities, Rels, Scope} =
        case Detail of
            full -> {Entities0, Rels0, maps:get(scope, Meta)};
            summary -> {[compact_entity(E) || E <- Entities0],
                        [compact_rel(R) || R <- Rels0],
                        compact_scope(maps:get(scope, Meta), Offset)}
        end,
    Next = case More of
               true -> mcp_store:make_cursor(CollId, Tool, Hash, Offset + length(Page));
               false -> null
           end,
    Env = #{<<"schemaVersion">> => ?SV,
      <<"sessionId">> => mcp_store:session_id(),
      <<"collectionId">> => CollId,
      <<"startedAt">> => mcp_encoder:iso8601(maps:get(started_at, Meta)),
      <<"finishedAt">> => mcp_encoder:iso8601(maps:get(finished_at, Meta)),
      <<"scope">> => Scope,
      <<"detail">> => atom_to_binary(Detail, utf8),
      <<"entities">> => Entities,
      <<"relationships">> => Rels,
      <<"complete">> => maps:get(complete, Meta),
      <<"truncated">> => maps:get(truncated, Meta),
      <<"omissions">> => maps:get(omissions, Meta),
      <<"nextCursor">> => Next,
      <<"page">> => #{<<"offset">> => Offset, <<"items">> => length(Page)}},
    case maps:get(format, Ctx, json) of
        mermaid -> Env#{<<"mermaid">> => mermaid_graph(Entities, Rels)};
        json -> Env
    end.

%% A `graph TD` of one page, from the entities and relationships it returns. Labels
%% are reduced to a safe character set (they come from atoms of the debugged VM).
mermaid_graph(Entities, Rels) ->
    Known = maps:from_list([{maps:get(<<"id">>, E), E} || E <- Entities]),
    Missing = lists:usort([Id || R <- Rels, Id <- [maps:get(<<"from">>, R), maps:get(<<"to">>, R)],
                                 not maps:is_key(Id, Known)]),
    Nodes = [mermaid_node(E) || E <- Entities]
        ++ [[<<"    ">>, mermaid_id(Id), <<"[\"">>, mermaid_text(Id), <<"\"]\n">>] || Id <- Missing],
    Edges = [mermaid_edge(R) || R <- Rels],
    iolist_to_binary([<<"graph TD\n">>, Nodes, Edges]).

mermaid_node(E) ->
    Label = [mermaid_text(mermaid_label(E)),
             case mermaid_detail(E) of
                 <<>> -> [];
                 D -> [<<"<br/>">>, mermaid_text(D)]
             end],
    [<<"    ">>, mermaid_id(maps:get(<<"id">>, E)), <<"[\"">>, Label, <<"\"]\n">>].

mermaid_edge(#{<<"type">> := Type, <<"from">> := F, <<"to">> := T}) ->
    Arrow = case Type of
                <<"supervises">> -> <<"-->">>;
                _ -> <<"-.->">>
            end,
    [<<"    ">>, mermaid_id(F), <<" ">>, Arrow, <<"|">>, mermaid_text(Type), <<"| ">>, mermaid_id(T), <<"\n">>].

mermaid_label(E) ->
    first_binary([maps:get(K, E, null) || K <- [<<"name">>, <<"driver">>, <<"childId">>, <<"pid">>, <<"id">>]]).

mermaid_detail(E) ->
    Parts = [V || K <- [<<"role">>, <<"restart">>, <<"childState">>], V <- [maps:get(K, E, null)],
                  is_binary(V), V =/= <<"unknown">>],
    iolist_to_binary(lists:join(<<" / ">>, Parts)).

first_binary([B | _]) when is_binary(B) -> B;
first_binary([_ | T]) -> first_binary(T);
first_binary([]) -> <<"?">>.

mermaid_id(Id) -> [<<"n_">>, re:replace(Id, "[^A-Za-z0-9_]", "_", [global, {return, binary}])].

mermaid_text(Bin) ->
    Clean = re:replace(Bin, "[^A-Za-z0-9_ :/.@#-]", "_", [global, {return, binary}]),
    case byte_size(Clean) > 60 of
        true -> <<(binary:part(Clean, 0, 57))/binary, "...">>;
        false -> Clean
    end.

%% detail=summary: same entities and edges, without the fields an agent rarely
%% needs to reason about the topology (they stay available with detail=full).
compact_entity(E) ->
    E1 = maps:without([<<"description">>, <<"pid">>, <<"specAvailable">>, <<"metadataAvailable">>], E),
    maps:filter(fun(<<"alive">>, true) -> false;
                   (<<"application">>, null) -> false;
                   (<<"name">>, null) -> false;
                   (<<"includedApplications">>, []) -> false;
                   (<<"callbackModule">>, V) -> V =/= <<"unknown">> andalso V =/= null;
                   (<<"behaviour">>, <<"unknown">>) -> false;
                   (<<"modules">>, [M]) -> M =/= maps:get(<<"callbackModule">>, E, undefined);
                   (<<"childId">>, C) -> C =/= maps:get(<<"name">>, E, undefined);
                   (_, _) -> true
                end, E1).

%% the observation interval is the collection's startedAt/finishedAt
compact_rel(R) ->
    maps:filter(fun(<<"observedAt">>, _) -> false;
                   (<<"fromInPage">>, true) -> false;
                   (<<"toInPage">>, true) -> false;
                   (_, _) -> true
                end, R).

%% coverage limitations are stated once, on the first page
compact_scope(Scope, 0) -> Scope;
compact_scope(Scope, _) -> maps:remove(<<"limitations">>, Scope).

%%------------------------------------------------------------------------------
%% Identities and small entity helpers
%%------------------------------------------------------------------------------

app_id(App) -> mcp_store:entity_id(application, {application, App}).
pid_id(Pid) -> mcp_store:entity_id(process, {process, Pid}).
module_id(Mod) -> mcp_store:entity_id(module, {module, Mod}).

bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> <<"<invalid>">>
    end;
bin(B) when is_binary(B) -> B.

%% Modules that belong to the inspector are flagged, never presented as application processes.
inspector_name(Name) ->
    lists:member(Name, [mcp_holder, mcp_sup, mcp_server, mcp_store]).

-define(INSPECTOR_MODULES, [mcp_runtime, mcp_server, mcp_store, mcp_sup, mcp_audit, mcp_holder,
                            mcp_encoder, mcp_policy, mcp_tools]).

%% A process of the inspector itself (registered name, or running one of its modules:
%% workers and connections are anonymous funs of those modules).
is_inspector_pid(Pid) ->
    case erlang:process_info(Pid, [registered_name, initial_call, current_function]) of
        undefined -> false;
        Info -> inspector_info(Info)
    end.

inspector_info(Info) ->
    Named = case lists:keyfind(registered_name, 1, Info) of
                {registered_name, N} when is_atom(N) -> inspector_name(N);
                _ -> false
            end,
    Named orelse inspector_module(proplists:get_value(initial_call, Info))
        orelse inspector_module(proplists:get_value(current_function, Info)).

inspector_module({M, _, _}) -> lists:member(M, ?INSPECTOR_MODULES);
inspector_module(_) -> false.

%% Safe check that a pid is a supervisor before any supervisor API is called on
%% it (calling which_children on a plain gen_server would crash it).
is_supervisor(Pid) ->
    is_pid(Pid) andalso node(Pid) =:= node() andalso
        try proc_lib:initial_call(Pid) of
            {supervisor, _, _} -> true;
            _ -> false
        catch _:_ -> false
        end.

behaviours(Mod) when is_atom(Mod) ->
    case erlang:module_loaded(Mod) of
        true ->
            try
                [B || {Attr, Bs} <- Mod:module_info(attributes),
                      Attr =:= behaviour orelse Attr =:= behavior,
                      B <- Bs, is_atom(B)]
            of
                L -> [bin(B) || B <- L]
            catch _:_ -> unknown
            end;
        false -> unknown
    end;
behaviours(_) -> unknown.

behaviours_field(Mod) ->
    case behaviours(Mod) of
        unknown -> <<"unknown">>;
        L -> L
    end.

process_entity(Pid, Fields, B) ->
    Id = pid_id(Pid),
    Name = case erlang:process_info(Pid, registered_name) of
               {registered_name, N} -> bin(N);
               _ -> null
           end,
    E = maps:merge(#{<<"kind">> => <<"process">>,
                     <<"pid">> => mcp_encoder:pid_text(Pid),
                     <<"name">> => Name,
                     <<"alive">> => true}, Fields),
    {Id, note_pid(add_entity(B, Id, E), Pid)}.

%%------------------------------------------------------------------------------
%% application_overview
%%------------------------------------------------------------------------------

started_apps(Ctx) ->
    case bounded(fun() -> application:which_applications(1500) end, min(2000, max(1, remaining(Ctx)))) of
        {ok, L} when is_list(L) -> {ok, lists:keysort(1, L)};
        timeout -> timeout;
        _ -> error
    end.

%% -> {discovered, RootPid} | no_start_module | unavailable
root_info(App, Ctx) ->
    case application:get_key(App, mod) of
        {ok, {_Mod, _Args}} ->
            Res = bounded(fun() ->
                                  case application_controller:get_master(App) of
                                      Master when is_pid(Master) ->
                                          {Root, _} = application_master:get_child(Master),
                                          Root;
                                      _ -> undefined
                                  end
                          end, min(1000, max(1, remaining(Ctx)))),
            case Res of
                {ok, Root} when is_pid(Root) -> {discovered, Root};
                _ -> unavailable
            end;
        _ -> no_start_module
    end.

application_overview_build(Args, #{enc := Enc} = Ctx) ->
    IncludeModules0 = maps:get(<<"includeModules">>, Args, false),
    %% listing the modules of every application floods an agent: with
    %% detail=summary, modules are only listed for a selected application
    NodeWide = not maps:is_key(<<"application">>, Args),
    IncludeModules = IncludeModules0 andalso not (NodeWide andalso maps:get(detail, Ctx) =:= summary),
    case select_application(maps:get(<<"application">>, Args, undefined)) of
        {error, _, _} = E -> E;
        {ok, Selected} ->
            {Apps, B0} = case started_apps(Ctx) of
                             {ok, L} -> {L, new_b()};
                             timeout ->
                                 {[], add_omission(new_b(), <<"timeout">>, 1, undefined,
                                                   <<"application_controller did not answer in time">>, false)};
                             error ->
                                 {[], add_omission(new_b(), <<"unavailable_while_paused">>, 1, undefined,
                                                   <<"the started applications could not be listed">>, false)}
                         end,
            Names = [A || {A, _, _} <- Apps],
            Wanted = case Selected of
                         all -> Apps;
                         {app, Name} -> [T || {A, _, _} = T <- Apps, A =:= Name]
                     end,
            B1 = case {Selected, Wanted} of
                     {{app, _}, []} ->
                         add_omission(B0, <<"disappeared">>, 1, undefined,
                                      <<"the selected application is not started (anymore)">>, false);
                     _ -> B0
                 end,
            B2a = lists:foldl(fun({App, Desc, Vsn}, Acc) ->
                                      app_entities(App, Desc, Vsn, Names, IncludeModules, Enc, Ctx, Acc)
                              end, B1, Wanted),
            %% references to applications outside the listing (not started, or not selected):
            %% added last so that a real entity is never shadowed by a stub
            Refs = lists:usort(lists:append([case application:get_key(A, applications) of
                                                 {ok, D} when is_list(D) -> D;
                                                 _ -> []
                                             end || {A, _, _} <- Wanted])),
            B2 = lists:foldl(fun(Dep, Acc) ->
                                     Running = lists:member(Dep, Names),
                                     add_entity(Acc, app_id(Dep),
                                                #{<<"kind">> => <<"application">>, <<"name">> => bin(Dep),
                                                  <<"running">> => Running, <<"metadataAvailable">> => false})
                             end, B2a, Refs),
            B3 = case IncludeModules0 andalso not IncludeModules of
                     true -> add_omission(B2, <<"limit_reached">>, 1, undefined,
                                          <<"modules are listed for one application at a time: pass application=<id> (or detail=full)">>,
                                          false);
                     false -> B2
                 end,
            Scope = #{<<"tool">> => <<"application_overview">>,
                      <<"application">> => case Selected of all -> null; {app, N} -> app_id(N) end,
                      <<"limitations">> =>
                          [<<"Only started applications are listed; dependencies that are not started appear as running=false stubs.">>,
                           <<"Declared dependencies (depends_on) are declarations from the application resource, not evidence of runtime communication.">>,
                           <<"Root discovery uses the application master; rootStatus explains when no root is available.">>,
                           <<"Application environment values, code paths and callbacks are never read or invoked.">>]},
            {ok, Scope, B3}
    end.

select_application(undefined) -> {ok, all};
select_application(Id) ->
    case mcp_store:resolve_id(Id) of
        {ok, {application, Name}} -> {ok, {app, Name}};
        {ok, _} -> {error, <<"invalid_id">>, <<"the id does not designate an application">>};
        expired -> {error, <<"unknown_or_expired_id">>, <<"unknown or expired entity id; call application_overview again">>}
    end.

app_entities(App, Desc, Vsn, _StartedNames, IncludeModules, Enc, Ctx, B0) ->
    AppId = app_id(App),
    Mods = case application:get_key(App, modules) of {ok, M} when is_list(M) -> M; _ -> unavailable end,
    Deps = case application:get_key(App, applications) of {ok, D} when is_list(D) -> D; _ -> [] end,
    Included = case application:get_key(App, included_applications) of {ok, I} when is_list(I) -> I; _ -> [] end,
    StartMod = case application:get_key(App, mod) of {ok, {SM, _}} -> bin(SM); _ -> null end,
    {RootStatus, Roots, B1} = case root_info(App, Ctx) of
                                  {discovered, Root} ->
                                      Kind = case is_supervisor(Root) of
                                                 true -> <<"supervisor">>;
                                                 false -> <<"unknown">>
                                             end,
                                      {RId, Bx} = process_entity(Root, #{<<"role">> => Kind,
                                                                         <<"application">> => bin(App)}, B0),
                                      mcp_store:note_member(Root, App, mcp_store:now_ms()),
                                      Bx1 = add_rel(Bx, <<"belongs_to">>, RId, AppId,
                                                    <<"application master child">>, <<"confirmed">>),
                                      {<<"discovered">>, [RId], Bx1};
                                  no_start_module -> {<<"no_start_module">>, [], B0};
                                  unavailable -> {<<"unavailable">>, [], B0}
                              end,
    Entity = #{<<"kind">> => <<"application">>,
               <<"name">> => bin(App),
               <<"version">> => bin(Vsn),
               <<"description">> => mcp_encoder:safe_binary(unicode:characters_to_binary(Desc), 256),
               <<"running">> => true,
               <<"startModule">> => StartMod,
               <<"dependencies">> => [bin(D) || D <- Deps],
               <<"includedApplications">> => [bin(D) || D <- Included],
               <<"moduleCount">> => case Mods of unavailable -> null; _ -> length(Mods) end,
               <<"rootStatus">> => RootStatus,
               <<"roots">> => Roots},
    B2 = add_entity(B1, AppId, Entity),
    B3 = lists:foldl(
           fun(Dep, Acc) ->
                   add_rel(Acc, <<"depends_on">>, AppId, app_id(Dep),
                           <<"declared in the application resource (applications key)">>, <<"confirmed">>)
           end, B2, Deps),
    case {IncludeModules, Mods} of
        {true, L} when is_list(L) ->
            Max = maps:get(max_items, Enc),
            Shown = lists:sublist(lists:sort(L), Max),
            Bm = lists:foldl(
                   fun(Mod, Acc) ->
                           MId = module_id(Mod),
                           Acc1 = add_entity(Acc, MId, #{<<"kind">> => <<"module">>, <<"name">> => bin(Mod),
                                                         <<"loaded">> => erlang:module_loaded(Mod),
                                                         <<"behaviours">> => behaviours_field(Mod)}),
                           add_rel(Acc1, <<"belongs_to">>, MId, AppId,
                                   <<"application resource modules key">>, <<"confirmed">>)
                   end, B3, Shown),
            case length(L) > Max of
                true -> add_omission(Bm, <<"limit_reached">>, length(L) - Max, AppId,
                                     <<"module list truncated; use includeModules=false or a narrower scope">>, false);
                false -> Bm
            end;
        _ -> B3
    end.

%%------------------------------------------------------------------------------
%% supervision_tree
%%------------------------------------------------------------------------------

supervision_tree_build(Args, #{config := Config} = Ctx) ->
    MaxDepth = min(maps:get(<<"maxDepth">>, Args, 16), mcp_policy:limit(max_traversal_depth, Config)),
    IncludeModules = maps:get(<<"includeModules">>, Args, false),
    case tree_start(Args, Ctx) of
        {error, _, _} = E -> E;
        {ok, Roots, App, Scope0, B0} ->
            Cfg = #{max_depth => MaxDepth, include_modules => IncludeModules, app => App, ctx => Ctx},
            B1 = walk([{R, 0} || R <- Roots], Cfg, #{}, B0),
            Scope = Scope0#{<<"tool">> => <<"supervision_tree">>, <<"maxDepth">> => MaxDepth,
                            <<"limitations">> =>
                                [<<"Only 'supervises' edges from supervisor:which_children/1 are emitted; links and monitors are never supervision.">>,
                                 <<"Unsupervised, unregistered and transient processes are not part of this map.">>,
                                 <<"Child start arguments are never returned.">>]},
            {ok, Scope, B1}
    end.

%% -> {ok, [RootPid], AppName | undefined, Scope, Builder} | {error, Code, Msg}
tree_start(#{<<"id">> := Id}, Ctx) ->
    case mcp_store:resolve_id(Id) of
        {ok, {application, App}} ->
            case lists:keymember(App, 1, application:which_applications(1000)) of
                false -> {error, <<"not_found">>, <<"the application is not started (anymore)">>};
                true ->
                    B0 = new_b(),
                    AppId = app_id(App),
                    case root_info(App, Ctx) of
                        {discovered, Root} ->
                            {ok, [Root], App, #{<<"application">> => AppId}, B0};
                        no_start_module ->
                            {ok, [], App, #{<<"application">> => AppId},
                             add_omission(B0, <<"unknown_membership">>, 1, AppId,
                                          <<"library application without a start module: no root supervisor exists">>, false)};
                        unavailable ->
                            {ok, [], App, #{<<"application">> => AppId},
                             add_omission(B0, <<"timeout">>, 1, AppId,
                                          <<"the root supervisor could not be discovered">>, false)}
                    end
            end;
        {ok, {process, Pid}} ->
            start_from_pid(Pid, undefined);
        {ok, _} ->
            {error, <<"invalid_id">>, <<"the id designates neither an application nor a process">>};
        expired ->
            {error, <<"unknown_or_expired_id">>, <<"unknown or expired entity id; call application_overview again">>}
    end;
tree_start(#{<<"supervisor">> := Name}, _Ctx) ->
    case resolve_process(Name) of
        {ok, Pid} -> start_from_pid(Pid, undefined);
        {error, _, _} = E -> E
    end.

start_from_pid(Pid, App) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        false -> {error, <<"not_found">>, <<"the process does not exist (anymore)">>};
        true ->
            case is_supervisor(Pid) of
                false -> {error, <<"not_a_supervisor">>, <<"the process is not a supervisor">>};
                true -> {ok, [Pid], App, #{<<"process">> => pid_id(Pid)}, new_b()}
            end
    end.

walk([], _Cfg, _Visited, B) -> B;
walk([{Sup, Depth} | Rest], #{ctx := Ctx} = Cfg, Visited, B) ->
    case {maps:is_key(Sup, Visited), remaining(Ctx)} of
        {true, _} ->
            walk(Rest, Cfg, Visited, B);
        {false, 0} ->
            B1 = add_omission(B, <<"timeout">>, length(Rest) + 1, undefined,
                              <<"the time budget was exhausted before every supervisor was visited">>, false),
            B1;
        {false, _} ->
            {SupId, B1} = process_entity(Sup, #{<<"role">> => <<"supervisor">>,
                                                <<"application">> => app_text(Cfg)}, B),
            case children_of(Sup, Ctx) of
                {ok, Children, Counts} ->
                    B1c = merge_entity(B1, SupId, #{<<"children">> => counts_map(Counts)}),
                    {B2, Next} = lists:foldl(
                                   fun(C, {Acc, Q}) -> child(C, Sup, SupId, Depth, Cfg, Acc, Q) end,
                                   {B1c, []}, Children),
                    walk(Rest ++ lists:reverse(Next), Cfg, Visited#{Sup => true}, B2);
                {too_many, N, Counts} ->
                    B1c = merge_entity(B1, SupId, #{<<"children">> => counts_map(Counts)}),
                    B2 = add_omission(B1c, <<"limit_reached">>, N, SupId,
                                      <<"too many children to list safely; children not listed">>, false),
                    walk(Rest, Cfg, Visited#{Sup => true}, B2);
                timeout ->
                    B2 = case paused_at(Sup) of
                             {ok, {PM, PL}} ->
                                 add_omission(B1, <<"unavailable_while_paused">>, 1, SupId,
                                              iolist_to_binary(
                                                [<<"the supervisor is stopped at a breakpoint (">>, bin(PM), $:,
                                                 integer_to_binary(PL),
                                                 <<"); its children are missing from this map until it is resumed">>]),
                                              false);
                             _ ->
                                 add_omission(B1, <<"timeout">>, 1, SupId,
                                              <<"the supervisor did not answer in time; its children are missing from this map">>, false)
                         end,
                    walk(Rest, Cfg, Visited#{Sup => true}, B2);
                gone ->
                    B2 = add_omission(B1, <<"disappeared">>, 1, SupId,
                                      <<"the supervisor exited during the collection">>, false),
                    walk(Rest, Cfg, Visited#{Sup => true}, B2)
            end
    end.

app_text(#{app := undefined}) -> null;
app_text(#{app := App}) -> bin(App).

%% -> {ok, [{ChildId, Child, Type, Modules, SpecMeta}], Counts} | {too_many, N, Counts} | timeout | gone
children_of(Sup, Ctx) ->
    Fun = fun() ->
                  Counts = supervisor:count_children(Sup),
                  Total = proplists:get_value(specs, Counts, 0),
                  case Total > ?MAX_LISTED_CHILDREN of
                      true -> {too_many, Total, Counts};
                      false ->
                          Cs = supervisor:which_children(Sup),
                          {ok, with_specs(Sup, Cs, 0, []), Counts}
                  end
          end,
    %% one third of the request budget per supervisor: a blocked supervisor
    %% must not starve its siblings
    PerCall = max(100, maps:get(timeout_ms, Ctx) div 3),
    case bounded(Fun, min(remaining(Ctx), PerCall)) of
        {ok, R} -> R;
        timeout -> timeout;
        {error, _} -> gone
    end.

%% supervisor:count_children/1 as an entity field: how many child specs, running
%% children, supervisors and workers (never child arguments).
counts_map(Counts) ->
    maps:from_list([{atom_to_binary(K, utf8), V}
                    || K <- [specs, active, supervisors, workers],
                       {K2, V} <- Counts, K2 =:= K, is_integer(V)]).

with_specs(_Sup, [], _N, Acc) -> lists:reverse(Acc);
with_specs(Sup, [{Id, Child, Type, Mods} | T], N, Acc) ->
    Meta = case N < ?MAX_SPEC_LOOKUPS of
               true -> spec_meta(Sup, Id, Child);
               false -> unavailable
           end,
    with_specs(Sup, T, N + 1, [{Id, Child, Type, Mods, Meta} | Acc]);
with_specs(Sup, [_ | T], N, Acc) ->
    with_specs(Sup, T, N, Acc).

%% restart / shutdown / significant only - never the start MFA.
spec_meta(Sup, Id, Child) ->
    Key = case Id of undefined when is_pid(Child) -> Child; _ -> Id end,
    try supervisor:get_childspec(Sup, Key) of
        {ok, Spec} when is_map(Spec) ->
            #{restart => maps:get(restart, Spec, undefined),
              shutdown => maps:get(shutdown, Spec, undefined),
              significant => maps:get(significant, Spec, undefined)};
        {ok, Spec} when is_tuple(Spec), tuple_size(Spec) =:= 6 ->
            #{restart => element(4, Spec), shutdown => element(5, Spec), significant => undefined};
        _ -> unavailable
    catch _:_ -> unavailable
    end.

child({ChildId, Child, Type, Mods, Meta}, Sup, SupId, Depth, #{ctx := Ctx} = Cfg, B, Next) ->
    Enc = maps:get(enc, Ctx),
    Role = case Type of supervisor -> <<"supervisor">>; worker -> <<"worker">>; _ -> <<"unknown">> end,
    ChildText = mcp_encoder:text(ChildId, Enc),
    {ModNames, Callback} = module_names(Mods),
    Common = #{<<"role">> => Role,
               <<"childId">> => ChildText,
               <<"modules">> => ModNames,
               <<"callbackModule">> => Callback,
               <<"behaviour">> => case Callback of
                                      null -> <<"unknown">>;
                                      _ -> behaviours_from_name(Mods)
                                  end,
               <<"application">> => app_text(Cfg)},
    Common1 = add_meta(Common, Meta),
    {ChildEntId, B1} =
        case Child of
            P when is_pid(P) ->
                case Cfg of
                    #{app := App} when App =/= undefined ->
                        mcp_store:note_member(P, App, mcp_store:now_ms());
                    _ -> ok
                end,
                process_entity(P, Common1, B);
            Other ->
                State = case Other of restarting -> <<"restarting">>; _ -> <<"not_running">> end,
                Id = mcp_store:entity_id(process, {slot, Sup, ChildText}),
                E = Common1#{<<"kind">> => <<"process">>, <<"alive">> => false,
                             <<"childState">> => State, <<"pid">> => null, <<"name">> => null},
                {Id, add_entity(B, Id, E)}
        end,
    B2 = add_rel(B1, <<"supervises">>, SupId, ChildEntId,
                 <<"supervisor:which_children/1">>, <<"confirmed">>),
    B3 = case maps:get(include_modules, Cfg) of
             true -> module_edges(Mods, ChildEntId, B2);
             false -> B2
         end,
    Next1 = case {Child, Type} of
                {P2, supervisor} when is_pid(P2) ->
                    case Depth + 1 < maps:get(max_depth, Cfg) of
                        true -> [{P2, Depth + 1} | Next];
                        false -> Next
                    end;
                _ -> Next
            end,
    B4 = case {Child, Type} of
             {P3, supervisor} when is_pid(P3) ->
                 case Depth + 1 < maps:get(max_depth, Cfg) of
                     true -> B3;
                     false ->
                         add_omission(B3, <<"limit_reached">>, 1, ChildEntId,
                                      <<"depth limit reached; call supervision_tree with this id to expand the subtree">>,
                                      false)
                 end;
             _ -> B3
         end,
    {B4, Next1}.

add_meta(M, unavailable) -> M#{<<"specAvailable">> => false};
add_meta(M, #{restart := R, shutdown := S, significant := Sig}) ->
    M1 = M#{<<"specAvailable">> => true,
            <<"restart">> => spec_value(R),
            <<"shutdown">> => spec_value(S)},
    case Sig of
        undefined -> M1;
        _ -> M1#{<<"significant">> => Sig =:= true}
    end.

spec_value(undefined) -> <<"unknown">>;
spec_value(V) when is_atom(V) -> bin(V);
spec_value(V) when is_integer(V) -> V;
spec_value(_) -> <<"unknown">>.

module_names(dynamic) -> {<<"dynamic">>, null};
module_names([M]) when is_atom(M) -> {[bin(M)], bin(M)};
module_names(L) when is_list(L) -> {[bin(M) || M <- L, is_atom(M)], null};
module_names(_) -> {<<"unknown">>, null}.

behaviours_from_name([M]) when is_atom(M) -> behaviours_field(M);
behaviours_from_name(_) -> <<"unknown">>.

module_edges(Mods, EntId, B) when is_list(Mods) ->
    lists:foldl(
      fun(M, Acc) when is_atom(M) ->
              MId = module_id(M),
              Acc1 = add_entity(Acc, MId, #{<<"kind">> => <<"module">>, <<"name">> => bin(M),
                                            <<"loaded">> => erlang:module_loaded(M),
                                            <<"behaviours">> => behaviours_field(M)}),
              add_rel(Acc1, <<"uses_module">>, EntId, MId,
                      <<"supervisor child specification modules">>, <<"confirmed">>);
         (_, Acc) -> Acc
      end, B, Mods);
module_edges(_, _, B) -> B.

%%------------------------------------------------------------------------------
%% registered_processes
%%------------------------------------------------------------------------------

registered_processes_build(Args, #{enc := Enc, config := Config} = Ctx) ->
    case select_application(maps:get(<<"application">>, Args, undefined)) of
        {error, _, _} = E -> E;
        {ok, Selected} ->
            Cap = mcp_policy:limit(max_items, Config) * 4,
            All = [N || N <- registered(), not inspector_name(N)],
            Sorted = lists:sort(All),
            Shown = lists:sublist(Sorted, Cap),
            Masters = masters(Ctx),
            {Confirmed, B0} = case Selected of
                                  {app, App} -> cached_tree_pids(App, Ctx);
                                  all -> {#{}, new_b()}
                              end,
            {B1, Unknown} = lists:foldl(
                              fun(Name, {Acc, Unk}) ->
                                      reg_entry(Name, Selected, Confirmed, Masters, Enc, Acc, Unk)
                              end, {B0, 0}, Shown),
            B2 = case Unknown of
                     0 -> B1;
                     _ -> add_omission(B1, <<"unknown_membership">>, Unknown, undefined,
                                       <<"registered processes not attributable to the selected application were left out">>,
                                       false)
                 end,
            B3 = case length(Sorted) > Cap of
                     true -> add_omission(B2, <<"limit_reached">>, length(Sorted) - Cap, undefined,
                                          <<"registered process list truncated; filter by application">>, false);
                     false -> B2
                 end,
            Scope = #{<<"tool">> => <<"registered_processes">>,
                      <<"application">> => case Selected of all -> null; {app, A} -> app_id(A) end,
                      <<"limitations">> =>
                          [<<"Only locally registered names are listed; unregistered processes are not enumerated.">>,
                           <<"A registered name alone never confirms application membership.">>,
                           <<"Inspector processes are excluded.">>]},
            {ok, Scope, B3}
    end.

masters(Ctx) ->
    case started_apps(Ctx) of
        {ok, Apps} ->
            lists:foldl(
              fun({App, _, _}, Acc) ->
                      case catch application_controller:get_master(App) of
                          M when is_pid(M) -> Acc#{M => App};
                          _ -> Acc
                      end
              end, #{}, Apps);
        _ -> #{}
    end.

cached_tree_pids(_App, #{tree_pids := Known}) -> {Known, new_b()};
cached_tree_pids(App, Ctx) -> tree_pids(App, Ctx).

%% pids found in an application's supervision tree (confirmed membership)
tree_pids(App, #{config := Config} = Ctx) ->
    case root_info(App, Ctx) of
        {discovered, Root} ->
            Cfg = #{max_depth => mcp_policy:limit(max_traversal_depth, Config),
                    include_modules => false, app => App, ctx => Ctx},
            B = walk([{Root, 0}], Cfg, #{}, new_b()),
            Pids = (maps:get(pids, B))#{Root => true},
            {maps:map(fun(_, _) -> App end, Pids), new_b_with_oms(B)};
        _ ->
            {#{}, add_omission(new_b(), <<"unknown_membership">>, 1, app_id(App),
                               <<"no root supervisor is discoverable for this application">>, false)}
    end.

new_b_with_oms(B) -> (new_b())#{oms := maps:get(oms, B)}.

reg_entry(Name, Selected, Confirmed, Masters, Enc, B, Unknown) ->
    case whereis(Name) of
        undefined -> {B, Unknown};
        Pid ->
            {Conf, App, Evidence} = membership(Pid, Confirmed, Masters),
            Keep = case Selected of
                       all -> true;
                       {app, Wanted} -> App =:= Wanted andalso Conf =/= <<"unknown">>
                   end,
            case Keep of
                false -> {B, Unknown + 1};
                true ->
                    Info = erlang:process_info(Pid, [status, message_queue_len]),
                    Fields = #{<<"role">> => <<"unknown">>,
                               <<"status">> => bin(proplists:get_value(status, Info, unknown)),
                               <<"messageQueueLen">> => proplists:get_value(message_queue_len, Info, 0),
                               <<"application">> => case App of undefined -> null; _ -> bin(App) end,
                               <<"membership">> => #{<<"confidence">> => Conf, <<"evidence">> => Evidence}},
                    _ = Enc,
                    {_, B1} = process_entity(Pid, Fields, B),
                    {B1, Unknown}
            end
    end.

%% -> {Confidence, App | undefined, Evidence}
membership(Pid, Confirmed, Masters) ->
    case maps:find(Pid, Confirmed) of
        {ok, App} ->
            {<<"confirmed">>, App, <<"found in the supervision tree of the application">>};
        error ->
            case mcp_store:member(Pid) of
                {App, _} ->
                    {<<"confirmed">>, App, <<"seen in a supervision tree observed earlier in this session">>};
                _ ->
                    case erlang:process_info(Pid, group_leader) of
                        {group_leader, GL} ->
                            case maps:find(GL, Masters) of
                                {ok, App} ->
                                    {<<"inferred">>, App, <<"group leader is the application master (not proof of membership)">>};
                                error -> {<<"unknown">>, undefined, <<"no evidence">>}
                            end;
                        _ -> {<<"unknown">>, undefined, <<"no evidence">>}
                    end
            end
    end.

%%------------------------------------------------------------------------------
%% process_info
%%------------------------------------------------------------------------------

tool_process_info(Args, #{config := Config} = Ctx) ->
    Started = mcp_store:now_ms(),
    case resolve_target(Args) of
        {error, _, _} = E -> E;
        {ok, Pid} ->
            case is_process_alive(Pid) of
                false -> {error, <<"not_found">>, <<"the process does not exist (anymore)">>};
                true ->
                    Items = [status, current_function, initial_call, reductions, memory,
                             message_queue_len, links, monitors],
                    case erlang:process_info(Pid, Items) of
                        undefined -> {error, <<"not_found">>, <<"the process exited during the observation">>};
                        Info ->
                            build_process_info(Pid, Info, Started, Args, Ctx, Config)
                    end
            end
    end.

build_process_info(Pid, Info, Started, Args, #{enc := Enc} = Ctx, Config) ->
    Max = mcp_policy:limit(max_items, Config),
    {Conf, App, Evidence} = membership(Pid, #{}, masters(Ctx)),
    Fields = #{<<"role">> => <<"unknown">>,
               <<"status">> => bin(proplists:get_value(status, Info, unknown)),
               <<"currentFunction">> => mfa_text(proplists:get_value(current_function, Info)),
               <<"initialCall">> => mfa_text(proplists:get_value(initial_call, Info)),
               <<"reductions">> => proplists:get_value(reductions, Info, 0),
               <<"memory">> => proplists:get_value(memory, Info, 0),
               <<"memoryUnit">> => <<"bytes">>,
               <<"messageQueueLen">> => proplists:get_value(message_queue_len, Info, 0),
               <<"application">> => case App of undefined -> null; _ -> bin(App) end,
               <<"membership">> => #{<<"confidence">> => Conf, <<"evidence">> => Evidence},
               <<"callbackModule">> => <<"unknown">>,
               <<"behaviour">> => <<"unknown">>},
    Fields1 = case paused_at(Pid) of
                  {ok, {PM, PL}} ->
                      Fields#{<<"debugger">> => #{<<"status">> => <<"break">>,
                                                  <<"module">> => bin(PM), <<"line">> => PL}};
                  _ -> Fields
              end,
    {Id, B1} = process_entity(Pid, Fields1, new_b()),
    Links = [L || L <- proplists:get_value(links, Info, []), is_pid(L) orelse is_port(L)],
    Mons = [M || {process, M} <- proplists:get_value(monitors, Info, []), is_pid(M)],
    {B2, Omitted1} = peers(Links, <<"linked_to">>, <<"process_info links">>, Id, Max, B1),
    {B3, Omitted2} = peers(Mons, <<"monitors">>, <<"process_info monitors">>, Id, Max, B2),
    B4 = case Omitted1 + Omitted2 of
             0 -> B3;
             N -> add_omission(B3, <<"limit_reached">>, N, Id,
                               <<"links/monitors truncated to max_items">>, false)
         end,
    _ = Enc,
    Scope = #{<<"tool">> => <<"process_info">>, <<"process">> => Id,
              <<"limitations">> =>
                  [<<"Links and monitors do not imply supervision or message traffic.">>,
                   <<"Callback module and behaviour are only reported when supervisor child metadata is available (see supervision_tree).">>,
                   <<"Process dictionary, stack, mailbox and state are never read.">>,
                   <<"The debugger field is only present for a process the debugger reports stopped at a breakpoint.">>]},
    finish_graph(<<"process_info">>, Args, Scope, B4, Started, Ctx).

peers(Peers, Type, Evidence, FromId, Max, B) ->
    {Shown, Rest} = case length(Peers) > Max of
                        true -> lists:split(Max, Peers);
                        false -> {Peers, []}
                    end,
    B1 = lists:foldl(
           fun(P, Acc) when is_pid(P), node(P) =:= node() ->
                   case is_process_alive(P) of
                       true ->
                           {PId, Acc1} = process_entity(P, #{<<"role">> => <<"unknown">>}, Acc),
                           add_rel(Acc1, Type, FromId, PId, Evidence, <<"confirmed">>);
                       false -> Acc
                   end;
              (P, Acc) when is_port(P) ->
                   PId = mcp_store:entity_id(process, {port, P}),
                   Acc1 = add_entity(Acc, PId, #{<<"kind">> => <<"process">>, <<"role">> => <<"port">>,
                                                 <<"pid">> => null, <<"name">> => null, <<"alive">> => true}),
                   add_rel(Acc1, Type, FromId, PId, Evidence, <<"confirmed">>);
              (_, Acc) -> Acc
           end, B, Shown),
    {B1, length(Rest)}.

mfa_text({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    iolist_to_binary([atom_to_binary(M, utf8), ":", atom_to_binary(F, utf8), "/", integer_to_binary(A)]);
mfa_text(_) -> <<"unknown">>.

resolve_target(#{<<"id">> := Id}) ->
    case mcp_store:resolve_id(Id) of
        {ok, {process, Pid}} when is_pid(Pid) -> {ok, Pid};
        {ok, _} -> {error, <<"invalid_id">>, <<"the id does not designate a live process">>};
        expired -> {error, <<"unknown_or_expired_id">>, <<"unknown or expired entity id">>}
    end;
resolve_target(#{<<"name">> := Name}) -> resolve_process(Name);
resolve_target(#{<<"pid">> := Text}) -> resolve_process(Text).

%% Registered name (existing atoms only) or local pid text. Never creates atoms.
resolve_process(<<"<", _/binary>> = Text) ->
    case re:run(Text, "^<0\\.[0-9]{1,10}\\.[0-9]{1,10}>\\z", [{capture, none}]) of
        match ->
            try list_to_pid(binary_to_list(Text)) of
                Pid when node(Pid) =:= node() -> {ok, Pid}
            catch _:_ -> {error, <<"invalid_pid">>, <<"not a valid local pid">>}
            end;
        nomatch -> {error, <<"invalid_pid">>, <<"only local pids of this node are accepted">>}
    end;
resolve_process(Name) when is_binary(Name) ->
    try binary_to_existing_atom(Name, utf8) of
        Atom ->
            case whereis(Atom) of
                undefined -> {error, <<"not_found">>, <<"no process is registered under this name">>};
                Pid -> {ok, Pid}
            end
    catch _:_ -> {error, <<"not_found">>, <<"no process is registered under this name">>}
    end.

%%------------------------------------------------------------------------------
%% topology_overview: application + supervision tree + registered processes +
%% owned approved ETS tables of ONE application in a single collection. Each part is
%% included only when its own tool is allowed by the project policy.
%%------------------------------------------------------------------------------

topology_overview_build(Args, #{config := Config} = Ctx) ->
    case overview_application(Args, Ctx) of
        {error, _, _} = E -> E;
        {ok, App} ->
            AppId = app_id(App),
            Allowed = fun(Tool) -> mcp_policy:tool_allowed(Tool, Config) end,
            Part = fun(Tool, Build, PArgs, C) ->
                           case Allowed(Tool) of
                               true -> Build(PArgs, C);
                               false -> denied
                           end
                   end,
            Tree = Part(<<"supervision_tree">>, fun supervision_tree_build/2, #{<<"id">> => AppId}, Ctx),
            %% the registered-process part reuses the pids of the tree just walked
            RegCtx = case Tree of
                         {ok, _, TB} -> Ctx#{tree_pids => maps:map(fun(_, _) -> App end, maps:get(pids, TB))};
                         _ -> Ctx
                     end,
            Over = Part(<<"application_overview">>, fun application_overview_build/2, #{<<"application">> => AppId}, Ctx),
            Reg = Part(<<"registered_processes">>, fun registered_processes_build/2, #{<<"application">> => AppId}, RegCtx),
            Ets = Part(<<"ets_tables">>, fun ets_tables_build/2, #{}, Ctx),
            Parts = [{<<"supervision_tree">>, Tree}, {<<"application_overview">>, Over},
                     {<<"registered_processes">>, Reg}, {<<"ets_tables">>, Ets}],
            case [E || {_, {error, _, _} = E} <- Parts] of
                [Err | _] -> Err;
                [] -> merge_overview(AppId, Parts)
            end
    end.

%% -> {ok, App} | {error, Code, Msg}: an entity id or the name of a started application
overview_application(#{<<"application">> := Id}, _Ctx) ->
    case select_application(Id) of
        {ok, {app, App}} -> {ok, App};
        {error, _, _} = E -> E
    end;
overview_application(#{<<"name">> := Name}, Ctx) ->
    try binary_to_existing_atom(Name, utf8) of
        App ->
            case started_apps(Ctx) of
                {ok, Apps} ->
                    case lists:keymember(App, 1, Apps) of
                        true -> {ok, App};
                        false -> {error, <<"not_found">>, <<"no started application has this name">>}
                    end;
                _ -> {error, <<"timeout">>, <<"application_controller did not answer in time">>}
            end
    catch _:_ -> {error, <<"not_found">>, <<"no started application has this name">>}
    end.

%% The richest entity wins (the tree describes a supervisor better than the overview
%% does), so the tree is merged first; ETS tables only when their owner is in the map.
merge_overview(AppId, Parts) ->
    Order = [<<"supervision_tree">>, <<"application_overview">>, <<"registered_processes">>],
    Merged = lists:foldl(
               fun(Tool, Acc) ->
                       case proplists:get_value(Tool, Parts) of
                           {ok, _, B} -> merge_builders(Acc, B);
                           _ -> Acc
                       end
               end, new_b(), Order),
    WithEts = case proplists:get_value(<<"ets_tables">>, Parts) of
                  {ok, _, EtsB} -> merge_owned_tables(Merged, EtsB);
                  _ -> Merged
              end,
    Denied = [Tool || {Tool, denied} <- Parts],
    B1 = lists:foldl(
           fun(Tool, Acc) ->
                   add_omission(Acc, <<"policy_denied">>, 1, undefined,
                                <<"part not included: the project policy does not allow ", Tool/binary>>, false)
           end, WithEts, Denied),
    Included = [Tool || {Tool, {ok, _, _}} <- Parts],
    Limitations = lists:usort(lists:append([maps:get(<<"limitations">>, Sc, []) || {_, {ok, Sc, _}} <- Parts])),
    Scope = #{<<"tool">> => <<"topology_overview">>,
              <<"application">> => AppId,
              <<"parts">> => Included,
              <<"limitations">> =>
                  [<<"One collection assembled from several observations made in sequence: it is an interval, not an atomic snapshot.">>
                   | Limitations]},
    {ok, Scope, B1}.

merge_builders(Into, From) ->
    B1 = lists:foldl(fun(E, Acc) -> add_entity(Acc, maps:get(<<"id">>, E), maps:remove(<<"id">>, E)) end,
                     Into, lists:reverse(maps:get(ents, From))),
    B2 = lists:foldl(fun(#{<<"type">> := T, <<"from">> := F, <<"to">> := To} = R, #{rel_keys := Keys, rels := Rs} = Acc) ->
                             Key = {T, F, To},
                             case maps:is_key(Key, Keys) of
                                 true -> Acc;
                                 false -> Acc#{rel_keys := Keys#{Key => true}, rels := [R | Rs]}
                             end
                     end, B1, lists:reverse(maps:get(rels, From))),
    B3 = B2#{oms := maps:get(oms, B2) ++ maps:get(oms, From)},
    B3#{pids := maps:merge(maps:get(pids, B3), maps:get(pids, From))}.

%% keep an approved table (and its owns_table edge) only when its owner is already in the map
merge_owned_tables(B, EtsB) ->
    Have = maps:get(ent_ids, B),
    Tables = maps:from_list([{maps:get(<<"id">>, E), E} || E <- maps:get(ents, EtsB),
                                                          maps:get(<<"kind">>, E) =:= <<"ets_table">>]),
    Owned = [R || #{<<"type">> := <<"owns_table">>, <<"from">> := F, <<"to">> := T} = R <- lists:reverse(maps:get(rels, EtsB)),
                  maps:is_key(F, Have), maps:is_key(T, Tables)],
    lists:foldl(fun(#{<<"to">> := T} = R, Acc) ->
                        Acc1 = add_entity(Acc, T, maps:remove(<<"id">>, maps:get(T, Tables))),
                        add_rel(Acc1, <<"owns_table">>, maps:get(<<"from">>, R), T,
                                maps:get(<<"evidence">>, R), maps:get(<<"confidence">>, R))
                end, B, Owned).

%%------------------------------------------------------------------------------
%% changes_since: what differs between a retained collection (the baseline) and a
%% fresh observation of the same tool and arguments. Identity is the entity id, so a
%% restarted process is `replaced` (same logical child, new id), never "the same".
%% Absence from a partial collection is not proof of termination: such removals are
%% marked `unconfirmed`. The baseline must still be retained (collection_ttl_ms).
%%------------------------------------------------------------------------------

-define(DIFFABLE, [<<"application_overview">>, <<"supervision_tree">>, <<"registered_processes">>,
                   <<"ets_tables">>, <<"top_processes">>, <<"topology_overview">>]).

changes_since_build(Args, #{config := Config} = Ctx) ->
    case mcp_store:fetch_collection_by_id(maps:get(<<"collectionId">>, Args)) of
        {error, _} ->
            {error, <<"baseline_expired">>,
             <<"the baseline collection is unknown or no longer retained (collection_ttl_ms, max_collections); "
               "call the tool again to get a new baseline">>};
        {ok, #{tool := OldTool, ctx := #{args := OldArgs} = OldCtx, meta := OldMeta, items := OldItems}} ->
            case lists:member(OldTool, ?DIFFABLE) andalso mcp_policy:tool_allowed(OldTool, Config) of
                false ->
                    {error, <<"invalid_baseline">>, <<"this collection cannot be used as a baseline">>};
                true ->
                    %% a fresh observation that is compared, not retained: only the diff takes a slot
                    %% observe again exactly as the baseline was observed (same detail)
                    case (builder(OldTool))(OldArgs, Ctx#{detail => maps:get(detail, OldCtx, summary)}) of
                        {ok, _Scope, NB} ->
                            Oms = lists:reverse(maps:get(oms, NB)),
                            NewMeta = #{complete => Oms =:= [], omissions => Oms},
                            diff_collections(OldTool, maps:get(<<"collectionId">>, Args),
                                             OldMeta, OldItems, NewMeta, builder_items(NB));
                        {error, _, _} = E -> E
                    end
            end;
        {ok, _} ->
            {error, <<"invalid_baseline">>, <<"this collection cannot be used as a baseline">>}
    end.

diff_collections(Tool, BaseId, OldMeta, OldItems, NewMeta, NewItems) ->
    Old = [E || {entity, E} <- OldItems],
    New = [E || {entity, E} <- NewItems],
    OldIds = maps:from_list([{maps:get(<<"id">>, E), true} || E <- Old]),
    NewIds = maps:from_list([{maps:get(<<"id">>, E), true} || E <- New]),
    Removed0 = [E || E <- Old, not maps:is_key(maps:get(<<"id">>, E), NewIds)],
    Added0 = [E || E <- New, not maps:is_key(maps:get(<<"id">>, E), OldIds)],
    RemByKey = by_key(Removed0),
    AddByKey = by_key(Added0),
    %% a logical child that vanished once and appeared once under a new id was replaced
    Pairs = [{R, A} || {Key, [R]} <- maps:to_list(RemByKey), {ok, [A]} <- [maps:find(Key, AddByKey)]],
    ReplacedOld = maps:from_list([{maps:get(<<"id">>, R), true} || {R, _} <- Pairs]),
    ReplacedNew = maps:from_list([{maps:get(<<"id">>, A), maps:get(<<"id">>, R)} || {R, A} <- Pairs]),
    Removed = [E || E <- Removed0, not maps:is_key(maps:get(<<"id">>, E), ReplacedOld)],
    Added = [E || E <- Added0, not maps:is_key(maps:get(<<"id">>, E), ReplacedNew)],
    Absence = case maps:get(complete, NewMeta) of
                  true -> <<"observed">>;
                  false -> <<"unconfirmed">>
              end,
    Changed = [{A, #{<<"change">> => <<"added">>}} || A <- Added]
        ++ [{A, #{<<"change">> => <<"replaced">>, <<"previousId">> => maps:get(maps:get(<<"id">>, A), ReplacedNew)}}
            || {_, A} <- Pairs]
        ++ [{R, #{<<"change">> => <<"removed">>, <<"absence">> => Absence}} || R <- Removed],
    B0 = lists:foldl(fun({E, Mark}, Acc) ->
                             add_entity(Acc, maps:get(<<"id">>, E), maps:merge(maps:remove(<<"id">>, E), Mark))
                     end, new_b(), Changed),
    %% the edges of the fresh observation that lead to a new or replaced entity
    Fresh = maps:from_list([{maps:get(<<"id">>, A), true} || A <- Added] ++ [{I, true} || I <- maps:keys(ReplacedNew)]),
    B1 = lists:foldl(
           fun({relationship, #{<<"type">> := T, <<"from">> := F, <<"to">> := To} = R}, Acc) ->
                   case maps:is_key(To, Fresh) of
                       true -> add_rel(Acc, T, F, To, maps:get(<<"evidence">>, R), maps:get(<<"confidence">>, R));
                       false -> Acc
                   end;
              (_, Acc) -> Acc
           end, B0, NewItems),
    Unchanged = length([E || E <- New, maps:is_key(maps:get(<<"id">>, E), OldIds)]),
    B2 = B1#{oms := lists:reverse(maps:get(omissions, NewMeta, [])) ++ maps:get(oms, B1)},
    B3 = case maps:get(complete, OldMeta) of
             true -> B2;
             false -> add_omission(B2, <<"limit_reached">>, 1, undefined,
                                   <<"the baseline collection was partial: entities reported as added may already have existed">>,
                                   false)
         end,
    Scope = #{<<"tool">> => <<"changes_since">>,
              <<"baselineCollectionId">> => BaseId,
              <<"baselineTool">> => Tool,
              <<"baselineFinishedAt">> => mcp_encoder:iso8601(maps:get(finished_at, OldMeta)),
              <<"summary">> => #{<<"added">> => length(Added), <<"removed">> => length(Removed),
                                 <<"replaced">> => length(Pairs), <<"unchanged">> => Unchanged},
              <<"limitations">> =>
                  [<<"Compares two observation intervals, not atomic snapshots; the entities of this answer are only those that changed.">>,
                   <<"replaced = the same logical child (name or child id) now has a new id, i.e. it was restarted or recreated.">>,
                   <<"A removal from a partial collection is marked absence=unconfirmed: absence is not proof of termination.">>,
                   <<"The baseline must still be retained (collection_ttl_ms, max_collections).">>]},
    {ok, Scope, B3}.

%% logical identity of a child across restarts: its registered name, else its child id;
%% only keys that are unique on their side can identify a replacement
by_key(Ents) ->
    Keyed = [{K, E} || E <- Ents, K <- [change_key(E)], K =/= undefined],
    lists:foldl(fun({K, E}, Acc) -> maps:update_with(K, fun(L) -> L ++ [E] end, [E], Acc) end, #{}, Keyed).

change_key(#{<<"kind">> := Kind, <<"name">> := N}) when is_binary(N) -> {Kind, name, N};
change_key(#{<<"kind">> := Kind, <<"childId">> := C}) when is_binary(C) -> {Kind, child, C};
change_key(_) -> undefined.

%%------------------------------------------------------------------------------
%% Developer tier (never enabled by default; the project names them in allowed_tools):
%% bounded samples of application data. Values go through mcp_encoder: depth/item/size
%% limits, values of secret-looking keys replaced, credentials in URLs scrubbed.
%%------------------------------------------------------------------------------

%% A live, local, non-inspector process addressed by id, name or pid.
dev_target(Args) ->
    case resolve_target(Args) of
        {error, _, _} = E -> E;
        {ok, Pid} ->
            case is_process_alive(Pid) andalso not is_inspector_pid(Pid) of
                true -> {ok, Pid};
                false -> {error, <<"not_found">>, <<"the process does not exist (anymore)">>}
            end
    end.

dev_header(Pid) ->
    #{<<"schemaVersion">> => ?SV,
      <<"sessionId">> => mcp_store:session_id(),
      <<"observedAt">> => now_iso(),
      <<"process">> => pid_id(Pid),
      <<"pid">> => mcp_encoder:pid_text(Pid),
      <<"name">> => case erlang:process_info(Pid, registered_name) of
                        {registered_name, N} -> bin(N);
                        _ -> null
                    end}.

%% sys:get_state/2 of a gen_server, gen_statem or gen_event (a system message is only sent to a
%% process whose callback module declares one of those behaviours; never a supervisor).
process_state(Args, #{enc := Enc} = Ctx) ->
    case dev_target(Args) of
        {error, _, _} = E -> E;
        {ok, Pid} ->
            case otp_behaviour(Pid) of
                undefined ->
                    {error, <<"not_an_otp_process">>,
                     <<"the state is only read from gen_server, gen_statem and gen_event processes">>};
                {supervisor, _} ->
                    %% the state of a supervisor holds the child specifications, start arguments included
                    {error, <<"use_supervision_tree">>,
                     <<"the state of a supervisor contains child start arguments; use supervision_tree">>};
                {Behaviour, Callback} ->
                    T = max(100, min(2000, remaining(Ctx))),
                    case bounded(fun() -> sys:get_state(Pid, T) end, T + 200) of
                        {ok, State} ->
                            {ok, (dev_header(Pid))#{<<"behaviour">> => bin(Behaviour),
                                                    <<"callbackModule">> => bin(Callback),
                                                    <<"state">> => mcp_encoder:term(State, Enc),
                                                    <<"redaction">> => <<"best effort: values of keys named like a secret and credentials in URLs are replaced; a record without keys cannot be redacted">>}};
                        timeout ->
                            {error, <<"timeout">>,
                             case paused_at(Pid) of
                                 {ok, {M, L}} -> <<"the process is stopped at a breakpoint (", (bin(M))/binary, ":",
                                                   (integer_to_binary(L))/binary, ")">>;
                                 _ -> <<"the process did not answer in time">>
                             end};
                        {error, _} ->
                            {error, <<"unavailable">>, <<"the process exited or does not answer system messages">>}
                    end
            end
    end.

%% {Behaviour, CallbackModule} | undefined
otp_behaviour(Pid) ->
    case catch proc_lib:initial_call(Pid) of
        {supervisor, Mod, _} when is_atom(Mod) -> {supervisor, Mod};
        {Mod, _, _} when is_atom(Mod) ->
            case behaviours(Mod) of
                L when is_list(L) ->
                    case [B || B <- [gen_server, gen_statem, gen_event, gen_fsm, supervisor],
                               lists:member(atom_to_binary(B, utf8), L)] of
                        [B | _] -> {B, Mod};
                        [] -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end.

%% The oldest messages of a mailbox (default 5, max 20), bounded and encoded.
mailbox_sample(Args, #{enc := Enc} = Ctx) ->
    case dev_target(Args) of
        {error, _, _} = E -> E;
        {ok, Pid} ->
            Limit = min(maps:get(<<"limit">>, Args, 5), ?MAX_SAMPLE),
            Len = case erlang:process_info(Pid, message_queue_len) of
                      {message_queue_len, N} -> N;
                      _ -> 0
                  end,
            Head = (dev_header(Pid))#{<<"queueLength">> => Len, <<"order">> => <<"oldest first">>},
            case Len > ?MAX_MAILBOX_COPY of
                true ->
                    {ok, Head#{<<"sample">> => [], <<"truncated">> => true,
                               <<"note">> => <<"the mailbox is too large to copy safely; only its length is reported">>}};
                false ->
                    T = max(100, min(2000, remaining(Ctx))),
                    case bounded(fun() -> erlang:process_info(Pid, messages) end, T) of
                        {ok, {messages, Msgs}} ->
                            {ok, Head#{<<"sample">> => [mcp_encoder:term(M, Enc) || M <- lists:sublist(Msgs, Limit)],
                                       <<"truncated">> => length(Msgs) > Limit,
                                       <<"redaction">> => <<"best effort: values of keys named like a secret and credentials in URLs are replaced">>}};
                        timeout -> {error, <<"timeout">>, <<"the process did not answer in time">>};
                        _ -> {error, <<"not_found">>, <<"the process exited during the observation">>}
                    end
            end
    end.

%% A few objects of an approved named table (allowed_ets_tables), never of a private one.
ets_sample(Args, #{enc := Enc, config := Config} = Ctx) ->
    Name = maps:get(<<"table">>, Args),
    Limit = min(maps:get(<<"limit">>, Args, 5), ?MAX_SAMPLE),
    case mcp_policy:ets_table_allowed(Name, Config) of
        false -> {error, <<"policy_denied">>, <<"the table is not approved by allowed_ets_tables">>};
        true ->
            Atom = try binary_to_existing_atom(Name, utf8) catch _:_ -> undefined end,
            Prot = case Atom of undefined -> undefined; _ -> catch ets:info(Atom, protection) end,
            case Prot of
                P when P =:= public; P =:= protected ->
                    T = max(100, min(2000, remaining(Ctx))),
                    case bounded(fun() ->
                                         Objs = case ets:match_object(Atom, '_', Limit) of
                                                    {O, _Cont} -> O;
                                                    '$end_of_table' -> []
                                                end,
                                         {Objs, ets:info(Atom, size), ets:info(Atom, type)}
                                 end, T) of
                        {ok, {Objects, Size, Type}} ->
                            {ok, #{<<"schemaVersion">> => ?SV,
                                   <<"sessionId">> => mcp_store:session_id(),
                                   <<"observedAt">> => now_iso(),
                                   <<"table">> => Name,
                                   <<"protection">> => bin(P),
                                   <<"type">> => bin(Type),
                                   <<"size">> => Size,
                                   <<"sample">> => [mcp_encoder:term(O, Enc) || O <- Objects],
                                   <<"truncated">> => Size > length(Objects),
                                   <<"redaction">> => <<"best effort: values of keys named like a secret and credentials in URLs are replaced">>}};
                        timeout -> {error, <<"timeout">>, <<"the table did not answer in time">>};
                        _ -> {error, <<"not_found">>, <<"the table does not exist (anymore)">>}
                    end;
                private -> {error, <<"policy_denied">>, <<"private tables are never read">>};
                _ -> {error, <<"not_found">>, <<"no table with this name exists">>}
            end
    end.

%%------------------------------------------------------------------------------
%% top_processes (ranking by queue length / reductions / memory)
%%------------------------------------------------------------------------------

top_processes_build(Args, Ctx) ->
    SortBy = sort_item(maps:get(<<"sortBy">>, Args, <<"message_queue_len">>)),
    Limit = min(maps:get(<<"limit">>, Args, 10), ?MAX_TOP),
    All = erlang:processes(),
    Total = length(All),
    {Scan, Cut} = case Total > ?MAX_SCANNED of
                      true -> {lists:sublist(All, ?MAX_SCANNED), Total - ?MAX_SCANNED};
                      false -> {All, 0}
                  end,
    {Keyed, Unscanned} = rank_keys(Scan, SortBy, self(), Ctx, 0, []),
    %% a few spare candidates: inspector processes are filtered out below
    Ranked = lists:sublist(lists:reverse(lists:sort(Keyed)), Limit + 8),
    Masters = masters(Ctx),
    {B1, Gone, _} = lists:foldl(
                      fun({_, Pid}, {Acc, G, Rank}) when Rank =< Limit ->
                              case top_entry(Pid, SortBy, Rank, Masters, Acc) of
                                  skip -> {Acc, G, Rank};
                                  gone -> {Acc, G + 1, Rank};
                                  {ok, Acc1} -> {Acc1, G, Rank + 1}
                              end;
                         (_, Done) -> Done
                      end, {new_b(), 0, 1}, Ranked),
    B2 = case Gone of
             0 -> B1;
             _ -> add_omission(B1, <<"disappeared">>, Gone, undefined,
                               <<"ranked processes that exited before they could be described">>, false)
         end,
    B3 = case Unscanned of
             0 -> B2;
             _ -> add_omission(B2, <<"timeout">>, Unscanned, undefined,
                               <<"the time budget ended the scan; the ranking only covers the processes scanned so far">>, false)
         end,
    B4 = case Cut of
             0 -> B3;
             _ -> add_omission(B3, <<"limit_reached">>, Cut, undefined,
                               <<"more processes than the scan limit; the ranking only covers the first ones">>, false)
         end,
    Scope = #{<<"tool">> => <<"top_processes">>,
              <<"sortBy">> => atom_to_binary(SortBy, utf8),
              <<"limit">> => Limit,
              <<"processCount">> => Total,
              <<"limitations">> =>
                  [<<"A point-in-time scan, not an atomic snapshot: processes start and exit while it runs.">>,
                   <<"reductions are cumulative since the process started, not a rate; memory is in bytes.">>,
                   <<"Only metadata is returned: never mailbox, dictionary, stack or state.">>,
                   <<"Inspector processes are excluded.">>]},
    {ok, Scope, B4}.

sort_item(<<"reductions">>) -> reductions;
sort_item(<<"memory">>) -> memory;
sort_item(_) -> message_queue_len.

%% -> {[{Value, Pid}], NotScanned}: stops early when the time budget is used up
rank_keys([], _Item, _Self, _Ctx, _N, Acc) -> {Acc, 0};
rank_keys([Pid | T] = Pids, Item, Self, Ctx, N, Acc) ->
    case N rem 1000 =:= 0 andalso N > 0 andalso remaining(Ctx) =:= 0 of
        true -> {Acc, length(Pids)};
        false ->
            Acc1 = case Pid =/= Self andalso rank_value(Pid, Item) of
                       {Item, V} when is_integer(V) -> [{V, Pid} | Acc];
                       _ -> Acc
                   end,
            rank_keys(T, Item, Self, Ctx, N + 1, Acc1)
    end.

rank_value(Port, Item) when is_port(Port) -> erlang:port_info(Port, Item);
rank_value(Pid, Item) -> erlang:process_info(Pid, Item).

top_entry(Pid, SortBy, Rank, Masters, B) ->
    Items = [registered_name, status, current_function, initial_call, reductions, memory, message_queue_len],
    case erlang:process_info(Pid, Items) of
        undefined -> gone;
        Info ->
            case inspector_info(Info) of
                true -> skip;
                false -> top_entity(Pid, Info, SortBy, Rank, Masters, B)
            end
    end.

top_entity(Pid, Info, SortBy, Rank, Masters, B) ->
    {Conf, App, Evidence} = membership(Pid, #{}, Masters),
    Fields = #{<<"role">> => <<"unknown">>,
               <<"rank">> => Rank,
               <<"rankedBy">> => atom_to_binary(SortBy, utf8),
               <<"status">> => bin(proplists:get_value(status, Info, unknown)),
               <<"currentFunction">> => mfa_text(proplists:get_value(current_function, Info)),
               <<"initialCall">> => mfa_text(proplists:get_value(initial_call, Info)),
               <<"reductions">> => proplists:get_value(reductions, Info, 0),
               <<"memory">> => proplists:get_value(memory, Info, 0),
               <<"memoryUnit">> => <<"bytes">>,
               <<"messageQueueLen">> => proplists:get_value(message_queue_len, Info, 0),
               <<"application">> => case App of undefined -> null; _ -> bin(App) end,
               <<"membership">> => #{<<"confidence">> => Conf, <<"evidence">> => Evidence}},
    {_, B1} = process_entity(Pid, Fields, B),
    {ok, B1}.

%%------------------------------------------------------------------------------
%% process_groups: every local process aggregated by its initial call. The scan keeps
%% one counter per group (never per-process data), so memory is O(groups) and the answer
%% is O(limit) whether the node has 400 or 400000 processes. Finds fan-out and leaks
%% (thousands of anonymous workers) that a top-N list of single processes never shows.
%%------------------------------------------------------------------------------

process_groups(Args, #{enc := Enc} = Ctx) ->
    Started = mcp_store:now_ms(),
    SortBy = case maps:get(<<"sortBy">>, Args, <<"count">>) of
                 <<"memory">> -> memory;
                 <<"reductions">> -> reductions;
                 <<"message_queue_len">> -> queue;
                 _ -> count
             end,
    Limit = min(maps:get(<<"limit">>, Args, 10), ?MAX_TOP),
    All = erlang:processes(),
    Total = length(All),
    {Scan, Cut} = case Total > ?MAX_SCANNED of
                      true -> {lists:sublist(All, ?MAX_SCANNED), Total - ?MAX_SCANNED};
                      false -> {All, 0}
                  end,
    Masters = masters(Ctx),
    Acc0 = #{groups => #{}, scanned => 0, registered => 0, memory => 0, other => 0},
    {Acc, Unscanned} = group_scan(Scan, self(), Masters, Ctx, 0, Acc0),
    #{groups := Groups, scanned := Scanned, registered := Registered, memory := TotalMem, other := Other} = Acc,
    Ranked = lists:sublist(lists:sort(fun(A, B) -> group_rank(SortBy, A) >= group_rank(SortBy, B) end,
                                      [G#{key => K} || {K, G} <- maps:to_list(Groups)]), Limit),
    Shown = [group_json(G, I, Scanned, TotalMem, Enc) || {I, G} <- lists:zip(lists:seq(1, length(Ranked)), Ranked)],
    CoveredCount = lists:sum([maps:get(count, G) || G <- Ranked]),
    Oms = [#{<<"reason">> => <<"timeout">>, <<"count">> => Unscanned, <<"resumable">> => false,
             <<"message">> => <<"the time budget ended the scan; the groups only cover the processes scanned so far">>}
           || Unscanned > 0]
        ++ [#{<<"reason">> => <<"limit_reached">>, <<"count">> => Cut, <<"resumable">> => false,
              <<"message">> => <<"more processes than the scan limit; the groups only cover the first ones">>}
            || Cut > 0]
        ++ [#{<<"reason">> => <<"limit_reached">>, <<"count">> => Other, <<"resumable">> => false,
              <<"message">> => <<"more distinct initial calls than the group limit; the rest are counted in <other>">>}
            || Other > 0],
    {ok, #{<<"schemaVersion">> => ?SV,
           <<"sessionId">> => mcp_store:session_id(),
           <<"observedAt">> => mcp_encoder:iso8601(Started),
           <<"sortBy">> => atom_to_binary(case SortBy of queue -> message_queue_len; _ -> SortBy end, utf8),
           <<"totals">> => #{<<"processCount">> => Total, <<"scanned">> => Scanned,
                             <<"registered">> => Registered, <<"unregistered">> => Scanned - Registered,
                             <<"groups">> => maps:size(Groups), <<"groupsShown">> => length(Shown),
                             <<"shownCoverPercent">> => percent(CoveredCount, Scanned),
                             <<"memoryBytes">> => TotalMem},
           <<"groups">> => Shown,
           <<"complete">> => Oms =:= [],
           <<"truncated">> => Cut > 0 orelse Other > 0,
           <<"omissions">> => Oms,
           <<"limitations">> =>
               [<<"Grouped by initial call (the callback module's init for OTP processes): a group is where processes were started, not a dependency.">>,
                <<"A point-in-time scan, not an atomic snapshot: processes start and exit while it runs.">>,
                <<"reductions are cumulative since the process started; memory is in bytes.">>,
                <<"Only metadata is read: never mailbox, dictionary contents, stack or state. Inspector processes are excluded.">>]}}.

group_scan([], _Self, _Masters, _Ctx, _N, Acc) -> {Acc, 0};
group_scan([Pid | T] = Pids, Self, Masters, Ctx, N, Acc) ->
    case N rem 1000 =:= 0 andalso N > 0 andalso remaining(Ctx) =:= 0 of
        true -> {Acc, length(Pids)};
        false ->
            Acc1 = case Pid =/= Self andalso
                       erlang:process_info(Pid, [registered_name, initial_call, current_function, memory,
                                                 reductions, message_queue_len, group_leader]) of
                       Info when is_list(Info) ->
                           case inspector_info(Info) of
                               true -> Acc;
                               false -> group_add(Pid, Info, Masters, Acc)
                           end;
                       _ -> Acc
                   end,
            group_scan(T, Self, Masters, Ctx, N + 1, Acc1)
    end.

group_add(Pid, Info, Masters, #{groups := Groups, scanned := S, registered := R, memory := M, other := O} = Acc) ->
    Key0 = group_key(Pid, Info),
    {Key, Cap} = case maps:is_key(Key0, Groups) orelse maps:size(Groups) < ?MAX_GROUPS of
                     true -> {Key0, 0};
                     false -> {other, 1}
                 end,
    Reg = case lists:keyfind(registered_name, 1, Info) of
              {registered_name, N} when is_atom(N), N =/= [] -> 1;
              _ -> 0
          end,
    Mem = proplists:get_value(memory, Info, 0),
    App = case maps:find(proplists:get_value(group_leader, Info), Masters) of
              {ok, A} -> A;
              error -> undefined
          end,
    G0 = maps:get(Key, Groups, #{count => 0, memory => 0, reductions => 0, queue => 0, registered => 0,
                                 apps => #{}, samples => []}),
    #{count := C, memory := GM, reductions := GR, queue := GQ, registered := GReg, apps := Apps, samples := Sm} = G0,
    G1 = G0#{count := C + 1, memory := GM + Mem,
             reductions := GR + proplists:get_value(reductions, Info, 0),
             queue := GQ + proplists:get_value(message_queue_len, Info, 0),
             registered := GReg + Reg,
             apps := maps:update_with(App, fun(X) -> X + 1 end, 1, Apps),
             samples := case length(Sm) < 3 of true -> Sm ++ [Pid]; false -> Sm end},
    Acc#{groups := Groups#{Key => G1}, scanned := S + 1, registered := R + Reg, memory := M + Mem, other := O + Cap}.

%% {M, F, A} of where the process started; for gen_server and friends the callback module's
%% init (OTP 25+ reads the one dictionary entry, never the whole dictionary).
group_key(Pid, Info) ->
    Dict = try erlang:process_info(Pid, {dictionary, '$initial_call'}) of
               {{dictionary, '$initial_call'}, {M0, F0, A0}} when is_atom(M0), is_atom(F0), is_integer(A0) -> {M0, F0, A0};
               _ -> undefined
           catch _:_ -> undefined
           end,
    case Dict of
        undefined ->
            case proplists:get_value(initial_call, Info) of
                {M, F, A} -> {M, F, A};
                _ -> unknown
            end;
        _ -> Dict
    end.

group_rank(count, #{count := V}) -> V;
group_rank(memory, #{memory := V}) -> V;
group_rank(reductions, #{reductions := V}) -> V;
group_rank(queue, #{queue := V}) -> V.

group_json(#{key := Key, count := C, memory := M, reductions := R, queue := Q, registered := Reg,
             apps := Apps, samples := Sm}, Rank, Scanned, TotalMem, Enc) ->
    {Text, Mod} = case Key of
                      {Mo, F, A} -> {iolist_to_binary([bin(Mo), ":", bin(F), "/", integer_to_binary(A)]), bin(Mo)};
                      other -> {<<"<other>">>, null};
                      unknown -> {<<"<unknown>">>, null}
                  end,
    AppList = lists:sublist(lists:reverse(lists:keysort(2, [{A, N} || {A, N} <- maps:to_list(Apps), A =/= undefined])), 5),
    Unattributed = maps:get(undefined, Apps, 0),
    #{<<"rank">> => Rank,
      <<"initialCall">> => mcp_encoder:safe_binary(Text, maps:get(max_binary_bytes, Enc)),
      <<"module">> => Mod,
      <<"count">> => C,
      <<"countPercent">> => percent(C, Scanned),
      <<"registered">> => Reg,
      <<"unregistered">> => C - Reg,
      <<"memoryBytes">> => M,
      <<"memoryPercent">> => percent(M, TotalMem),
      <<"reductions">> => R,
      <<"messageQueueLen">> => Q,
      <<"applications">> => [#{<<"name">> => bin(A), <<"count">> => N,
                               <<"confidence">> => <<"inferred">>} || {A, N} <- AppList],
      <<"unattributed">> => Unattributed,
      <<"samples">> => [pid_id(P) || P <- Sm]}.

percent(_, 0) -> 0.0;
percent(N, Total) -> round(N * 1000 / Total) / 10.

%%------------------------------------------------------------------------------
%% top_ports: local ports ranked by queue size / bytes in / bytes out. Only the
%% driver name (never an address, command line or path), counters and the owner.
%%------------------------------------------------------------------------------

top_ports_build(Args, Ctx) ->
    SortBy = case maps:get(<<"sortBy">>, Args, <<"queue_size">>) of
                 <<"input">> -> input;
                 <<"output">> -> output;
                 _ -> queue_size
             end,
    Limit = min(maps:get(<<"limit">>, Args, 10), ?MAX_TOP),
    All = erlang:ports(),
    Total = length(All),
    {Scan, Cut} = case Total > ?MAX_SCANNED of
                      true -> {lists:sublist(All, ?MAX_SCANNED), Total - ?MAX_SCANNED};
                      false -> {All, 0}
                  end,
    {Keyed, Unscanned} = rank_keys(Scan, SortBy, self(), Ctx, 0, []),
    Ranked = lists:sublist(lists:reverse(lists:sort(Keyed)), Limit),
    {B1, Gone, _} = lists:foldl(
                      fun({_, Port}, {Acc, G, Rank}) ->
                              case port_entry(Port, SortBy, Rank, Acc) of
                                  gone -> {Acc, G + 1, Rank};
                                  skip -> {Acc, G, Rank};
                                  {ok, Acc1} -> {Acc1, G, Rank + 1}
                              end
                      end, {new_b(), 0, 1}, Ranked),
    B2 = scan_omissions(B1, Gone, Unscanned, Cut, <<"ports">>),
    Scope = #{<<"tool">> => <<"top_ports">>,
              <<"sortBy">> => atom_to_binary(SortBy, utf8),
              <<"limit">> => Limit,
              <<"portCount">> => Total,
              <<"limitations">> =>
                  [<<"A point-in-time scan, not an atomic snapshot: ports open and close while it runs.">>,
                   <<"Only the driver name, byte counters, queue size and the owner process are reported; addresses, command lines, paths and data are never read.">>,
                   <<"driver is <other> for any port that is not a well-known VM driver (for example a spawned program: its name is its command line).">>,
                   <<"owns_port links a port to its connected process; it is not evidence of traffic.">>]},
    {ok, Scope, B2}.

port_entry(Port, SortBy, Rank, B) ->
    Items = [name, connected, input, output, queue_size, registered_name],
    %% port_info/2 takes one item at a time; a closed port answers undefined
    Info = [I || Item <- Items, I <- [erlang:port_info(Port, Item)], I =/= undefined],
    case erlang:port_info(Port, name) of
        undefined -> gone;
        _ ->
            PId = mcp_store:entity_id(process, {port, Port}),
            Fields = #{<<"kind">> => <<"process">>, <<"role">> => <<"port">>, <<"pid">> => null,
                       <<"name">> => case lists:keyfind(registered_name, 1, Info) of
                                         {registered_name, N} when is_atom(N) -> bin(N);
                                         _ -> null
                                     end,
                       <<"alive">> => true,
                       <<"rank">> => Rank,
                       <<"rankedBy">> => atom_to_binary(SortBy, utf8),
                       <<"driver">> => driver_name(proplists:get_value(name, Info)),
                       <<"input">> => proplists:get_value(input, Info, 0),
                       <<"output">> => proplists:get_value(output, Info, 0),
                       <<"queueSize">> => proplists:get_value(queue_size, Info, 0),
                       <<"bytesUnit">> => <<"bytes">>},
            case proplists:get_value(connected, Info) of
                Owner0 when is_pid(Owner0) -> InspectorOwned = is_inspector_pid(Owner0);
                _ -> InspectorOwned = false
            end,
            B1 = case InspectorOwned of
                     true -> B;
                     false -> add_entity(B, PId, Fields)
                 end,
            case proplists:get_value(connected, Info) of
                _ when InspectorOwned -> skip;
                Owner when is_pid(Owner), node(Owner) =:= node() ->
                    {OwnerId, B2} = process_entity(Owner, #{<<"role">> => <<"unknown">>}, B1),
                    {ok, add_rel(B2, <<"owns_port">>, OwnerId, PId, <<"port_info connected">>, <<"confirmed">>)};
                _ -> {ok, B1}
            end
    end.

%% Only well-known VM drivers are named: the "name" of a port opened with spawn/2 is its
%% command line, which can be any text (and look like a driver name).
driver_name(Name) when is_list(Name) ->
    case lists:member(Name, ["tcp_inet", "udp_inet", "sctp_inet", "efile", "tty_sl", "forker", "fd",
                             "ram_file_drv", "zlib_drv", "inet_gethost", "spawn", "ssl_tls"]) of
        true -> list_to_binary(Name);
        false -> <<"<other>">>
    end;
driver_name(_) -> <<"<other>">>.

%% omissions shared by the scanning tools
scan_omissions(B, Gone, Unscanned, Cut, What) ->
    B1 = case Gone of
             0 -> B;
             _ -> add_omission(B, <<"disappeared">>, Gone, undefined,
                               <<"ranked ", What/binary, " that ended before they could be described">>, false)
         end,
    B2 = case Unscanned of
             0 -> B1;
             _ -> add_omission(B1, <<"timeout">>, Unscanned, undefined,
                               <<"the time budget ended the scan; the ranking only covers the ", What/binary,
                                 " scanned so far">>, false)
         end,
    case Cut of
        0 -> B2;
        _ -> add_omission(B2, <<"limit_reached">>, Cut, undefined,
                          <<"more ", What/binary, " than the scan limit; the ranking only covers the first ones">>, false)
    end.

%%------------------------------------------------------------------------------
%% ets_summary: ETS memory per owner process, without any table name, key or value
%%------------------------------------------------------------------------------

ets_summary_build(Args, #{config := Config} = Ctx) ->
    Limit = min(maps:get(<<"limit">>, Args, 10), ?MAX_TOP),
    All = ets:all(),
    Total = length(All),
    {Scan, Cut} = case Total > ?MAX_ETS_SCAN of
                      true -> {lists:sublist(All, ?MAX_ETS_SCAN), Total - ?MAX_ETS_SCAN};
                      false -> {All, 0}
                  end,
    {Owners, Unscanned, Excluded} = ets_owners(Scan, Config, Ctx, 0, #{}, 0),
    WordSize = erlang:system_info(wordsize),
    TotalWords = lists:sum([W || {_, W} <- maps:values(Owners)]),
    Ranked = lists:sublist(lists:reverse(lists:sort([{W, C, O} || {O, {C, W}} <- maps:to_list(Owners)])), Limit),
    Masters = masters(Ctx),
    {B1, _} = lists:foldl(
                fun({Words, Count, Owner}, {Acc, Rank}) ->
                        {Conf, App, Evidence} = membership(Owner, #{}, Masters),
                        Fields = #{<<"role">> => <<"unknown">>,
                                   <<"rank">> => Rank,
                                   <<"application">> => case App of undefined -> null; _ -> bin(App) end,
                                   <<"membership">> => #{<<"confidence">> => Conf, <<"evidence">> => Evidence},
                                   <<"ets">> => #{<<"tables">> => Count, <<"memory">> => Words,
                                                  <<"memoryUnit">> => <<"words">>,
                                                  <<"memoryBytes">> => Words * WordSize}},
                        {_, Acc1} = process_entity(Owner, Fields, Acc),
                        {Acc1, Rank + 1}
                end, {new_b(), 1}, Ranked),
    B2a = scan_omissions(B1, 0, Unscanned, Cut, <<"tables">>),
    B2 = case Excluded of
             0 -> B2a;
             _ -> add_omission(B2a, <<"policy_denied">>, Excluded, undefined,
                               <<"private tables and tables not approved by allowed_ets_tables are not counted">>, false)
         end,
    Scope = #{<<"tool">> => <<"ets_summary">>,
              <<"limit">> => Limit,
              <<"tableCount">> => Total - Excluded,
              <<"ownerCount">> => maps:size(Owners),
              <<"totalMemory">> => #{<<"words">> => TotalWords, <<"bytes">> => TotalWords * WordSize},
              <<"limitations">> =>
                  [<<"Per-owner aggregates only: no table name, key, object or value is read or returned, and no table inventory is listed.">>,
                   <<"Only tables approved by allowed_ets_tables (exact names, or all) are counted; private tables never are.">>,
                   <<"A point-in-time scan, not an atomic snapshot; tables are created and deleted while it runs.">>,
                   <<"For the metadata of a specific table use ets_tables (approved names only).">>]},
    {ok, Scope, B2}.

%% -> {#{OwnerPid => {Tables, Words}}, NotScanned, NotCounted}
ets_owners([], _Config, _Ctx, _N, Acc, Ex) -> {Acc, 0, Ex};
ets_owners([Tab | T] = Tabs, Config, Ctx, N, Acc, Ex) ->
    case N rem 500 =:= 0 andalso N > 0 andalso remaining(Ctx) =:= 0 of
        true -> {Acc, length(Tabs), Ex};
        false ->
            {Acc1, Ex1} = case ets_counted(Tab, Config) of
                              false -> {Acc, Ex + 1};
                              true ->
                                  try {ets:info(Tab, owner), ets:info(Tab, memory)} of
                                      {Owner, Words} when is_pid(Owner), node(Owner) =:= node(), is_integer(Words) ->
                                          case is_inspector_pid(Owner) of
                                              true -> {Acc, Ex};
                                              false -> {maps:update_with(Owner, fun({C, W}) -> {C + 1, W + Words} end,
                                                                         {1, Words}, Acc), Ex}
                                          end;
                                      _ -> {Acc, Ex}
                                  catch _:_ -> {Acc, Ex}
                                  end
                          end,
            ets_owners(T, Config, Ctx, N + 1, Acc1, Ex1)
    end.

%% never a private table; approved by exact name, or every table when allowed_ets_tables is `all`
ets_counted(Tab, Config) ->
    try ets:info(Tab, protection) of
        private -> false;
        undefined -> false;
        _ ->
            case maps:get(allowed_ets_tables, Config) of
                all -> true;
                Names ->
                    ets:info(Tab, named_table) =:= true andalso
                        lists:member(atom_to_binary(ets:info(Tab, name), utf8), Names)
            end
    catch _:_ -> false
    end.

%%------------------------------------------------------------------------------
%% ets_tables (metadata of approved named tables only)
%%------------------------------------------------------------------------------

ets_tables_build(_Args, #{config := Config}) ->
    Names = case maps:get(allowed_ets_tables, Config) of
                all -> [atom_to_binary(N, utf8) || T <- ets:all(), is_atom(T), ets:info(T, named_table) =:= true,
                                                    N <- [ets:info(T, name)], is_atom(N)];
                L -> L
            end,
    {B, Denied, Gone} = lists:foldl(
                          fun(Name, {Acc, D, G}) -> ets_entry(Name, Acc, D, G) end,
                          {new_b(), 0, 0}, Names),
    B1 = case Denied of
             0 -> B;
             _ -> add_omission(B, <<"policy_denied">>, Denied, undefined,
                               <<"approved tables that are private are never inspected">>, false)
         end,
    B2 = case Gone of
             0 -> B1;
             _ -> add_omission(B1, <<"disappeared">>, Gone, undefined,
                               <<"approved tables that do not exist (or no longer exist) on the node">>, false)
         end,
    Scope = #{<<"tool">> => <<"ets_tables">>,
              <<"approvedTables">> => length(Names),
              <<"limitations">> =>
                  [<<"Only tables approved by allowed_ets_tables are inspected; no node-wide inventory exists.">>,
                   <<"Keys, objects and values are never read.">>,
                   <<"memory is reported in words with memoryBytes derived from the VM word size.">>]},
    {ok, Scope, B2}.

ets_entry(NameBin, B, Denied, Gone) ->
    Atom = try binary_to_existing_atom(NameBin, utf8) catch _:_ -> undefined end,
    case Atom of
        undefined -> {B, Denied, Gone + 1};
        _ ->
            case catch ets:info(Atom, protection) of
                undefined -> {B, Denied, Gone + 1};
                private -> {B, Denied + 1, Gone};
                Prot when Prot =:= public; Prot =:= protected ->
                    case ets_facts(Atom, Prot) of
                        undefined -> {B, Denied, Gone + 1};
                        {Owner, Facts} ->
                            TabIdent = ets_identity(Atom, Owner),
                            TId = mcp_store:entity_id(ets_table, {ets, Atom, TabIdent}),
                            B1 = add_entity(B, TId, Facts#{<<"kind">> => <<"ets_table">>}),
                            {OwnerId, B2} = process_entity(Owner, #{<<"role">> => <<"unknown">>}, B1),
                            B3 = add_rel(B2, <<"owns_table">>, OwnerId, TId, <<"ets:info owner">>, <<"confirmed">>),
                            {B3, Denied, Gone}
                    end;
                _ -> {B, Denied, Gone + 1}
            end
    end.

ets_facts(Tab, Prot) ->
    try
        Owner = ets:info(Tab, owner),
        Words = ets:info(Tab, memory),
        true = is_pid(Owner) andalso is_integer(Words),
        {Owner, #{<<"name">> => bin(Tab),
                  <<"protection">> => bin(Prot),
                  <<"type">> => bin(ets:info(Tab, type)),
                  <<"size">> => ets:info(Tab, size),
                  <<"memory">> => Words,
                  <<"memoryUnit">> => <<"words">>,
                  <<"memoryBytes">> => Words * erlang:system_info(wordsize),
                  <<"owner">> => pid_id(Owner)}}
    catch _:_ -> undefined
    end.

%% A recreated table has a different identity even if its name is reused.
ets_identity(Tab, Owner) ->
    try ets:info(Tab, id) of
        Id when Id =/= undefined -> Id;
        _ -> {Owner, ets:info(Tab, heir)}
    catch _:_ -> {Owner, undefined}
    end.
