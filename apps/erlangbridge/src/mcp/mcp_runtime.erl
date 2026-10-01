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
%% registered/0, supervisor:which_children/1 and supervisor:count_children/1
%% materialize lists in the target VM and cannot be interrupted once started;
%% killing the worker only discards the late reply.
-module(mcp_runtime).

-export([call/3, bounded/2, args_hash/1]).

-define(SV, <<"1.0">>).
-define(MAX_LISTED_CHILDREN, 20000).
-define(MAX_SPEC_LOOKUPS, 200).
%% default page size for agents (detail=summary); detail=full pages use max_items
-define(SUMMARY_PAGE, 100).

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
    Ctx = Ctx0#{enc => maps:with([max_depth, max_items, max_binary_bytes], maps:get(limits, Config)),
                soft_deadline => now_mono() + (Timeout * 7) div 10,
                detail => Detail, page_size => PageSize},
    case maps:find(<<"cursor">>, Args) of
        {ok, Cursor} -> page_from_cursor(Tool, Args, Cursor, Ctx);
        error -> run(Tool, Args, Ctx)
    end.

run(<<"runtime_summary">>, Args, Ctx) -> runtime_summary(Args, Ctx);
run(<<"debug_session">>, _Args, Ctx) -> debug_session(Ctx);
run(<<"application_overview">>, Args, Ctx) -> application_overview(Args, Ctx);
run(<<"supervision_tree">>, Args, Ctx) -> supervision_tree(Args, Ctx);
run(<<"registered_processes">>, Args, Ctx) -> registered_processes(Args, Ctx);
run(<<"process_info">>, Args, Ctx) -> tool_process_info(Args, Ctx);
run(<<"ets_tables">>, Args, Ctx) -> ets_tables(Args, Ctx);
run(_, _, _) -> {error, <<"unknown_tool">>, <<"unknown tool">>}.

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
           <<"memory">> => #{<<"unit">> => <<"bytes">>,
                             <<"total">> => proplists:get_value(total, Mem),
                             <<"processes">> => proplists:get_value(processes, Mem),
                             <<"system">> => proplists:get_value(system, Mem),
                             <<"atom">> => proplists:get_value(atom, Mem),
                             <<"binary">> => proplists:get_value(binary, Mem),
                             <<"ets">> => proplists:get_value(ets, Mem)}}}.

node_text(false) -> atom_to_binary(node(), utf8);
node_text(true) ->
    case binary:split(atom_to_binary(node(), utf8), <<"@">>) of
        [Name, _Host] -> <<Name/binary, "@<redacted>">>;
        [Name] -> Name
    end.

%% Safe debugger metadata only: mode, connection state, interpreted modules and
%% breakpoint locations (module + line). Source and options are not returned.
debug_session(#{enc := Enc} = _Ctx) ->
    Max = maps:get(max_items, Enc),
    {Interpreted, IUnavailable} = debug_call(fun() -> int:interpreted() end, []),
    {Breaks, BUnavailable} = debug_call(fun() -> int:all_breaks() end, []),
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
           <<"debuggerMetadata">> => case IUnavailable orelse BUnavailable of
                                         true -> <<"unavailable">>;
                                         false -> <<"available">>
                                     end,
           <<"truncated">> => length(Interpreted) > Max orelse length(Breaks) > Max}}.

debug_call(Fun, Default) ->
    case bounded(Fun, 1500) of
        {ok, R} when is_list(R) -> {R, false};
        _ -> {Default, true}
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
    Items = [{entity, E} || E <- lists:reverse(maps:get(ents, B))]
        ++ [{relationship, R} || R <- lists:reverse(maps:get(rels, B))],
    Hash = args_hash(maps:remove(<<"cursor">>, Args)),
    {CollId, Meta1, Stored} = mcp_store:put_collection(Tool, Hash, Meta, Items, #{}),
    render_page(Tool, Hash, CollId, Meta1, Stored, 0, Ctx).

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
    #{<<"schemaVersion">> => ?SV,
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
      <<"page">> => #{<<"offset">> => Offset, <<"items">> => length(Page)}}.

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

application_overview(Args, #{enc := Enc} = Ctx) ->
    Started = mcp_store:now_ms(),
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
            finish_graph(<<"application_overview">>, Args, Scope, B3, Started, Ctx)
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

supervision_tree(Args, #{config := Config} = Ctx) ->
    Started = mcp_store:now_ms(),
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
            finish_graph(<<"supervision_tree">>, Args, Scope, B1, Started, Ctx)
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
                {ok, Children} ->
                    {B2, Next} = lists:foldl(
                                   fun(C, {Acc, Q}) -> child(C, Sup, SupId, Depth, Cfg, Acc, Q) end,
                                   {B1, []}, Children),
                    walk(Rest ++ lists:reverse(Next), Cfg, Visited#{Sup => true}, B2);
                {too_many, N} ->
                    B2 = add_omission(B1, <<"limit_reached">>, N, SupId,
                                      <<"too many children to list safely; children not listed">>, false),
                    walk(Rest, Cfg, Visited#{Sup => true}, B2);
                timeout ->
                    B2 = add_omission(B1, <<"timeout">>, 1, SupId,
                                      <<"the supervisor did not answer in time; its children are missing from this map">>, false),
                    walk(Rest, Cfg, Visited#{Sup => true}, B2);
                gone ->
                    B2 = add_omission(B1, <<"disappeared">>, 1, SupId,
                                      <<"the supervisor exited during the collection">>, false),
                    walk(Rest, Cfg, Visited#{Sup => true}, B2)
            end
    end.

app_text(#{app := undefined}) -> null;
app_text(#{app := App}) -> bin(App).

%% -> {ok, [{ChildId, Child, Type, Modules, SpecMeta}]} | {too_many, N} | timeout | gone
children_of(Sup, Ctx) ->
    Fun = fun() ->
                  Counts = supervisor:count_children(Sup),
                  Total = proplists:get_value(specs, Counts, 0),
                  case Total > ?MAX_LISTED_CHILDREN of
                      true -> {too_many, Total};
                      false ->
                          Cs = supervisor:which_children(Sup),
                          {ok, with_specs(Sup, Cs, 0, [])}
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

registered_processes(Args, #{enc := Enc, config := Config} = Ctx) ->
    Started = mcp_store:now_ms(),
    case select_application(maps:get(<<"application">>, Args, undefined)) of
        {error, _, _} = E -> E;
        {ok, Selected} ->
            Cap = mcp_policy:limit(max_items, Config) * 4,
            All = [N || N <- registered(), not inspector_name(N)],
            Sorted = lists:sort(All),
            Shown = lists:sublist(Sorted, Cap),
            Masters = masters(Ctx),
            {Confirmed, B0} = case Selected of
                                  {app, App} -> tree_pids(App, Ctx);
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
            finish_graph(<<"registered_processes">>, Args, Scope, B3, Started, Ctx)
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
    {Id, B1} = process_entity(Pid, Fields, new_b()),
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
                   <<"Process dictionary, stack, mailbox and state are never read.">>]},
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
    case re:run(Text, "^<0\\.[0-9]{1,10}\\.[0-9]{1,10}>$", [{capture, none}]) of
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
%% ets_tables (metadata of approved named tables only)
%%------------------------------------------------------------------------------

ets_tables(Args, #{config := Config} = Ctx) ->
    Started = mcp_store:now_ms(),
    Names = maps:get(allowed_ets_tables, Config),
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
    finish_graph(<<"ets_tables">>, Args, Scope, B2, Started, Ctx).

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
