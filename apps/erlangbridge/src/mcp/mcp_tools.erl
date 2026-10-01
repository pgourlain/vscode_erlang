%% The eight read-only tools: descriptions, strict input schemas, output
%% schemas and the argument validator. Unknown arguments are rejected.
-module(mcp_tools).

-export([list/1, known/1, validate_args/2, structured_error_schema/0]).
-export([prompts/0, prompt/3]).

-define(SCHEMA_VERSION, <<"1.0">>).
-export([schema_version/0]).
schema_version() -> ?SCHEMA_VERSION.

%% tools/list result entries for the allowed tools.
list(Config) ->
    [tool_def(Name) || Name <- mcp_policy:all_tools(), mcp_policy:tool_allowed(Name, Config)].

known(Name) -> lists:member(Name, mcp_policy:all_tools()).

tool_def(Name) ->
    {Title, Desc, In, Out} = def(Name),
    #{<<"name">> => Name,
      <<"title">> => Title,
      <<"description">> => Desc,
      <<"inputSchema">> => In,
      <<"outputSchema">> => Out,
      <<"annotations">> => #{<<"readOnlyHint">> => true,
                             <<"destructiveHint">> => false,
                             <<"idempotentHint">> => true,
                             <<"openWorldHint">> => false}}.

%%------------------------------------------------------------------------------
%% Prompts: one static prompt that tells an agent how to use the tools. It carries
%% no data and grants nothing; the argument is checked before it is embedded.
%%------------------------------------------------------------------------------

prompts() ->
    [#{<<"name">> => <<"map_application">>,
       <<"title">> => <<"Map an OTP application">>,
       <<"description">> => <<"Map the supervision topology of one application of the debugged node and report "
                              "coverage gaps and observations, using only what the inspector tools return.">>,
       <<"arguments">> => [#{<<"name">> => <<"application">>,
                             <<"description">> => <<"Name of a started OTP application.">>,
                             <<"required">> => true}]}].

%% -> {ok, GetPromptResult} | {error, Message}
prompt(<<"map_application">>, Args, Config) when is_map(Args) ->
    case maps:get(<<"application">>, Args, undefined) of
        App when is_binary(App), byte_size(App) > 0, byte_size(App) =< 255 ->
            case re:run(App, "^[A-Za-z0-9_@.-]+\\z", [{capture, none}]) of
                match ->
                    {ok, #{<<"description">> => <<"Map the OTP topology of ", App/binary>>,
                           <<"messages">> => [#{<<"role">> => <<"user">>,
                                                <<"content">> => #{<<"type">> => <<"text">>,
                                                                   <<"text">> => map_application_text(App, Config)}}]}};
                nomatch -> {error, <<"invalid argument 'application': not an application name">>}
            end;
        undefined -> {error, <<"missing required argument 'application'">>};
        _ -> {error, <<"invalid argument 'application'">>}
    end;
prompt(<<"map_application">>, _, _) ->
    {error, <<"arguments must be an object">>};
prompt(_, _, _) ->
    {error, <<"unknown prompt">>}.

map_application_text(App, Config) ->
    Allowed = fun(Tool) -> mcp_policy:tool_allowed(Tool, Config) end,
    Steps = case Allowed(<<"topology_overview">>) of
                true ->
                    ["1. topology_overview(name=\"", App, "\"): the application, its supervision tree, registered "
                     "processes and owned approved ETS tables in one call. If an omission says limit_reached or "
                     "timeout, expand that subtree with supervision_tree and the id it gives.\n"];
                false ->
                    ["1. runtime_summary, then application_overview to find the id and root supervisors of \"", App,
                     "\"; supervision_tree from each root (expand subtrees named in limit_reached or timeout "
                     "omissions); registered_processes(application=<id>) for registered workers outside the tree.\n"]
            end,
    iolist_to_binary(
      ["Using the erlang-otp-topology-inspector MCP server, map the OTP topology of application ", App,
       " in the node I am debugging. Use only the tools that tools/list returns.\n\nSteps:\n", Steps,
       "2. top_processes (or process_info) only for processes that matter: supervisors, registered workers, "
       "anything with a non-zero message queue. If debug_session reports processes stopped at a breakpoint, "
       "say so: their supervisors may be reported as unavailable_while_paused.\n"
       "3. ets_tables and debug_session only if they relate to the application.\n\n"
       "Output:\n"
       "- A Mermaid `graph TD` of the supervision tree (edge = supervises; label children with kind and restart "
       "type). Tools take format=mermaid and return the diagram from the reported edges.\n"
       "- A table: name/pid, module, kind, restart, msg queue len, membership (confirmed/inferred).\n"
       "- \"Coverage gaps\": every truncated/omission/timeout reported by the tools.\n"
       "- 3-5 observations (single points of failure, deep trees, hot queues, temporary children).\n\n"
       "Rules: use only what the tools returned. Structural edges are not message traffic, so do not describe "
       "runtime call flows. Keep detail=summary unless I ask for the full JSON."]).

%%------------------------------------------------------------------------------
%% Definitions
%%------------------------------------------------------------------------------

def(<<"runtime_summary">>) ->
    {<<"Runtime summary">>,
     <<"Bounded metadata of the debugged Erlang node: OTP/ERTS versions, node name (host redacted by default), "
       "uptime, scheduler count, run queue length, process/atom/port/ETS counts against their VM limits, memory "
       "summary and the opaque debug session id. "
       "Start here, then call application_overview.">>,
     obj(#{<<"redactNodeHost">> => bool(<<"Replace the host part of the node name by <redacted>. Default true.">>)}, []),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"otpRelease">> => str(), <<"ertsVersion">> => str(), <<"node">> => str(),
           <<"uptimeMs">> => int(), <<"schedulers">> => int(), <<"processCount">> => int(),
           <<"schedulersOnline">> => int(), <<"runQueue">> => int(),
           <<"connectedNodes">> => #{<<"type">> => <<"object">>},
           <<"resources">> => #{<<"type">> => <<"object">>},
           <<"memory">> => #{<<"type">> => <<"object">>}},
         [<<"schemaVersion">>, <<"sessionId">>])};

def(<<"application_overview">>) ->
    {<<"Application overview">>,
     <<"Discover the started OTP applications of the node without knowing any process name: names, versions, "
       "declared dependencies (declarations, not evidence of runtime communication) and discoverable root "
       "supervisors with reusable entity ids. Returns a paginated map (entities, typed relationships, coverage "
       "limitations). Never calls application callbacks, loads code or reads application environment.">>,
     obj(#{<<"application">> => id_prop(<<"Only this application (entity id from a previous call).">>),
           <<"includeModules">> => bool(<<"Also list module entities (with declared behaviours when the module is already loaded). Default false.">>),
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"supervision_tree">>) ->
    {<<"Supervision tree">>,
     <<"Traverse supervisors from an application id or process (supervisor) id discovered by application_overview, "
       "or from an explicitly given local supervisor (registered name or pid). Uses supervisor APIs with deadlines: "
       "an unresponsive supervisor yields a partial result with a 'timeout' omission. Emits typed 'supervises' edges, "
       "child kind, module and restart/shutdown metadata; never child start arguments. Never resumes or alters processes.">>,
     obj(#{<<"id">> => id_prop(<<"Application or process entity id.">>),
           <<"supervisor">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 255,
                                 <<"description">> => <<"Registered name or local pid text of a supervisor.">>},
           <<"maxDepth">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 16},
           <<"includeModules">> => bool(<<"Add module entities and uses_module edges. Default false.">>),
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"registered_processes">>) ->
    {<<"Registered processes">>,
     <<"Bounded list of locally registered process names with safe summary metadata. Membership of an application is "
       "reported as confirmed (found in its supervision tree), inferred (group leader) or unknown; unrelated names are "
       "never silently assigned to the selected application. Does not enumerate unregistered processes.">>,
     obj(#{<<"application">> => id_prop(<<"Only processes attributed to this application.">>),
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"process_info">>) ->
    {<<"Process info">>,
     <<"Allowlisted fields of one local process (by entity id, registered name or pid): registered name, status, "
       "current function, initial call, reductions, memory (words), message queue length, links and monitors as "
       "bounded identifiers, application membership with evidence. No dictionary, stack, mailbox or state.">>,
     obj(#{<<"id">> => id_prop(<<"Process entity id.">>),
           <<"name">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 255,
                           <<"description">> => <<"Registered name (must be an existing atom).">>},
           <<"pid">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 64,
                          <<"description">> => <<"Local pid text such as <0.123.0>.">>},
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"ets_tables">>) ->
    {<<"ETS tables">>,
     <<"Metadata of the named ETS tables explicitly approved by the project policy (allowed_ets_tables): name, "
       "protection, type, owner entity id, size and memory with units. Private tables are excluded. Never reads keys, "
       "objects or values.">>,
     obj(#{<<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"topology_overview">>) ->
    {<<"Topology overview">>,
     <<"One-call map of a single started application: the application with its declared dependencies and root, "
       "its supervision tree (with child counts), its registered processes and the approved ETS tables owned by those "
       "processes, as one collection of entities and typed relationships. Give the application by entity id or by "
       "name. Parts whose own tool is not allowed by the project policy are left out and reported as a policy_denied "
       "omission. For several applications, deeper subtrees or exports use the individual tools.">>,
     obj(#{<<"application">> => id_prop(<<"Application entity id from a previous call.">>),
           <<"name">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 255,
                           <<"description">> => <<"Name of a started application (must be an existing atom).">>},
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"changes_since">>) ->
    {<<"Changes since a collection">>,
     <<"Observe again the tool and arguments of an earlier collection (collectionId of a previous graph result) and "
       "return only what changed: added, removed and replaced entities (replaced = the same logical child with a new "
       "id, i.e. restarted or recreated; previousId names the old one), plus the edges leading to new entities. "
       "A removal from a partial collection is marked absence=unconfirmed. The baseline is only retained for a short "
       "time (collection_ttl_ms, few collections): compare soon after the first call, otherwise baseline_expired.">>,
     obj(#{<<"collectionId">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 64,
                                   <<"description">> => <<"collectionId of the baseline (from a previous graph result).">>},
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, [<<"collectionId">>]),
     graph_output()};

def(<<"process_state">>) ->
    {<<"Process state (developer tier)">>,
     <<"The state of one gen_server, gen_statem or gen_event process (sys:get_state with a short timeout), "
       "by entity id, registered name or pid. Bounded (depth, items, binary size); values of keys named like a secret "
       "(password, token, ...) and credentials in URLs are replaced, but a record without keys cannot be redacted: "
       "the result can contain application data. Only enabled when the project lists it in allowed_tools. "
       "Fails for a process stopped at a breakpoint (reported as such).">>,
     obj(#{<<"id">> => id_prop(<<"Process entity id.">>),
           <<"name">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 255,
                           <<"description">> => <<"Registered name (must be an existing atom).">>},
           <<"pid">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 64,
                          <<"description">> => <<"Local pid text such as <0.123.0>.">>}}, []),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"process">> => str(), <<"state">> => #{}}, [<<"schemaVersion">>, <<"sessionId">>])};

def(<<"mailbox_sample">>) ->
    {<<"Mailbox sample (developer tier)">>,
     <<"The queue length and the oldest few messages (default 5, max 20) of one local process, by entity id, registered "
       "name or pid. Messages are bounded and encoded like process_state (best-effort redaction); a mailbox of more than "
       "20000 messages is not copied, only its length is reported. Only enabled when the project lists it in "
       "allowed_tools.">>,
     obj(#{<<"id">> => id_prop(<<"Process entity id.">>),
           <<"name">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 255,
                           <<"description">> => <<"Registered name (must be an existing atom).">>},
           <<"pid">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 64,
                          <<"description">> => <<"Local pid text such as <0.123.0>.">>},
           <<"limit">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 20}}, []),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"process">> => str(), <<"queueLength">> => int(), <<"sample">> => #{<<"type">> => <<"array">>}},
         [<<"schemaVersion">>, <<"sessionId">>])};

def(<<"ets_sample">>) ->
    {<<"ETS sample (developer tier)">>,
     <<"A few objects (default 5, max 20) of one named ETS table approved by the project (allowed_ets_tables: exact "
       "names, or all). Never a private table. Objects are bounded and encoded like process_state (best-effort "
       "redaction). Only enabled when the project lists it in allowed_tools.">>,
     obj(#{<<"table">> => #{<<"type">> => <<"string">>, <<"maxLength">> => 255,
                            <<"description">> => <<"Name of the table (must be an existing atom).">>},
           <<"limit">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 20}}, [<<"table">>]),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"table">> => str(), <<"size">> => int(), <<"sample">> => #{<<"type">> => <<"array">>}},
         [<<"schemaVersion">>, <<"sessionId">>])};

def(<<"process_groups">>) ->
    {<<"Process groups">>,
     <<"Aggregate every local process by where it was started (initial call; for gen_server and friends the callback "
       "module's init) and return the largest groups (default 10, max 50) by process count, memory, reductions or "
       "queue length: totals, shares of the node, registered versus unregistered, inferred application membership and "
       "up to 3 sample process ids per group (usable with process_info). Finds fan-out and leaks - thousands of "
       "anonymous workers - that a top list of single processes does not show. The answer has a fixed size however "
       "many processes exist; the scan is bounded (process cap, time budget, group cap) and says when it was cut. "
       "Metadata only.">>,
     obj(#{<<"sortBy">> => #{<<"type">> => <<"string">>,
                             <<"enum">> => [<<"count">>, <<"memory">>, <<"reductions">>, <<"message_queue_len">>],
                             <<"description">> => <<"Ranking criterion. Default count.">>},
           <<"limit">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 50,
                            <<"description">> => <<"Number of groups to return. Default 10.">>}}, []),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"totals">> => #{<<"type">> => <<"object">>}, <<"groups">> => #{<<"type">> => <<"array">>},
           <<"complete">> => #{<<"type">> => <<"boolean">>}, <<"omissions">> => #{<<"type">> => <<"array">>}},
         [<<"schemaVersion">>, <<"sessionId">>])};

def(<<"top_ports">>) ->
    {<<"Top ports">>,
     <<"Rank the local ports of the node (sockets, files, spawned programs) by queue size, bytes in or bytes out and "
       "return the top ones (default 10, max 50) with their driver name, byte counters, queue size and the owner "
       "process (owns_port edge). Addresses, command lines, paths and data are never read; a port that is not a plain "
       "driver name is reported as <redacted>. Point-in-time scan.">>,
     obj(#{<<"sortBy">> => #{<<"type">> => <<"string">>,
                             <<"enum">> => [<<"queue_size">>, <<"input">>, <<"output">>],
                             <<"description">> => <<"Ranking criterion. Default queue_size.">>},
           <<"limit">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 50,
                            <<"description">> => <<"Number of ports to return. Default 10.">>},
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"ets_summary">>) ->
    {<<"ETS summary">>,
     <<"ETS memory aggregated per owner process, largest first (default 10 owners, max 50): number of tables and "
       "memory in words and bytes, with application membership evidence, plus node totals in the scope. No table "
       "name, key, object or value is read and no table inventory is listed; use ets_tables for the metadata of a "
       "specific approved table.">>,
     obj(#{<<"limit">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 50,
                            <<"description">> => <<"Number of owners to return. Default 10.">>},
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"top_processes">>) ->
    {<<"Top processes">>,
     <<"Rank the local processes of the node by message queue length, reductions or memory and return the top ones "
       "(default 10, max 50) with status, current function, initial call, registered name and application membership "
       "evidence. Use it to find hot, stuck or heavy processes without calling process_info on each one. A bounded "
       "point-in-time scan: processes start and exit during it, reductions are cumulative since process start. "
       "Never reads mailbox, dictionary, stack or state.">>,
     obj(#{<<"sortBy">> => #{<<"type">> => <<"string">>,
                             <<"enum">> => [<<"message_queue_len">>, <<"reductions">>, <<"memory">>],
                             <<"description">> => <<"Ranking criterion. Default message_queue_len.">>},
           <<"limit">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 50,
                            <<"description">> => <<"Number of processes to return. Default 10.">>},
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop(),
           <<"format">> => format_prop()}, []),
     graph_output()};

def(<<"debug_session">>) ->
    {<<"Debug session">>,
     <<"Safe metadata of the current debug session: launch or attach, node connection, interpreted modules and "
       "active breakpoint locations (module and line), and the processes currently stopped at a breakpoint (process id, "
       "module and line). No source, environment or credentials.">>,
     obj(#{}, []),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"mode">> => str(), <<"nodeConnected">> => #{<<"type">> => <<"boolean">>},
           <<"interpretedModules">> => #{<<"type">> => <<"array">>},
           <<"breakpoints">> => #{<<"type">> => <<"array">>},
           <<"pausedProcesses">> => #{<<"type">> => <<"array">>}},
         [<<"schemaVersion">>, <<"sessionId">>])}.

structured_error_schema() ->
    obj(#{<<"error">> => obj(#{<<"code">> => str(), <<"message">> => str()}, [<<"code">>])}, [<<"error">>]).

graph_output() ->
    Arr = #{<<"type">> => <<"array">>},
    obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"collectionId">> => str(),
          <<"startedAt">> => str(), <<"finishedAt">> => str(),
          <<"scope">> => #{<<"type">> => <<"object">>},
          <<"entities">> => Arr, <<"relationships">> => Arr,
          <<"complete">> => #{<<"type">> => <<"boolean">>},
          <<"truncated">> => #{<<"type">> => <<"boolean">>},
          <<"omissions">> => Arr,
          <<"nextCursor">> => #{<<"type">> => [<<"string">>, <<"null">>]}},
        [<<"schemaVersion">>, <<"sessionId">>, <<"collectionId">>, <<"startedAt">>, <<"finishedAt">>,
         <<"scope">>, <<"entities">>, <<"relationships">>, <<"complete">>, <<"truncated">>,
         <<"omissions">>, <<"nextCursor">>]).

obj(Props, Required) ->
    #{<<"type">> => <<"object">>, <<"properties">> => Props,
      <<"required">> => Required, <<"additionalProperties">> => false}.

str() -> #{<<"type">> => <<"string">>}.
int() -> #{<<"type">> => <<"integer">>}.
bool(Desc) -> #{<<"type">> => <<"boolean">>, <<"description">> => Desc}.
id_prop(Desc) -> #{<<"type">> => <<"string">>, <<"maxLength">> => 64, <<"description">> => Desc}.
format_prop() ->
    #{<<"type">> => <<"string">>, <<"enum">> => [<<"json">>, <<"mermaid">>],
      <<"description">> => <<"json (default): entities and relationships only. mermaid: also return a 'mermaid' field "
                            "with a ready-to-render `graph TD` of this page (built from the returned edges, so it "
                            "cannot show a relationship the tools did not report).">>}.

detail_prop() ->
    #{<<"type">> => <<"string">>, <<"enum">> => [<<"summary">>, <<"full">>],
      <<"description">> => <<"summary (default): compact entities/edges, 100 items per page - best for reasoning. "
                             "full: every field (pid, descriptions, edge observedAt...) and the largest pages - "
                             "use it only to export the complete JSON (e.g. to write it to a file).">>}.

page_size_prop() ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 1, <<"maximum">> => 500,
      <<"description">> => <<"Items (entities + relationships) per page; capped by the project policy.">>}.

cursor_prop() ->
    #{<<"type">> => <<"string">>, <<"maxLength">> => 200,
      <<"description">> => <<"nextCursor of a previous call with the same arguments.">>}.

%%------------------------------------------------------------------------------
%% Validation of arguments against the input schema (strict subset of JSON schema)
%%------------------------------------------------------------------------------

%% -> ok | {error, Message}
validate_args(Tool, Args) ->
    case known(Tool) of
        false -> {error, <<"unknown tool">>};
        true ->
            {_, _, Schema, _} = def(Tool),
            case is_map(Args) of
                false -> {error, <<"arguments must be an object">>};
                true ->
                    case validate_object(Schema, Args) of
                        ok -> exclusive(Tool, Args);
                        {error, _} = E -> E
                    end
            end
    end.

exclusive(Tool, Args) when Tool =:= <<"process_info">>; Tool =:= <<"process_state">>; Tool =:= <<"mailbox_sample">> ->
    case length([K || K <- [<<"id">>, <<"name">>, <<"pid">>], maps:is_key(K, Args)]) of
        1 -> ok;
        _ -> {error, <<"exactly one of id, name or pid is required">>}
    end;
exclusive(<<"topology_overview">>, Args) ->
    case maps:is_key(<<"cursor">>, Args) of
        true -> ok;
        false ->
            case length([K || K <- [<<"application">>, <<"name">>], maps:is_key(K, Args)]) of
                1 -> ok;
                _ -> {error, <<"exactly one of application or name is required">>}
            end
    end;
exclusive(<<"supervision_tree">>, Args) ->
    case maps:is_key(<<"cursor">>, Args) of
        true -> ok;
        false ->
            case length([K || K <- [<<"id">>, <<"supervisor">>], maps:is_key(K, Args)]) of
                1 -> ok;
                _ -> {error, <<"exactly one of id or supervisor is required">>}
            end
    end;
exclusive(_, _) ->
    ok.

validate_object(#{<<"properties">> := Props, <<"required">> := Required}, Args) ->
    Unknown = [K || K <- maps:keys(Args), not maps:is_key(K, Props)],
    Missing = [K || K <- Required, not maps:is_key(K, Args)],
    case {Unknown, Missing} of
        {[_ | _], _} -> {error, <<"unexpected argument">>};
        {_, [_ | _]} -> {error, <<"missing required argument">>};
        _ -> validate_props(maps:to_list(Args), Props)
    end.

validate_props([], _) -> ok;
validate_props([{K, V} | T], Props) ->
    case validate_value(maps:get(K, Props), V) of
        ok -> validate_props(T, Props);
        {error, Msg} -> {error, <<"invalid argument '", K/binary, "': ", Msg/binary>>}
    end.

validate_value(#{<<"type">> := <<"string">>, <<"enum">> := Enum}, V) when is_binary(V) ->
    case lists:member(V, Enum) of
        true -> ok;
        false -> {error, <<"not an allowed value">>}
    end;
validate_value(#{<<"type">> := <<"string">>} = S, V) when is_binary(V) ->
    case unicode:characters_to_binary(V, utf8, utf8) of
        B when is_binary(B) ->
            case byte_size(V) > maps:get(<<"maxLength">>, S, 4096) of
                true -> {error, <<"too long">>};
                false -> ok
            end;
        _ -> {error, <<"not valid UTF-8">>}
    end;
validate_value(#{<<"type">> := <<"boolean">>}, V) when is_boolean(V) -> ok;
validate_value(#{<<"type">> := <<"integer">>} = S, V) when is_integer(V) ->
    Min = maps:get(<<"minimum">>, S, V),
    Max = maps:get(<<"maximum">>, S, V),
    case V >= Min andalso V =< Max of
        true -> ok;
        false -> {error, <<"out of range">>}
    end;
validate_value(#{<<"type">> := T}, _) ->
    {error, <<"expected ", T/binary>>}.
