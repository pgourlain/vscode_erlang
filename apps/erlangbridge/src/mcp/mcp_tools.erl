%% The eight read-only tools: descriptions, strict input schemas, output
%% schemas and the argument validator. Unknown arguments are rejected.
-module(mcp_tools).

-export([list/1, known/1, validate_args/2, structured_error_schema/0]).

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
%% Definitions
%%------------------------------------------------------------------------------

def(<<"runtime_summary">>) ->
    {<<"Runtime summary">>,
     <<"Bounded metadata of the debugged Erlang node: OTP/ERTS versions, node name (host redacted by default), "
       "uptime, scheduler and process counts, memory summary and the opaque debug session id. "
       "Start here, then call application_overview.">>,
     obj(#{<<"redactNodeHost">> => bool(<<"Replace the host part of the node name by <redacted>. Default true.">>)}, []),
     obj(#{<<"schemaVersion">> => str(), <<"sessionId">> => str(), <<"observedAt">> => str(),
           <<"otpRelease">> => str(), <<"ertsVersion">> => str(), <<"node">> => str(),
           <<"uptimeMs">> => int(), <<"schedulers">> => int(), <<"processCount">> => int(),
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
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop()}, []),
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
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop()}, []),
     graph_output()};

def(<<"registered_processes">>) ->
    {<<"Registered processes">>,
     <<"Bounded list of locally registered process names with safe summary metadata. Membership of an application is "
       "reported as confirmed (found in its supervision tree), inferred (group leader) or unknown; unrelated names are "
       "never silently assigned to the selected application. Does not enumerate unregistered processes.">>,
     obj(#{<<"application">> => id_prop(<<"Only processes attributed to this application.">>),
           <<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop()}, []),
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
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop()}, []),
     graph_output()};

def(<<"ets_tables">>) ->
    {<<"ETS tables">>,
     <<"Metadata of the named ETS tables explicitly approved by the project policy (allowed_ets_tables): name, "
       "protection, type, owner entity id, size and memory with units. Private tables are excluded. Never reads keys, "
       "objects or values.">>,
     obj(#{<<"cursor">> => cursor_prop(),
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop()}, []),
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
           <<"detail">> => detail_prop(), <<"pageSize">> => page_size_prop()}, []),
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

exclusive(<<"process_info">>, Args) ->
    case length([K || K <- [<<"id">>, <<"name">>, <<"pid">>], maps:is_key(K, Args)]) of
        1 -> ok;
        _ -> {error, <<"exactly one of id, name or pid is required">>}
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
