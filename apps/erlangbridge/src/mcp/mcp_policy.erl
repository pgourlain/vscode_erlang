%% Configuration and policy of the embedded MCP inspector.
%%
%% Two sources, with disjoint ownership:
%%   * VS Code settings (erlang.mcp.enabled/host/port): activation and binding;
%%   * the optional top-level `{mcp, [...]}` term of the target project's
%%     rebar.config: tool/table allowlists and *lower* resource limits.
%% Neither can exceed the built-in ceilings.
%%
%% The project file is read by a bounded, non-evaluating reader (no
%% rebar.config.script, no profile merging, no includes). It interns the atoms
%% of the file, so it must only run in a short-lived helper VM (see cli/0) -
%% never in the long-lived LSP or target VM.
%%
%% Normalized configuration (internal contract, atom keys) :
%%   #{host => "127.0.0.1", port => 0,
%%     allowed_tools => [<<"runtime_summary">>, ...],
%%     allowed_ets_tables => [<<"orders_index">>],
%%     limits => #{max_items => 500, ...}}
%% and its JSON form (camelCase binary keys) which crosses to the target.
-module(mcp_policy).

-include_lib("kernel/include/file.hrl").

-export([all_tools/0, default_tools/0, defaults/0, default_limits/0, ceilings/0, limit/2, tool_allowed/2]).
-export([ets_table_allowed/2]).
-export([resolve/1, read_project_policy/2, normalize_project_term/1]).
-export([validate_host/1, validate_config/1, to_json_map/1, from_json_map/1, validate_token/1]).
-export([cli/0]).

-define(MAX_PROJECT_FILE_BYTES, 262144).
-define(MAX_PROJECT_TOKENS, 20000).
-define(PROJECT_FILE, "rebar.config").

%% Enabled unless the project policy narrows them (metadata only).
-define(DEFAULT_TOOLS, [<<"runtime_summary">>, <<"application_overview">>,
                        <<"supervision_tree">>, <<"process_info">>,
                        <<"registered_processes">>, <<"ets_tables">>,
                        <<"debug_session">>, <<"top_processes">>, <<"topology_overview">>, <<"changes_since">>,
                        <<"top_ports">>, <<"ets_summary">>]).
%% Developer tier: they return data of the debugged application (a bounded process
%% state, a few mailbox messages, a few table rows), so a project must name them in
%% allowed_tools; they are never enabled by default.
-define(DEV_TOOLS, [<<"process_state">>, <<"mailbox_sample">>, <<"ets_sample">>]).
-define(TOOLS, (?DEFAULT_TOOLS ++ ?DEV_TOOLS)).

%% {internal key, project key, JSON key, default, ceiling}: a project may raise a
%% limit above its default up to the ceiling, never above.
-define(LIMITS,
        [{max_request_bytes,    max_request_bytes,    <<"maxRequestBytes">>,    16384,   65536},
         {max_result_bytes,     max_result_bytes,     <<"maxResultBytes">>,     262144,  2097152},
         {max_items,            max_items,            <<"maxItems">>,           500,     5000},
         {max_depth,            max_depth,            <<"maxDepth">>,           16,      32},
         {max_binary_bytes,     max_binary_bytes,     <<"maxBinaryBytes">>,     4096,    65536},
         {max_traversal_depth,  max_traversal_depth,  <<"maxTraversalDepth">>,  8,       16},
         {max_concurrency,      max_concurrency,      <<"maxConcurrency">>,     2,       8},
         {request_timeout_ms,   request_timeout_ms,   <<"requestTimeoutMs">>,   3000,    15000},
         {collection_ttl_ms,    collection_ttl_ms,    <<"collectionTtlMs">>,    30000,   600000},
         {max_collection_bytes, max_collection_bytes, <<"maxCollectionBytes">>, 1048576, 8388608},
         {max_collections,      max_collections,      <<"maxCollections">>,     2,       16}]).

%% Keys a project term must never carry (owned by VS Code / the runtime).
-define(FORBIDDEN_PROJECT_KEYS,
        [enabled, required, host, port, transport, auth_token, token,
         credentials, cookie, authorization, bearer]).

all_tools() -> ?TOOLS.
default_tools() -> ?DEFAULT_TOOLS.

default_limits() ->
    maps:from_list([{K, D} || {K, _, _, D, _} <- ?LIMITS]).

ceilings() ->
    maps:from_list([{K, C} || {K, _, _, _, C} <- ?LIMITS]).

defaults() ->
    #{host => "127.0.0.1",
      port => 0,
      allowed_tools => ?DEFAULT_TOOLS,
      allowed_ets_tables => [],
      limits => default_limits()}.

%% Is the named table approved? allowed_ets_tables is a list of exact names, or `all`.
ets_table_allowed(_Name, #{allowed_ets_tables := all}) -> true;
ets_table_allowed(Name, #{allowed_ets_tables := Names}) -> lists:member(Name, Names).

limit(Key, #{limits := Limits}) -> maps:get(Key, Limits).

tool_allowed(Tool, #{allowed_tools := Tools}) -> lists:member(Tool, Tools).

%%------------------------------------------------------------------------------
%% Host validation: loopback literals or names resolving only to loopback.
%%------------------------------------------------------------------------------

%% -> {ok, LiteralAddressString} | {error, Message}
validate_host(Host) when is_binary(Host) ->
    validate_host(binary_to_list(Host));
validate_host(Host) when is_list(Host), length(Host) > 0, length(Host) =< 255 ->
    case inet:parse_strict_address(Host) of
        {ok, Ip} -> check_loopback(Ip, Host);
        {error, _} -> resolve_loopback(Host)
    end;
validate_host(_) ->
    {error, "erlang.mcp.host must be a non-empty loopback address"}.

check_loopback(Ip, Text) ->
    case is_loopback(Ip) of
        true -> {ok, inet:ntoa(Ip)};
        false ->
            {error, lists:flatten(io_lib:format(
                "erlang.mcp.host '~ts' is not a loopback address: only 127.0.0.0/8 and ::1 are accepted", [Text]))}
    end.

resolve_loopback(Name) ->
    Ips = lists:usort([Ip || Fam <- [inet, inet6],
                             {ok, Ip} <- [safe_getaddr(Name, Fam)]]),
    case Ips of
        [] ->
            {error, lists:flatten(io_lib:format("erlang.mcp.host '~ts' cannot be resolved", [Name]))};
        _ ->
            case lists:all(fun is_loopback/1, Ips) of
                true -> {ok, inet:ntoa(hd(prefer_v4(Ips)))};
                false ->
                    {error, lists:flatten(io_lib:format(
                        "erlang.mcp.host '~ts' resolves to a non-loopback address", [Name]))}
            end
    end.

prefer_v4(Ips) ->
    {V4, V6} = lists:partition(fun(Ip) -> tuple_size(Ip) =:= 4 end, Ips),
    V4 ++ V6.

safe_getaddr(Name, Family) ->
    try inet:getaddr(Name, Family) catch _:_ -> {error, einval} end.

is_loopback({127, _, _, _}) -> true;
is_loopback({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_loopback(_) -> false.

%%------------------------------------------------------------------------------
%% Settings + project policy -> normalized configuration
%%------------------------------------------------------------------------------

%% Input (binary-keyed map, decoded JSON) :
%%   <<"enabled">>, <<"host">>, <<"port">>, <<"cwd">>, <<"root">>, <<"trusted">>
%% -> disabled | {ok, Config} | {error, Message}
resolve(Input) when is_map(Input) ->
    case maps:get(<<"enabled">>, Input, false) of
        true -> resolve_enabled(Input);
        _ -> disabled
    end.

resolve_enabled(Input) ->
    case maps:get(<<"trusted">>, Input, false) of
        true ->
            Host = maps:get(<<"host">>, Input, <<"127.0.0.1">>),
            Port = maps:get(<<"port">>, Input, 0),
            case {validate_host(Host), valid_port(Port)} of
                {{error, Msg}, _} -> {error, Msg};
                {_, false} -> {error, "erlang.mcp.port must be an integer between 0 and 65535"};
                {{ok, Literal}, true} ->
                    resolve_project(Input, Literal, Port)
            end;
        _ ->
            {error, "MCP is disabled: the workspace is not trusted"}
    end.

resolve_project(Input, Host, Port) ->
    Cwd = to_list(maps:get(<<"cwd">>, Input, <<>>)),
    Root = to_list(maps:get(<<"root">>, Input, <<>>)),
    case read_project_policy(Cwd, Root) of
        {error, Msg} -> {error, Msg};
        {ok, none} -> finish(Host, Port, #{});
        {ok, Project} -> finish(Host, Port, Project)
    end.

finish(Host, Port, Project) ->
    Base = (defaults())#{host => Host, port => Port},
    Merged = maps:merge(Base, maps:without([limits], Project)),
    Limits = maps:merge(maps:get(limits, Base), maps:get(limits, Project, #{})),
    Config = Merged#{limits => Limits},
    case validate_config(Config) of
        ok -> {ok, Config};
        {error, Msg} -> {error, Msg}
    end.

valid_port(P) -> is_integer(P) andalso P >= 0 andalso P =< 65535.

to_list(B) when is_binary(B) ->
    case unicode:characters_to_list(B) of
        L when is_list(L) -> L;
        _ -> binary_to_list(B)
    end;
to_list(L) when is_list(L) -> L.

%%------------------------------------------------------------------------------
%% Project policy reader
%%------------------------------------------------------------------------------

%% Nearest rebar.config from Cwd up to Root (inclusive), never above Root.
%% -> {ok, none} | {ok, #{allowed_tools|allowed_ets_tables|limits => ...}} | {error, Msg}
read_project_policy(Cwd, Root) ->
    case find_project_file(Cwd, Root) of
        {error, _} = E -> E;
        none -> {ok, none};
        {ok, File} ->
            case read_terms(File) of
                {ok, Terms} -> project_policy(Terms);
                {error, Msg} -> {error, Msg}
            end
    end.

find_project_file(Cwd, Root) ->
    C = normalize_dir(Cwd),
    R = normalize_dir(Root),
    case C =/= [] andalso R =/= [] andalso within(C, R) of
        true -> walk_up(C, R);
        false -> {error, "MCP is disabled: the debug cwd is outside the selected workspace folder"}
    end.

walk_up(Dir, Root) ->
    File = filename:join(Dir, ?PROJECT_FILE),
    case filelib:is_regular(File) of
        true -> {ok, File};
        false when Dir =:= Root -> none;
        false ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true -> none;
                false -> walk_up(Parent, Root)
            end
    end.

normalize_dir([]) -> [];
normalize_dir(Dir) ->
    Abs = filename:absname(Dir),
    Parts = [P || P <- filename:split(Abs), P =/= "."],
    Resolved = lists:foldl(
                 fun("..", [_ | Acc]) -> Acc;
                    ("..", []) -> [];
                    (P, Acc) -> [P | Acc]
                 end, [], Parts),
    filename:join(lists:reverse(Resolved)).

%% Path-component prefix check (not a string prefix: /a/bc is not within /a/b).
within(Dir, Root) ->
    D = filename:split(Dir),
    R = filename:split(Root),
    lists:prefix(R, D).

read_terms(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{size = Size}} when Size > ?MAX_PROJECT_FILE_BYTES ->
            {error, "rebar.config is too large to read the MCP policy (limit 256 KiB)"};
        {ok, _} ->
            case file:read_file(File) of
                {ok, Bin} -> scan_terms(Bin);
                {error, _} -> {error, "rebar.config cannot be read"}
            end;
        {error, _} ->
            {error, "rebar.config cannot be read"}
    end.

scan_terms(Bin) ->
    case unicode:characters_to_list(Bin) of
        Str when is_list(Str) ->
            try
                collect_terms(Str, 1, 0, [])
            catch
                throw:{malformed, Msg} -> {error, Msg}
            end;
        _ ->
            {error, "rebar.config is not valid text (UTF-8 expected)"}
    end.

collect_terms(Str, Line, Count, Acc) ->
    case erl_scan:tokens([], Str, Line) of
        {done, {ok, Tokens, EndLine}, Rest} ->
            Count1 = Count + length(Tokens),
            Count1 > ?MAX_PROJECT_TOKENS andalso throw({malformed, "rebar.config is too complex to read the MCP policy"}),
            Term = case erl_parse:parse_term(Tokens) of
                       {ok, T} -> T;
                       {error, _} -> throw({malformed, "rebar.config is malformed: it must contain only Erlang terms ending with '.'"})
                   end,
            collect_terms(Rest, EndLine, Count1, [Term | Acc]);
        {done, {eof, _}, _} ->
            {ok, lists:reverse(Acc)};
        {done, {error, _, _}, _} ->
            throw({malformed, "rebar.config is malformed: it cannot be tokenized"});
        {more, Cont} ->
            case erl_scan:tokens(Cont, eof, Line) of
                {done, {ok, [], _}, _} -> {ok, lists:reverse(Acc)};
                {done, {eof, _}, _} -> {ok, lists:reverse(Acc)};
                _ -> throw({malformed, "rebar.config is malformed: incomplete term"})
            end
    end.

project_policy(Terms) ->
    Mcp = [V || {mcp, V} <- Terms],
    case Mcp of
        [] ->
            case profile_policy(Terms) of
                true -> {error, "an 'mcp' policy inside a rebar profile is not supported: "
                                "move it to the top level of rebar.config"};
                false -> {ok, none}
            end;
        [Term] -> normalize_project_term(Term);
        _ -> {error, "rebar.config has several top-level 'mcp' terms"}
    end.

profile_policy(Terms) ->
    lists:any(
      fun({profiles, Profiles}) when is_list(Profiles) ->
              lists:any(fun({_Name, Opts}) when is_list(Opts) -> lists:keymember(mcp, 1, Opts);
                           (_) -> false
                        end, Profiles);
         (_) -> false
      end, Terms).

%% The {mcp, Term} value -> {ok, #{...}} | {error, Msg}
normalize_project_term(Term) when is_list(Term) ->
    try
        Keys = [element(1, T) || T <- Term, is_tuple(T), tuple_size(T) =:= 2],
        length(Keys) =:= length(Term) orelse throw("the mcp term must be a list of {Key, Value} pairs"),
        length(Keys) =:= length(lists:usort(Keys)) orelse throw("the mcp term has duplicate keys"),
        lists:foreach(
          fun(K) ->
                  lists:member(K, ?FORBIDDEN_PROJECT_KEYS) andalso
                      throw(io_lib:format("'~w' cannot be set in rebar.config: it is owned by the VS Code Erlang settings", [K])),
                  lists:member(K, [allowed_tools, allowed_ets_tables, limits]) orelse
                      throw(io_lib:format("unknown key '~w' in the mcp term", [K]))
          end, Keys),
        {ok, lists:foldl(fun project_key/2, #{}, Term)}
    catch
        throw:Msg when is_list(Msg) -> {error, "invalid mcp policy: " ++ Msg};
        throw:Msg -> {error, "invalid mcp policy: " ++ lists:flatten(Msg)}
    end;
normalize_project_term(_) ->
    {error, "invalid mcp policy: the mcp term must be a list"}.

project_key({allowed_tools, Tools}, Acc) ->
    Names = binary_list(Tools, "allowed_tools"),
    length(Names) =:= length(lists:usort(Names)) orelse throw("allowed_tools has duplicates"),
    lists:foreach(fun(N) -> lists:member(N, ?TOOLS) orelse throw("allowed_tools has an unknown tool name") end, Names),
    Acc#{allowed_tools => Names};
project_key({allowed_ets_tables, all}, Acc) ->
    Acc#{allowed_ets_tables => all};
project_key({allowed_ets_tables, Tables}, Acc) ->
    Names = binary_list(Tables, "allowed_ets_tables"),
    length(Names) =:= length(lists:usort(Names)) orelse throw("allowed_ets_tables has duplicates"),
    lists:foreach(fun(N) -> byte_size(N) > 0 andalso byte_size(N) =< 255 orelse
                                throw("allowed_ets_tables entries must be 1..255 bytes") end, Names),
    Acc#{allowed_ets_tables => Names};
project_key({limits, Limits}, Acc) ->
    is_list(Limits) orelse throw("limits must be a list"),
    Pairs = [case L of {K, V} -> {K, V}; _ -> throw("limits must be {Key, Integer} pairs") end || L <- Limits],
    Ks = [K || {K, _} <- Pairs],
    length(Ks) =:= length(lists:usort(Ks)) orelse throw("limits has duplicate keys"),
    Map = lists:foldl(
            fun({K, V}, M) ->
                    case lists:keyfind(K, 2, ?LIMITS) of
                        false -> throw(io_lib:format("unknown limit '~w'", [K]));
                        {Internal, _, _, _, Ceiling} ->
                            (is_integer(V) andalso V > 0 andalso V =< Ceiling) orelse
                                throw(io_lib:format("limit '~w' must be a positive integer <= ~w", [K, Ceiling])),
                            M#{Internal => V}
                    end
            end, #{}, Pairs),
    Acc#{limits => Map}.

binary_list(L, What) when is_list(L) ->
    lists:foreach(fun(B) -> is_binary(B) orelse throw(What ++ " entries must be binaries such as <<\"name\">>") end, L),
    L;
binary_list(_, What) ->
    throw(What ++ " must be a list").

%%------------------------------------------------------------------------------
%% Authoritative validation (runs again in the target before any listener).
%%------------------------------------------------------------------------------

validate_config(#{host := Host, port := Port, allowed_tools := Tools,
                  allowed_ets_tables := Tables, limits := Limits} = Config) ->
    Known = [host, port, allowed_tools, allowed_ets_tables, limits],
    Extra = maps:keys(maps:without(Known, Config)),
    Checks = [fun() -> Extra =:= [] orelse {error, "unknown MCP configuration key"} end,
              fun() -> case literal_loopback(Host) of true -> ok; false -> {error, "MCP host must be a loopback address literal"} end end,
              fun() -> valid_port(Port) orelse {error, "MCP port must be an integer between 0 and 65535"} end,
              fun() -> (is_list(Tools) andalso lists:all(fun(T) -> lists:member(T, ?TOOLS) end, Tools)
                        andalso length(Tools) =:= length(lists:usort(Tools)))
                           orelse {error, "invalid MCP tool allowlist"} end,
              fun() -> Tables =:= all orelse
                           (is_list(Tables) andalso lists:all(fun is_binary/1, Tables)
                            andalso length(Tables) =:= length(lists:usort(Tables)))
                           orelse {error, "invalid MCP ETS table allowlist"} end,
              fun() -> validate_limits(Limits) end],
    run_checks(Checks);
validate_config(_) ->
    {error, "incomplete MCP configuration"}.

run_checks([]) -> ok;
run_checks([C | T]) ->
    case C() of
        ok -> run_checks(T);
        true -> run_checks(T);
        {error, _} = E -> E
    end.

literal_loopback(Host) when is_list(Host) ->
    case inet:parse_strict_address(Host) of
        {ok, Ip} -> is_loopback(Ip);
        _ -> false
    end;
literal_loopback(_) -> false.

validate_limits(Limits) when is_map(Limits) ->
    Expected = [K || {K, _, _, _, _} <- ?LIMITS],
    case lists:sort(maps:keys(Limits)) =:= lists:sort(Expected) of
        false -> {error, "MCP limits are incomplete or unknown"};
        true ->
            Bad = [K || {K, _, _, _, Ceiling} <- ?LIMITS,
                        V <- [maps:get(K, Limits)],
                        not (is_integer(V) andalso V > 0 andalso V =< Ceiling)],
            case Bad of
                [] -> validate_relations(Limits);
                _ -> {error, lists:flatten(io_lib:format("MCP limit(s) out of range: ~w", [Bad]))}
            end
    end;
validate_limits(_) ->
    {error, "MCP limits must be a map"}.

%% Error and result envelopes must fit.
validate_relations(#{max_result_bytes := Result, max_request_bytes := Request,
                     max_collection_bytes := Coll, max_items := Items, max_binary_bytes := Bin}) ->
    if Result < 4096 -> {error, "max_result_bytes must be at least 4096"};
       Request < 1024 -> {error, "max_request_bytes must be at least 1024"};
       Coll < Result -> {error, "max_collection_bytes must be at least max_result_bytes"};
       Bin > Result div 4 -> {error, "max_binary_bytes must not exceed a quarter of max_result_bytes"};
       Items < 1 -> {error, "max_items must be positive"};
       true -> ok
    end.

%%------------------------------------------------------------------------------
%% JSON form (crosses adapter -> target). Only fixed atoms are produced.
%%------------------------------------------------------------------------------

to_json_map(#{host := Host, port := Port, allowed_tools := Tools,
              allowed_ets_tables := Tables, limits := Limits}) ->
    #{<<"host">> => list_to_binary(Host),
      <<"port">> => Port,
      <<"allowedTools">> => Tools,
      <<"allowedETSTables">> => case Tables of all -> <<"*">>; _ -> Tables end,
      <<"limits">> => maps:from_list([{J, maps:get(K, Limits)} || {K, _, J, _, _} <- ?LIMITS])}.

from_json_map(#{<<"host">> := Host, <<"port">> := Port, <<"allowedTools">> := Tools,
                <<"allowedETSTables">> := Tables, <<"limits">> := Limits} = M)
  when is_binary(Host), is_map(Limits) ->
    Known = [<<"host">>, <<"port">>, <<"allowedTools">>, <<"allowedETSTables">>,
             <<"limits">>, <<"mode">>, <<"authToken">>],
    case maps:keys(maps:without(Known, M)) of
        [] ->
          case token_of(M) of
           {error, _} = TE -> TE;
           _ ->
            KnownLimits = [J || {_, _, J, _, _} <- ?LIMITS],
            case maps:keys(maps:without(KnownLimits, Limits)) of
                [] ->
                    Internal = maps:from_list([{K, maps:get(J, Limits)}
                                               || {K, _, J, _, _} <- ?LIMITS, maps:is_key(J, Limits)]),
                    Config = #{host => binary_to_list(Host), port => Port,
                               allowed_tools => Tools,
                               allowed_ets_tables => case Tables of <<"*">> -> all; _ -> Tables end,
                               limits => Internal},
                    case validate_config(Config) of
                        ok -> {ok, Config};
                        {error, _} = E -> E
                    end;
                _ -> {error, "unknown MCP limit"}
            end
          end;
        _ -> {error, "unknown MCP configuration key"}
    end;
from_json_map(_) ->
    {error, "invalid MCP configuration"}.

%% Optional fixed token (development convenience, erlang.mcp.authToken): when
%% absent the session gets a fresh random token. 16..128 URL-safe characters.
validate_token(T) when is_binary(T), byte_size(T) >= 16, byte_size(T) =< 128 ->
    case re:run(T, "^[A-Za-z0-9._~-]+\\z", [{capture, none}]) of
        match -> ok;
        nomatch -> {error, "erlang.mcp.authToken may only contain letters, digits and ._~-"}
    end;
validate_token(_) ->
    {error, "erlang.mcp.authToken must be 16 to 128 characters"}.

token_of(#{<<"authToken">> := T}) -> validate_token(T);
token_of(_) -> ok.

%%------------------------------------------------------------------------------
%% Helper VM entry point: erl -noshell -pa ebin -s mcp_policy cli
%% Reads one JSON line from stdin, prints one JSON line, halts.
%%------------------------------------------------------------------------------

cli() ->
    Reply = try
                catch io:setopts(standard_io, [{encoding, unicode}]),
                case io:get_line("") of
                    Line when is_list(Line), length(Line) < 8192 ->
                        {ok, Input, _} = vscode_jsone_decode:decode(unicode:characters_to_binary(Line)),
                        case resolve(Input) of
                            disabled -> #{status => <<"disabled">>};
                            {ok, Config} -> #{status => <<"ok">>, config => to_json_map(Config)};
                            {error, Msg} -> #{status => <<"error">>, message => unicode:characters_to_binary(Msg)}
                        end;
                    _ ->
                        #{status => <<"error">>, message => <<"invalid MCP policy request">>}
                end
            catch
                _:_ -> #{status => <<"error">>, message => <<"MCP policy could not be resolved">>}
            end,
    {ok, Json} = vscode_jsone:encode(Reply),
    io:format("~s~n", [Json]),
    init:stop().

