%% Authenticated MCP endpoint (Streamable HTTP, JSON responses only) of the
%% embedded inspector.
%%
%% Pinned protocol revision : 2025-06-18. Stateless: no Mcp-Session-Id is
%% issued, no SSE stream (GET -> 405). Implemented methods: initialize,
%% notifications/* (202), ping, tools/list, tools/call. No prompts, no
%% resources. Batches are rejected (removed in this revision).
%%
%% Security: explicit loopback bind, Host/Origin validation, no CORS, bearer
%% token required on every request *before* the body is read, body size cap,
%% header/body/connection timeouts, bounded connections and concurrent tool
%% calls. Tokens and parameters are never logged.
-module(mcp_server).
-behaviour(gen_server).

-export([start_link/2, info/0, renew/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% spawned processes
-export([acceptor/3, conn_main/2]).

-define(PROTOCOL_VERSION, <<"2025-06-18">>).
-define(PATH, <<"/mcp">>).
-define(LEASE_MS, 8000).
-define(LEASE_CHECK_MS, 1000).
-define(IO_TIMEOUT_MS, 5000).
-define(CONN_LIFETIME_MS, 30000).
-define(MAX_HEADERS, 64).

-record(state, {listen, port, host, token, config, lease_deadline, conns = #{}, max_conns}).

start_link(Config, Token) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Config, Token}, []).

info() -> gen_server:call(?MODULE, info).

%% Lease renewal from the debug adapter. It runs in the server process, never in
%% an application process, so it keeps working while the application is stopped
%% at a breakpoint.
renew() -> gen_server:cast(?MODULE, renew).

%%------------------------------------------------------------------------------

init({Config, Token}) ->
    process_flag(trap_exit, true),
    Host = maps:get(host, Config),
    {ok, Ip} = inet:parse_strict_address(Host),
    Family = case tuple_size(Ip) of 4 -> []; 8 -> [inet6] end,
    Opts = Family ++ [binary, {active, false}, {reuseaddr, true}, {ip, Ip},
                      {packet, raw}, {backlog, 16}, {send_timeout, ?IO_TIMEOUT_MS},
                      {send_timeout_close, true}],
    case gen_tcp:listen(maps:get(port, Config), Opts) of
        {ok, L} ->
            {ok, Port} = inet:port(L),
            Max = mcp_policy:limit(max_concurrency, Config) * 4,
            State = #state{listen = L, port = Port, host = Host, token = Token, config = Config,
                           lease_deadline = mono() + ?LEASE_MS, max_conns = Max},
            spawn_link(?MODULE, acceptor, [L, self(), Token]),
            erlang:send_after(?LEASE_CHECK_MS, self(), lease_check),
            mcp_audit:event(listening, #{port => Port}),
            {ok, State};
        {error, Reason} ->
            mcp_audit:event(listen_failed, #{reason => Reason}),
            {stop, {listen_failed, Reason}}
    end.

handle_call(info, _, #state{host = Host, port = Port} = S) ->
    {reply, #{host => Host, port => Port, path => ?PATH}, S};
handle_call({conn, Pid}, _, #state{conns = Conns, max_conns = Max} = S) ->
    case maps:size(Conns) >= Max of
        true -> {reply, busy, S};
        false ->
            Ref = erlang:monitor(process, Pid),
            erlang:send_after(?CONN_LIFETIME_MS, self(), {conn_timeout, Pid}),
            {reply, ok, S#state{conns = Conns#{Pid => Ref}}}
    end;
handle_call(_, _, S) ->
    {reply, {error, unsupported}, S}.

handle_cast(renew, S) ->
    {noreply, S#state{lease_deadline = mono() + ?LEASE_MS}};
handle_cast(_, S) ->
    {noreply, S}.

handle_info(lease_check, #state{lease_deadline = D} = S) ->
    case mono() > D of
        true ->
            mcp_audit:event(lease_expired, #{}),
            {stop, lease_expired, S};
        false ->
            erlang:send_after(?LEASE_CHECK_MS, self(), lease_check),
            {noreply, S}
    end;
handle_info({'DOWN', _, process, Pid, _}, #state{conns = Conns} = S) ->
    {noreply, S#state{conns = maps:remove(Pid, Conns)}};
handle_info({conn_timeout, Pid}, S) ->
    exit(Pid, kill),
    {noreply, S};
handle_info({'EXIT', _Acceptor, Reason}, S) ->
    {stop, {acceptor_exit, Reason}, S};
handle_info(_, S) ->
    {noreply, S}.

terminate(_, #state{listen = L, conns = Conns}) ->
    catch gen_tcp:close(L),
    [exit(Pid, kill) || Pid <- maps:keys(Conns)],
    ok.

mono() -> erlang:monotonic_time(millisecond).

%%------------------------------------------------------------------------------
%% Acceptor and connections
%%------------------------------------------------------------------------------

acceptor(L, Server, Token) ->
    case gen_tcp:accept(L) of
        {ok, Sock} ->
            Pid = spawn(?MODULE, conn_main, [Sock, {Server, Token}]),
            ok = gen_tcp:controlling_process(Sock, Pid),
            Pid ! case gen_server:call(Server, {conn, Pid}) of
                      ok -> go;
                      busy -> busy
                  end,
            acceptor(L, Server, Token);
        {error, _} ->
            exit(normal)
    end.

conn_main(Sock, {Server, Token}) ->
    receive
        go ->
            try
                handle_connection(Sock, Token)
            catch
                _:_ -> reply(Sock, 500, [], <<"{\"error\":\"internal error\"}">>)
            end;
        busy ->
            reply(Sock, 503, [{<<"Retry-After">>, <<"1">>}], <<"{\"error\":\"too many connections\"}">>)
    after ?IO_TIMEOUT_MS ->
            ok
    end,
    _ = Server,
    gen_tcp:close(Sock).

handle_connection(Sock, Token) ->
    Config = mcp_store:config(),
    #{port := Port} = ?MODULE:info(),
    Limits = maps:get(limits, Config),
    MaxBody = maps:get(max_request_bytes, Limits),
    ok = inet:setopts(Sock, [{packet, http_bin}, {packet_size, 8192}]),
    case read_head(Sock) of
        {ok, Method, Path, Headers} ->
            case check_head(Method, Path, Headers, Port, Config, Token, MaxBody) of
                {reject, Code, Extra, Body} -> reply(Sock, Code, Extra, Body);
                {ok, Len} ->
                    ok = inet:setopts(Sock, [{packet, raw}]),
                    case read_body(Sock, Len) of
                        {ok, Body} -> serve(Sock, Body, Config);
                        error -> reply(Sock, 400, [], err_body(<<"incomplete request body">>))
                    end
            end;
        error ->
            reply(Sock, 400, [], err_body(<<"malformed request">>))
    end.

read_head(Sock) ->
    case gen_tcp:recv(Sock, 0, ?IO_TIMEOUT_MS) of
        {ok, {http_request, Method, {abs_path, Path}, _}} ->
            case read_headers(Sock, #{}, 0) of
                {ok, Headers} -> {ok, Method, Path, Headers};
                error -> error
            end;
        _ -> error
    end.

read_headers(_, _, N) when N > ?MAX_HEADERS -> error;
read_headers(Sock, Acc, N) ->
    case gen_tcp:recv(Sock, 0, ?IO_TIMEOUT_MS) of
        {ok, http_eoh} -> {ok, Acc};
        {ok, {http_header, _, Field, _, Value}} ->
            Name = header_name(Field),
            case maps:is_key(Name, Acc) of
                true when Name =:= <<"host">>; Name =:= <<"authorization">>;
                          Name =:= <<"content-length">>; Name =:= <<"origin">> ->
                    error;     %% duplicated security-relevant header
                _ -> read_headers(Sock, Acc#{Name => Value}, N + 1)
            end;
        _ -> error
    end.

header_name(A) when is_atom(A) -> lower(atom_to_binary(A, utf8));
header_name(B) when is_binary(B) -> lower(B).

lower(B) -> list_to_binary(string:to_lower(binary_to_list(B))).

%% Cheap checks first; the bearer token before the body is read.
check_head(Method, Path, Headers, Port, Config, Token, MaxBody) ->
    Checks = [fun() -> Path =:= ?PATH orelse {reject, 404, [], err_body(<<"not found">>)} end,
              fun() -> Method =:= 'POST' orelse
                           {reject, 405, [{<<"Allow">>, <<"POST">>}], err_body(<<"method not allowed">>)} end,
              fun() -> host_ok(maps:get(<<"host">>, Headers, undefined), Port, Config) orelse
                           {reject, 403, [], err_body(<<"forbidden host">>)} end,
              fun() -> origin_ok(maps:get(<<"origin">>, Headers, undefined), Port, Config) orelse
                           {reject, 403, [], err_body(<<"forbidden origin">>)} end,
              fun() -> auth_ok(maps:get(<<"authorization">>, Headers, undefined), Token) orelse
                           {reject, 401, [{<<"WWW-Authenticate">>, <<"Bearer">>}], err_body(<<"unauthorized">>)} end,
              fun() -> content_type_ok(maps:get(<<"content-type">>, Headers, undefined)) orelse
                           {reject, 415, [], err_body(<<"content type must be application/json">>)} end,
              fun() -> not maps:is_key(<<"transfer-encoding">>, Headers) orelse
                           {reject, 411, [], err_body(<<"content-length required">>)} end],
    case run_checks(Checks) of
        ok -> length_of(maps:get(<<"content-length">>, Headers, undefined), MaxBody);
        Reject ->
            mcp_audit:event(rejected, #{code => element(2, Reject)}),
            journal(#{method => <<"http">>, status => integer_to_binary(element(2, Reject))}),
            Reject
    end.

run_checks([]) -> ok;
run_checks([C | T]) ->
    case C() of
        true -> run_checks(T);
        {reject, _, _, _} = R -> R
    end.

length_of(undefined, _) -> {reject, 411, [], err_body(<<"content-length required">>)};
length_of(Bin, Max) ->
    try binary_to_integer(Bin) of
        N when N >= 0, N =< Max -> {ok, N};
        N when N > Max -> {reject, 413, [], err_body(<<"request too large">>)};
        _ -> {reject, 400, [], err_body(<<"invalid content-length">>)}
    catch _:_ -> {reject, 400, [], err_body(<<"invalid content-length">>)}
    end.

allowed_hosts(Port, Config) ->
    P = integer_to_binary(Port),
    case inet:parse_strict_address(maps:get(host, Config)) of
        {ok, {127, 0, 0, 1}} -> [<<"127.0.0.1:", P/binary>>, <<"localhost:", P/binary>>];
        {ok, {127, _, _, _} = Ip} -> [iolist_to_binary([inet:ntoa(Ip), ":", P])];
        {ok, _} -> [<<"[::1]:", P/binary>>, <<"localhost:", P/binary>>]
    end.

host_ok(undefined, _, _) -> false;
host_ok(Host, Port, Config) -> lists:member(lower(Host), allowed_hosts(Port, Config)).

%% No Origin (non-browser client) is accepted; a browser Origin must itself be loopback.
origin_ok(undefined, _, _) -> true;
origin_ok(Origin, Port, Config) ->
    lists:any(fun(H) -> lower(Origin) =:= <<"http://", H/binary>> end, allowed_hosts(Port, Config)).

auth_ok(undefined, _) -> false;
auth_ok(Value, Token) ->
    case binary:split(Value, <<" ">>) of
        [Scheme, Presented] -> lower(Scheme) =:= <<"bearer">> andalso ct_eq(Presented, Token);
        _ -> false
    end.

%% Timing-resistant comparison (length is not secret: tokens have a fixed size).
ct_eq(A, B) when byte_size(A) =:= byte_size(B) ->
    ct_fold(binary_to_list(A), binary_to_list(B), 0) =:= 0;
ct_eq(_, _) -> false.

ct_fold([], [], Acc) -> Acc;
ct_fold([X | Xs], [Y | Ys], Acc) -> ct_fold(Xs, Ys, Acc bor (X bxor Y)).

content_type_ok(undefined) -> false;
content_type_ok(Value) ->
    [Type | _] = binary:split(lower(Value), <<";">>),
    trim(Type) =:= <<"application/json">>.

trim(B) -> list_to_binary(string:strip(binary_to_list(B))).

read_body(_Sock, 0) -> {ok, <<>>};
read_body(Sock, Len) ->
    case gen_tcp:recv(Sock, Len, ?IO_TIMEOUT_MS) of
        {ok, Bin} -> {ok, Bin};
        _ -> error
    end.

%%------------------------------------------------------------------------------
%% JSON-RPC
%%------------------------------------------------------------------------------

serve(Sock, Body, Config) ->
    T0 = erlang:monotonic_time(millisecond),
    {Code, Resp, Meta} = handle_rpc(Body, Config),
    reply(Sock, Code, [], Resp),
    journal(Meta#{durationMs => erlang:monotonic_time(millisecond) - T0,
                  bytes => byte_size(Resp)}).

%% -> {HttpCode, ResponseBody, JournalMeta}
handle_rpc(Body, Config) ->
    case decode(Body) of
        {ok, #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Req} when is_binary(Method) ->
            Id = maps:get(<<"id">>, Req, undefined),
            Params = maps:get(<<"params">>, Req, #{}),
            case {Id, is_map(Params)} of
                {undefined, _} ->
                    %% notification (e.g. notifications/initialized, notifications/cancelled)
                    {202, <<>>, #{method => Method, status => <<"ok">>}};
                {_, false} ->
                    rpc(error_obj(Id, -32602, <<"params must be an object">>), Method, Params);
                {_, true} when is_integer(Id); is_binary(Id) ->
                    rpc(dispatch(Method, Params, Id, Config), Method, Params);
                _ ->
                    rpc(error_obj(null, -32600, <<"invalid request id">>), Method, #{})
            end;
        {ok, L} when is_list(L) ->
            rpc(error_obj(null, -32600, <<"batch requests are not supported">>), <<"batch">>, #{});
        {ok, _} ->
            rpc(error_obj(null, -32600, <<"invalid request">>), <<"invalid">>, #{});
        error ->
            {400, encode(error_obj(null, -32700, <<"parse error">>)),
             #{method => <<"invalid">>, status => <<"-32700">>}}
    end.

rpc(Obj, Method, Params) ->
    {200, encode(Obj), journal_meta(Method, Params, Obj)}.

%% Request journal (see mcp_store:notify/1): request metadata only - never the
%% arguments, the result content or the token. The tool name is only kept when
%% it is a known tool, so a client cannot inject text in the journal.
journal_meta(Method, Params, Obj) ->
    M0 = #{method => known_method(Method)},
    M1 = case Method of
             <<"tools/call">> ->
                 Tool = maps:get(<<"name">>, Params, undefined),
                 Args = maps:get(<<"arguments">>, Params, #{}),
                 M0#{tool => case is_binary(Tool) andalso mcp_tools:known(Tool) of
                                 true -> Tool;
                                 false -> <<"?">>
                             end,
                     cursor => is_map(Args) andalso maps:is_key(<<"cursor">>, Args)};
             _ -> M0
         end,
    maps:merge(M1, outcome(Obj)).

known_method(M) ->
    Known = [<<"initialize">>, <<"ping">>, <<"tools/list">>, <<"tools/call">>,
             <<"notifications/initialized">>, <<"notifications/cancelled">>],
    case lists:member(M, Known) of
        true -> M;
        false -> <<"other">>
    end.

outcome(#{<<"error">> := #{<<"code">> := Code}}) ->
    #{status => integer_to_binary(Code)};
outcome(#{<<"result">> := #{<<"isError">> := true, <<"structuredContent">> := #{<<"error">> := #{<<"code">> := C}}}}) ->
    #{status => C};
outcome(#{<<"result">> := #{<<"structuredContent">> := S}}) when is_map(S) ->
    Graph = case S of
                #{<<"entities">> := E, <<"relationships">> := R} ->
                    #{entities => length(E), relationships => length(R),
                      omissions => length(maps:get(<<"omissions">>, S, [])),
                      complete => maps:get(<<"complete">>, S, true),
                      more => is_binary(maps:get(<<"nextCursor">>, S, null)),
                      offset => maps:get(<<"offset">>, maps:get(<<"page">>, S, #{}), 0)};
                _ -> #{}
            end,
    Graph#{status => <<"ok">>};
outcome(_) ->
    #{status => <<"ok">>}.

journal(Meta) ->
    catch mcp_store:notify(Meta),
    ok.

decode(Body) ->
    try vscode_jsone_decode:decode(Body) of
        {ok, Term, Rest} ->
            case binary:replace(Rest, [<<" ">>, <<"\r">>, <<"\n">>, <<"\t">>], <<>>, [global]) of
                <<>> -> {ok, Term};
                _ -> error
            end;
        _ -> error
    catch _:_ -> error
    end.

encode(Term) ->
    {ok, Json} = vscode_jsone:encode(Term),
    iolist_to_binary(Json).

error_obj(Id, Code, Message) ->
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
      <<"error">> => #{<<"code">> => Code, <<"message">> => Message}}.

result_obj(Id, Result) ->
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"result">> => Result}.

dispatch(<<"initialize">>, Params, Id, _Config) ->
    case maps:get(<<"protocolVersion">>, Params, undefined) of
        V when is_binary(V) ->
            %% version negotiation: the server answers with the revision it implements
            result_obj(Id, #{<<"protocolVersion">> => ?PROTOCOL_VERSION,
                             <<"capabilities">> => #{<<"tools">> => #{<<"listChanged">> => false}},
                             <<"serverInfo">> => #{<<"name">> => <<"erlang-otp-topology-inspector">>,
                                                   <<"title">> => <<"Erlang OTP topology inspector">>,
                                                   <<"version">> => <<"1.0.0">>},
                             <<"instructions">> =>
                                 <<"Read-only structural map of the debugged Erlang node. Workflow: runtime_summary, "
                                   "then application_overview (no modules), pick the application and follow its 'roots' "
                                   "ids with supervision_tree (expand deeper subtrees with the ids given in 'limit_reached' "
                                   "omissions), then process_info / registered_processes(application=id) / ets_tables / "
                                   "debug_session only where needed; top_processes finds hot or stuck processes in one call. Ids are reusable across tools. Default detail=summary "
                                   "keeps answers small; follow nextCursor only if you need the remaining items. Use "
                                   "detail=full only when the user asks for the complete JSON (e.g. to save it to a file). "
                                   "Maps are bounded observation intervals, not atomic snapshots: honour complete/"
                                   "truncated/omissions. Structural edges are not message traffic.">>});
        _ ->
            error_obj(Id, -32602, <<"protocolVersion is required">>)
    end;
dispatch(<<"ping">>, _, Id, _) ->
    result_obj(Id, #{});
dispatch(<<"tools/list">>, _, Id, Config) ->
    result_obj(Id, #{<<"tools">> => mcp_tools:list(Config)});
dispatch(<<"tools/call">>, Params, Id, Config) ->
    tools_call(Params, Id, Config);
dispatch(_, _, Id, _) ->
    error_obj(Id, -32601, <<"method not found">>).

tools_call(Params, Id, Config) ->
    Name = maps:get(<<"name">>, Params, undefined),
    Args = maps:get(<<"arguments">>, Params, #{}),
    Known = is_binary(Name) andalso mcp_tools:known(Name) andalso mcp_policy:tool_allowed(Name, Config),
    case Known of
        false ->
            error_obj(Id, -32602, <<"unknown tool">>);
        true ->
            case mcp_tools:validate_args(Name, Args) of
                {error, Msg} -> error_obj(Id, -32602, Msg);
                ok -> run_tool(Name, Args, Id, Config)
            end
    end.

run_tool(Name, Args, Id, Config) ->
    case mcp_store:acquire_slot(self()) of
        busy ->
            mcp_audit:event(tool_call, #{tool => Name, status => busy}),
            error_obj(Id, -32000, <<"inspector busy: too many concurrent requests, retry later">>);
        ok ->
            T0 = erlang:monotonic_time(millisecond),
            Timeout = mcp_policy:limit(request_timeout_ms, Config),
            Ctx = #{config => Config, timeout_ms => Timeout},
            Res = mcp_runtime:bounded(fun() -> mcp_runtime:call(Name, Args, Ctx) end, Timeout),
            mcp_store:release_slot(self()),
            {Status, Result} = tool_result(Res, Config),
            mcp_audit:event(tool_call, #{tool => Name, status => Status,
                                         duration_ms => erlang:monotonic_time(millisecond) - T0}),
            result_obj(Id, Result)
    end.

tool_result({ok, {ok, Structured}}, Config) ->
    Result = #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => encode(Structured)}],
               <<"structuredContent">> => Structured,
               <<"isError">> => false},
    Max = mcp_policy:limit(max_result_bytes, Config),
    case byte_size(encode(result_obj(0, Result))) =< Max of
        true -> {ok, Result};
        false -> {result_too_large, tool_error(<<"result_too_large">>,
                                               <<"the result exceeds max_result_bytes; narrow the scope">>)}
    end;
tool_result({ok, {error, Code, Message}}, _) ->
    {error, tool_error(Code, Message)};
tool_result(timeout, _) ->
    {timeout, tool_error(<<"timeout">>, <<"the inspection exceeded the request time limit; narrow the scope or retry">>)};
tool_result({error, _}, _) ->
    %% no stack trace or internal term is exposed
    {error, tool_error(<<"internal_error">>, <<"the inspection failed">>)}.

tool_error(Code, Message) ->
    Structured = #{<<"error">> => #{<<"code">> => Code, <<"message">> => Message}},
    #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => encode(Structured)}],
      <<"structuredContent">> => Structured,
      <<"isError">> => true}.

err_body(Message) ->
    encode(#{<<"error">> => Message}).

%%------------------------------------------------------------------------------
%% HTTP response
%%------------------------------------------------------------------------------

reply(Sock, Code, Extra, Body) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"Content-Length">>, integer_to_binary(byte_size(Body))},
               {<<"Cache-Control">>, <<"no-store">>},
               {<<"Connection">>, <<"close">>} | Extra],
    Head = [<<"HTTP/1.1 ">>, integer_to_binary(Code), $\s, reason(Code), <<"\r\n">>,
            [[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- Headers], <<"\r\n">>],
    catch gen_tcp:send(Sock, [Head, Body]),
    ok.

reason(200) -> <<"OK">>;
reason(202) -> <<"Accepted">>;
reason(400) -> <<"Bad Request">>;
reason(401) -> <<"Unauthorized">>;
reason(403) -> <<"Forbidden">>;
reason(404) -> <<"Not Found">>;
reason(405) -> <<"Method Not Allowed">>;
reason(411) -> <<"Length Required">>;
reason(413) -> <<"Payload Too Large">>;
reason(415) -> <<"Unsupported Media Type">>;
reason(500) -> <<"Internal Server Error">>;
reason(503) -> <<"Service Unavailable">>;
reason(_) -> <<"Error">>.
