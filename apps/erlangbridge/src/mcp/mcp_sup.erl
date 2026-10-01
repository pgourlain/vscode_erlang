%% Dedicated supervisor of the embedded MCP inspector, plus the session API
%% used by the debugger bridge (vscode_connection).
%%
%% The subtree is started only on demand for a debug session and is completely
%% separate from the LSP supervision tree. It fails closed: any child exit
%% (listener failure, lease expiry, crash) shuts the whole subtree down; it is
%% never restarted with a different port or token.
%%
%% A `mcp_holder` process owns the supervisor link so that the subtree outlives
%% the (rpc) process that requested it.
-module(mcp_sup).
-behaviour(supervisor).

-export([start_session/2, start_session/3, start_session/4, stop_session/0, renew/0, active/0]).
-export([init/1]).
%% spawned
-export([holder/6]).

-define(START_TIMEOUT_MS, 5000).

%% Config: normalized configuration (already validated by the caller and again here).
%% Mode: launch | attach
%% -> {ok, #{host, port, path, sessionId, token}} | {error, Reason :: binary()}
start_session(Config, Mode) ->
    start_session(Config, Mode, undefined).

%% FixedToken: undefined (random per session) or a validated developer token.
start_session(Config, Mode, FixedToken) ->
    start_session(Config, Mode, FixedToken, #{}).

%% Opts: #{notify => fun((map()) -> any())} - receives one non-secret metadata
%% map per MCP request (method, tool, status, duration, sizes); never arguments,
%% results or the token.
start_session(Config, Mode, FixedToken, Opts) ->
    case mcp_policy:validate_config(Config) of
        {error, Msg} -> {error, list_to_binary(Msg)};
        ok ->
            case whereis(mcp_holder) of
                undefined ->
                    Token = case FixedToken of undefined -> new_token(); _ -> FixedToken end,
                    Ref = make_ref(),
                    Caller = self(),
                    Holder = spawn(?MODULE, holder, [Caller, Ref, Config, Mode, Token, Opts]),
                    receive
                        {Ref, {ok, Info}} -> {ok, Info#{token => Token}};
                        {Ref, {error, Reason}} -> {error, describe(Reason)}
                    after ?START_TIMEOUT_MS ->
                            exit(Holder, kill),
                            {error, <<"the MCP inspector did not start in time">>}
                    end;
                _ ->
                    {error, <<"an MCP inspector is already running on this node">>}
            end
    end.

stop_session() ->
    case whereis(mcp_holder) of
        undefined -> ok;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {stop, self()},
            receive {'DOWN', Ref, process, Pid, _} -> ok
            after 3000 ->
                    exit(Pid, kill),
                    erlang:demonitor(Ref, [flush]),
                    ok
            end
    end.

renew() ->
    case whereis(mcp_server) of
        undefined -> {error, not_running};
        _ -> mcp_server:renew(), ok
    end.

active() -> whereis(mcp_holder) =/= undefined.

%%------------------------------------------------------------------------------

new_token() ->
    B = base64:encode(crypto:strong_rand_bytes(32)),
    << <<(url_safe(C))>> || <<C>> <= B, C =/= $= >>.

url_safe($+) -> $-;
url_safe($/) -> $_;
url_safe(C) -> C.

describe({shutdown, {failed_to_start_child, mcp_server, {listen_failed, eaddrinuse}}}) ->
    <<"the configured erlang.mcp.port is already in use; MCP is disabled for this session">>;
describe({shutdown, {failed_to_start_child, mcp_server, {listen_failed, Reason}}}) ->
    iolist_to_binary(io_lib:format("the MCP listener cannot bind (~w)", [Reason]));
describe(_) ->
    <<"the MCP inspector could not start">>.

holder(Caller, Ref, Config, Mode, Token, Opts) ->
    try register(mcp_holder, self()) of
        true -> holder_start(Caller, Ref, Config, Mode, Token, Opts)
    catch error:badarg ->
            Caller ! {Ref, {error, already_started}}
    end.

holder_start(Caller, Ref, Config, Mode, Token, Opts) ->
    process_flag(trap_exit, true),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, {Config, Mode, Token, maps:get(notify, Opts, undefined)}) of
        {ok, Sup} ->
            Info = mcp_server:info(),
            Caller ! {Ref, {ok, Info#{sessionId => mcp_store:session_id()}}},
            holder_loop(Sup);
        {error, Reason} ->
            Caller ! {Ref, {error, Reason}}
    end.

holder_loop(Sup) ->
    receive
        {stop, _From} ->
            exit(Sup, shutdown),
            receive {'EXIT', Sup, _} -> ok after 3000 -> ok end;
        {'EXIT', Sup, Reason} ->
            mcp_audit:event(supervisor_exit, #{reason => Reason});
        _ ->
            holder_loop(Sup)
    end.

init({Config, Mode, Token, Notify}) ->
    Children = [#{id => mcp_store, start => {mcp_store, start_link, [Config, Mode, Notify]},
                  restart => permanent, shutdown => 2000, type => worker, modules => [mcp_store]},
                #{id => mcp_server, start => {mcp_server, start_link, [Config, Token]},
                  restart => permanent, shutdown => 2000, type => worker, modules => [mcp_server]}],
    {ok, {#{strategy => one_for_all, intensity => 0, period => 1}, Children}}.
