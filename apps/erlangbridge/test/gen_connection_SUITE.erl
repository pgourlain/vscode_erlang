-module(gen_connection_SUITE).
-behaviour(gen_connection).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

all() -> [command_server_binds_to_loopback].

%% gen_connection callbacks: this suite is the callback module.
%% get_port/0 returns a free loopback port nobody listens on, so the
%% "listen" notification POST fails and gen_connection ignores the result.
get_port() -> free_port().
init(_Port) -> ok.
decode_request(_Data) -> #{}.

command_server_binds_to_loopback(_Config) ->
    Before = erlang:ports(),
    ok = gen_connection:start(?MODULE),
    LSock = wait_for_listener(Before, 50),
    %% gen_connection spawns the command server unlinked and exposes no
    %% stop API. Kill the socket owner to close the listen socket: closing
    %% it from here would crash the accept loop with a badmatch instead.
    {connected, Owner} = erlang:port_info(LSock, connected),
    try
        {ok, {Addr, Port}} = inet:sockname(LSock),
        ?assertEqual({127,0,0,1}, Addr),
        {ok, C} = gen_tcp:connect({127,0,0,1}, Port, [], 1000),
        ok = gen_tcp:close(C)
    after
        exit(Owner, kill)
    end.

free_port() ->
    {ok, S} = gen_tcp:listen(0, [{ip, {127,0,0,1}}]),
    {ok, P} = inet:port(S),
    ok = gen_tcp:close(S),
    P.

%% Discovery via erlang:ports/0 assumes the default inet backend
%% (gen_tcp sockets are ports). A listen socket has no peer; an
%% accepted or client socket does.
wait_for_listener(_Before, 0) ->
    ct:fail(command_server_not_listening);
wait_for_listener(Before, N) ->
    New = [P || P <- erlang:ports() -- Before,
                erlang:port_info(P, name) =:= {name, "tcp_inet"},
                inet:peername(P) =:= {error, enotconn}],
    case New of
        [One] -> One;
        _ -> timer:sleep(100), wait_for_listener(Before, N - 1)
    end.
