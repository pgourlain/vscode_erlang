-module(gen_lsp_sup).

-behavior(gen_supervisor).

-export([start_link/1, start_socket/0]).
-export([init/1]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, once}, {reuseaddr, true}]).

start_link(Port) ->
    io:format("supervisor start_link(~p)", [Port]), 
    case supervisor:start_link(?MODULE, Port) of
        {ok, Pid} -> 
            io:format("supervisor start_link PID(~p)", [Pid]),
            supervisor:start_child(Pid, []),
            %case start_child(Pid) of
            %_Any -> io:format("start_child result(~p)", [_Any])
            %end,
            %spawn(fun() -> observer:start() end),
            {ok, Pid};
        _ ->
            io:format("supervisor start_link failed", []),
            {error, not_started}
    end.

to_int(V) when is_integer(V) ->
    V;
to_int(V) when is_list(V) ->
    erlang:list_to_integer(V).

init(VsCodePort) ->
    {ok, LSock} = gen_tcp:listen(0, ?TCP_OPTIONS),
    {ok, Port} = inet:port(LSock),
    gen_connection:send_message_to_vscode(to_int(VsCodePort), "listen", #{port => Port}),
    spawn_link(fun empty_listeners/0),
    UserSpec = {gen_lsp_server, {gen_lsp_server, start_link, [VsCodePort, LSock]},
                            temporary, infinity, worker, [gen_lsp_server]},
    StartSpecs = {{simple_one_for_one, 60, 3600}, [UserSpec]},
    {ok, StartSpecs}.

start_socket() ->
    R = supervisor:start_child(?MODULE, []),
    io:format("start_socket ~p", [R]),
    R.
 
%% Start with 5 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,2)],
    ok.
