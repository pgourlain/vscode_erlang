-module(gen_lsp_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, once}, {reuseaddr, true}]).

start_link(Port) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Port) of
        {ok, Pid} -> 
            supervisor:start_child(Pid, []),
            {ok, Pid};
        _ ->
            error_logger:error_msg("~p:start_link failed",[?MODULE]),
            {error, not_started}
    end.

init(VsCodePort) ->
    {ok, LSock} = gen_tcp:listen(list_to_integer(VsCodePort), ?TCP_OPTIONS),
    UserSpec = #{id => gen_lsp_server,
                 start => {gen_lsp_server, start_link, [list_to_integer(VsCodePort), LSock]},
                 restart => temporary,
                 modules => [gen_lsp_server],
                 type => worker},
    StartSpecs = {{simple_one_for_one, 60, 3600}, [UserSpec]},
    gen_tcp:send(LSock, <<"{}">>),
    {ok, StartSpecs}.
