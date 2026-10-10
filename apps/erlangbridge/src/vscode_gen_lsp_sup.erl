-module(vscode_gen_lsp_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, once}, {reuseaddr, true}, {ip, {127,0,0,1}}]).

start_link(Port) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Port) of
        {ok, Pid} -> 
            supervisor:start_child(Pid, []),
            {ok, Pid};
        _ ->
            vscode_lsp_log:error(<<"LSP">>, "~p:start_link failed",[?MODULE]),
            {error, not_started}
    end.

init(VsCodePort) ->
    {ok, LSock} = gen_tcp:listen(list_to_integer(VsCodePort), ?TCP_OPTIONS),
    UserSpec = #{id => vscode_gen_lsp_server,
                 start => {vscode_gen_lsp_server, start_link, [list_to_integer(VsCodePort), LSock]},
                 restart => temporary,
                 modules => [vscode_gen_lsp_server],
                 type => worker},
    StartSpecs = {{simple_one_for_one, 60, 3600}, [UserSpec]},
    gen_tcp:send(LSock, <<"{}">>),
    {ok, StartSpecs}.
