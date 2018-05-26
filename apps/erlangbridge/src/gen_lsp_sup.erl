-module(gen_lsp_sup).

-export([start_link/1]).
-export([init/1]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, once}, {reuseaddr, true}]).

start_link(Port) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Port) of
        {ok, Pid} -> 
            error_logger:info_report([{?MODULE, start_link}, {pid, Pid}]),            
            C = supervisor:start_child(Pid, []),
            error_logger:info_report([{?MODULE, supervisor_startchild}, {result, C}]),            
            {ok, Pid};
        _ ->
            error_logger:error_msg("~p:start_link failed",[?MODULE]),
            {error, not_started}
    end.

to_int(V) when is_integer(V) ->
    V;
to_int(V) when is_list(V) ->
    erlang:list_to_integer(V).

init(VsCodePort) ->
    {ok, LSock} = gen_tcp:listen(list_to_integer(VsCodePort), ?TCP_OPTIONS),
    %gen_connection:send_message_to_vscode(to_int(VsCodePort), "listen", #{port => list_to_integer(VsCodePort)}),
    UserSpec = {gen_lsp_server, {gen_lsp_server, start_link, [list_to_integer(VsCodePort), LSock]},
                            temporary, infinity, worker, [gen_lsp_server]},
    StartSpecs = {{simple_one_for_one, 60, 3600}, [UserSpec]},
    gen_tcp:send(LSock, <<"{}">>),    
    error_logger:info_msg("init ~p",[LSock]),
    {ok, StartSpecs}.
