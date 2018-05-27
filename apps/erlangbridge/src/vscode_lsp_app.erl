-module(vscode_lsp_app).

-behavior(application).

-export([start/2, stop/1]).


get_port() ->
    case init:get_argument(vscode_port) of
    {ok, [[P]]} -> P;
    _ -> 0
    end.
    

start(_Type, _Args) ->
    application:start(inets),
    %uncomment to monitor erlang processes
    %spawn(fun() -> observer:start() end),
    Port = get_port(),
    case vscode_lsp_app_sup:start_link(Port) of
    {ok, Pid} ->
        {ok, Pid};
    _Any -> _Any        
    end.

stop(_State) ->
    ok.