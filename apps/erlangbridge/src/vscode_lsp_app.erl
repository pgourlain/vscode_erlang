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
    gen_lsp_sup:start_link(get_port()).

stop(_State) ->
    ok.