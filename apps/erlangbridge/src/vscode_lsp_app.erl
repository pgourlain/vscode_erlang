%%%-------------------------------------------------------------------
%%% @doc Erlang Language Server Process (LSP) for Visual Studio Code.
%%%
%%% Application supervision tree:
%%%
%%% ```
%%% vscode_lsp_entry
%%%  |
%%%  | vscode_lsp [app.src]
%%%  |
%%% vscode_lsp_app [app]
%%%  |
%%% vscode_lsp_app_sup [sup]
%%%  |
%%%  +--gen_lsp_sup [sup]
%%%  |   |
%%%  |   +--gen_lsp_server [gen_server]
%%%  |
%%%  +--gen_lsp_doc_sup [sup]
%%%  |   |  - (D)ETS document_contents
%%%  |   |  - (D)ETS document_inlayhints
%%%  |   |  - (D)ETS dodged_syntax_tree
%%%  |   |  - ETS references
%%%  |   |  - (D)ETS syntax_tree
%%%  |   |
%%%  |   +--gen_lsp_doc_server [gen_server]
%%%  |
%%%  +--gen_lsp_config_sup [sup]
%%%  |   |
%%%  |   +--gen_lsp_config_server [gen_server]
%%%  |
%%%  +--gen_lsp_help_sup [sup]
%%%      |
%%%      +--gen_lsp_help_server [gen_server]
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(vscode_lsp_app).
-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

get_port() ->
    case init:get_argument(vscode_port) of
        {ok, [[P]]} -> P;
        _ -> "0"
    end.

start(_Type, _Args) ->
    application:start(inets),
    %uncomment to monitor erlang processes
    %spawn(fun() -> observer:start() end),
    Port = get_port(),
    case vscode_lsp_app_sup:start_link(Port) of
        {ok, Pid} -> {ok, Pid};
        _Any      -> _Any
    end.

stop(_State) ->
    ok.
