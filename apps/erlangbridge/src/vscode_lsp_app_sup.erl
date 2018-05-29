-module(vscode_lsp_app_sup).

-export([init/1, start_link/1, start_sup_socket/1, start_sup_doc/0, start_sup_config/0, start_sup_help/0, start_child/1]).

start_link(VsCodePort) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, VsCodePort).

init(VsCodePort) ->
    SocketSpec = socket_spec(VsCodePort),
    DocSpec = doc_spec(),
    ConfigSpec = config_spec(),
    HelpSpec = help_spec(),
    StartSpecs = {{one_for_one, 60, 3600}, [SocketSpec, DocSpec, ConfigSpec, HelpSpec]},
    {ok, StartSpecs}.

socket_spec(VsCodePort) ->
    #{id => gen_lsp_sup,
    start => {gen_lsp_sup, start_link, [VsCodePort]},
    restart => permanent,
    modules => [gen_lsp_sup],
    type => supervisor}.

start_sup_socket(VsCodePort) ->
    supervisor:start_child({local, ?MODULE}, socket_spec(VsCodePort)).

doc_spec() ->
    #{id => gen_lsp_doc_sup,
    start => {gen_lsp_doc_sup, start_link, []},
    restart => permanent,
    modules => [gen_lsp_doc_sup],
    type => supervisor}.
%        {gen_lsp_doc_sup, {gen_lsp_doc_sup, start_link, []},
%                        permanent, infinity, supervisor, [gen_lsp_doc_sup]}.

config_spec() ->
    #{id => gen_lsp_config_sup,
    start => {gen_lsp_config_sup, start_link, []},
    restart => permanent,
    modules => [gen_lsp_config_sup],
    type => supervisor}.

help_spec() ->
    #{id => gen_lsp_help_sup,
    start => {gen_lsp_help_sup, start_link, []},
    restart => permanent,
    modules => [gen_lsp_help_sup],
    type => supervisor}.

start_sup_doc() ->
    supervisor:start_child(?MODULE, doc_spec()).

start_sup_config() ->
    supervisor:start_child(?MODULE, config_spec()).

start_sup_help() ->
    supervisor:start_child(?MODULE, help_spec()).

start_child(Arg) ->
    error_logger:error_msg([{vscode_lsp_app_sup, start_child}, {arg, Arg}]),
    error.
