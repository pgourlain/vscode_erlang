-module(gen_lsp_doc_sup).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    % error_logger:info_msg("~p:init()", [?MODULE]),
    % because lsp_log use another gen_server we can't use it
    % gen_lsp_server:lsp_log("~p:init()", [?MODULE]),
    UserSpec = #{id => gen_lsp_doc_server,
        start => {gen_lsp_doc_server, start_link, []},
        restart => permanent,
        modules => [gen_lsp_doc_server],
        type => worker},
    StartSpecs = {{one_for_one, 60, 3600}, [UserSpec]},
    {ok, StartSpecs}.
