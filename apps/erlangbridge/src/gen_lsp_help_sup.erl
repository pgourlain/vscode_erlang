-module(gen_lsp_help_sup).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    UserSpec = #{id => gen_lsp_help_server,
        start => {gen_lsp_help_server, start_link, []},
        restart => permanent,
        modules => [gen_lsp_help_server],
        type => worker},
    StartSpecs = {{one_for_one, 60, 3600}, [UserSpec]},
    {ok, StartSpecs}.
