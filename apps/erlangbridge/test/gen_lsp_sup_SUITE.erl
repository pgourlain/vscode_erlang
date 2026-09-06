-module(gen_lsp_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

all() -> [lsp_listener_binds_to_loopback].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    ErlangSection = #{verbose => false},
    gen_lsp_config_server:update_config(erlang, ErlangSection),
    Config.

end_per_suite(_Config) ->
    application:stop(vscode_lsp),
    ok.

%% gen_lsp_sup:init/1 opens the listen socket and hands it to its
%% gen_lsp_server child through the child spec start arguments.
lsp_listener_binds_to_loopback(_Config) ->
    [{_, Child, _, _}] = supervisor:which_children(gen_lsp_sup),
    {ok, #{start := {gen_lsp_server, start_link, [_Port, LSock]}}} =
        supervisor:get_childspec(gen_lsp_sup, Child),
    ?assertMatch({ok, {{127,0,0,1}, _}}, inet:sockname(LSock)).
