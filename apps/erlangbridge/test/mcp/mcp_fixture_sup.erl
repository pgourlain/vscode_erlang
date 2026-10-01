%% Test-only OTP fixture for the MCP topology inspector. Never part of production.
%%
%% Expected topology (independent of inspector output):
%%
%%   mcp_fx_root (supervisor, app root)
%%     |- worker_a          registered mcp_fx_worker_a, links to an unsupervised orphan
%%     |- mcp_fx_sub_sup    supervisor
%%     |    |- worker_b     unregistered
%%     |    `- worker_c     registered mcp_fx_worker_c, owns ETS mcp_fx_orders (public)
%%     |                    and mcp_fx_private (private)
%%     `- mcp_fx_dyn_sup    supervisor (simple_one_for_one), N dynamic workers
%%
%% Forbidden data: every SENTINEL_* string below must never appear in a result.
-module(mcp_fixture_sup).
-behaviour(supervisor).
-behaviour(application).

-export([start/2, stop/1, init/1]).
-export([start_fixture/0, stop_fixture/0, add_dynamic/1, sentinel/0]).

sentinel() -> "SENTINEL".

start(_Type, _Args) ->
    supervisor:start_link({local, mcp_fx_root}, ?MODULE, root).

stop(_State) -> ok.

%% Load and start the fixture as a real OTP application.
start_fixture() ->
    App = {application, mcp_fixture_app,
           [{description, "mcp fixture"}, {vsn, "9.9.9"},
            {modules, [mcp_fixture_sup, mcp_fixture_worker]},
            {registered, [mcp_fx_root]},
            {applications, [kernel, stdlib]},
            {env, [{secret, "SENTINEL_ENV"}]},
            {mod, {mcp_fixture_sup, []}}]},
    ok = application:load(App),
    ok = application:start(mcp_fixture_app, temporary),
    ok.

stop_fixture() ->
    catch application:stop(mcp_fixture_app),
    catch application:unload(mcp_fixture_app),
    ok.

add_dynamic(N) ->
    [{ok, _} = supervisor:start_child(mcp_fx_dyn_sup, [{dynamic_worker, I}]) || I <- lists:seq(1, N)],
    ok.

init(root) ->
    Children = [#{id => worker_a, start => {mcp_fixture_worker, start_link, [mcp_fx_worker_a, {"SENTINEL_START_ARG", orphan}]},
                  restart => permanent, shutdown => 1000, type => worker, modules => [mcp_fixture_worker]},
                #{id => mcp_fx_sub_sup, start => {supervisor, start_link, [{local, mcp_fx_sub_sup}, ?MODULE, sub]},
                  restart => permanent, shutdown => infinity, type => supervisor, modules => [?MODULE]},
                #{id => mcp_fx_dyn_sup, start => {supervisor, start_link, [{local, mcp_fx_dyn_sup}, ?MODULE, dyn]},
                  restart => permanent, shutdown => infinity, type => supervisor, modules => [?MODULE]}],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 5}, Children}};
init(sub) ->
    Children = [#{id => worker_b, start => {mcp_fixture_worker, start_link, [undefined, {"SENTINEL_START_ARG", plain}]},
                  restart => transient, shutdown => 2000, type => worker, modules => [mcp_fixture_worker]},
                #{id => worker_c, start => {mcp_fixture_worker, start_link, [mcp_fx_worker_c, {"SENTINEL_START_ARG", tables}]},
                  restart => permanent, shutdown => 1000, type => worker, modules => [mcp_fixture_worker]}],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 5}, Children}};
init(dyn) ->
    Child = #{id => dyn, start => {mcp_fixture_worker, start_link, [undefined]},
              restart => temporary, shutdown => 1000, type => worker, modules => [mcp_fixture_worker]},
    {ok, {#{strategy => simple_one_for_one, intensity => 10, period => 5}, [Child]}}.
