%% Test-only worker of the MCP fixture; holds forbidden sentinel data everywhere
%% the inspector must never look (state, dictionary, ETS objects, mailbox).
-module(mcp_fixture_worker).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Arg) -> start_link(undefined, Arg).

start_link(undefined, Arg) ->
    gen_server:start_link(?MODULE, Arg, []);
start_link(Name, Arg) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Arg}, []).

init({mcp_fx_worker_a, _}) ->
    put(secret, "SENTINEL_PDICT"),
    %% a linked but unsupervised process (registered)
    Orphan = spawn_link(fun() ->
                                register(mcp_fx_orphan, self()),
                                receive stop -> ok end
                        end),
    {ok, #{secret => "SENTINEL_STATE", orphan => Orphan}};
init({mcp_fx_worker_c, _}) ->
    put(secret, "SENTINEL_PDICT"),
    catch ets:delete(mcp_fx_orders),
    ets:new(mcp_fx_orders, [named_table, public, set]),
    ets:insert(mcp_fx_orders, {"SENTINEL_ETS_KEY", "SENTINEL_ETS_VALUE"}),
    ets:new(mcp_fx_private, [named_table, private, set]),
    ets:insert(mcp_fx_private, {"SENTINEL_PRIVATE_KEY", "SENTINEL_PRIVATE_VALUE"}),
    {ok, #{secret => "SENTINEL_STATE"}};
init(_) ->
    put(secret, "SENTINEL_PDICT"),
    {ok, #{secret => "SENTINEL_STATE"}}.

handle_call(_, _, S) -> {reply, ok, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
