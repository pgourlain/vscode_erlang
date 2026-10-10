-module(hier_gs).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, 0}.

handle_call(get, _From, State) ->
    Reply = helper(State),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

helper(X) ->
    Y = X + 1,
    Y * 2.
