-module(missing_callbacks).
-behaviour(gen_server).
-export([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
