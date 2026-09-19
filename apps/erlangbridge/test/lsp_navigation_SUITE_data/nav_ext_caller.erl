-module(nav_ext_caller).
-include("nav_ext_include.hrl").
-export([call_it/0]).

call_it() ->
    nav_ext_target:greet("world").

%% see https://www.erlang.org/doc for more.
