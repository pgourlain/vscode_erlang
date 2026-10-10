-module(nav_ext_impl).
-behaviour(nav_ext_target).
-export([handle/1]).

handle(Msg) ->
    Msg.
