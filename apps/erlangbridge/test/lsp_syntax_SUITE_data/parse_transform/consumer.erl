-module(consumer).
-export([go/0]).

%% helper/0 is not defined anywhere in this module - it only exists once
%% inject_helper:parse_transform/2 (declared in rebar.config's erl_opts,
%% not via -compile(...) here) injects it. Without that transform running,
%% erl_lint reports "function helper/0 undefined".
go() -> helper().
