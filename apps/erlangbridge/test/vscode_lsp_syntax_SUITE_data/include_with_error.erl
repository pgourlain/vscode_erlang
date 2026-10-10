-module(include_with_error).
-include("broken_include.hrl").
-export([go/0]).

go() -> ok.
