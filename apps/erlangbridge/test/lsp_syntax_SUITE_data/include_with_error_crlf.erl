-module(include_with_error_crlf).

-include("broken_include.hrl").
-export([go/0]).

go() -> ok.
