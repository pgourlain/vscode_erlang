-module(include_with_error_comment).

-include("broken_include.hrl"). % trailing comment
-export([go/0]).

go() -> ok.
