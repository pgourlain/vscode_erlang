-module(unused_fn_multiline_export).
-export([
    a/0,
    b/1
]).

a() -> ok.
b(X) -> X.

unused() ->
    ok.
