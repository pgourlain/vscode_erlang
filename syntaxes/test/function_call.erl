-module(function_call).

-define(F, f).

f() ->
    ok.

g(M, F) ->
    m:f(),
    m:F(),
    m:?F(),
    M:f(),
    ?MODULE:f(),
    ok.

-spec h() -> ok.
h() ->
    g(m, f),
    ok.
