-module(implicit_fun_expression).

-define(F, f).

f() ->
    ok.

g(M, F) ->
    fun f/0,
    fun F/0,
    fun ?F/0,
    fun m:f/0,
    fun m:F/0,
    fun m:?F/0,
    fun M:f/0,
    fun ?MODULE:f/0,
    ok.

-spec h() -> ok.
h() ->
    g(m, f),
    ok.
