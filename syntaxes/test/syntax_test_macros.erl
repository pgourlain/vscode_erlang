-module(syntax_test_macros).

-compile(export_all).

-define(ONE, 1).
-define(INC(N), (N+1)).
-define(ADD(N, M), (N+M)).
-define(NAME_VALUE(Param), [??Param, Param]).

-spec f() -> ok.
f() ->
    ?ONE,
    ?INC(1),
    ?ADD(1, 2),
    ?ADD(   % parameters
         1, % 1
         2  % 2
        ),  % end
    X = 1,
    ?NAME_VALUE(X),
    ok.

-spec g() -> ok.
g() ->
    ok.
