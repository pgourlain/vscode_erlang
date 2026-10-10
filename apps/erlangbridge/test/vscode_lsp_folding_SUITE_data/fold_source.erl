-module(fold_source).
%% region helpers
-export([f/1,
         g/0]).
%% endregion

%% This is a doc comment
%% spanning two lines.
f(X) ->
    case X of
        0 -> zero;
        _ -> nonzero
    end.

g() -> ok.

h(0) ->
    zero;
h(_) ->
    nonzero.
