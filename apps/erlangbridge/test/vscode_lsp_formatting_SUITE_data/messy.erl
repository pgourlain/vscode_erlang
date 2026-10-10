-module(messy).
-export([add/2, describe/1]).
add(A,B) ->
  A+B.

describe(Value) ->
  case Value of
      0 -> zero;
      _ -> nonzero
  end.
