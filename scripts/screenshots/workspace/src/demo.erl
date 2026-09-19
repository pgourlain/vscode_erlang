-module(demo).
-export([start/1, area/1, total_area/1]).
-export([old_api/0]).
-deprecated([{old_api, 0}]).

-define(DEFAULT_SIDE, 10).
-record(shape, {kind :: circle | square, size = ?DEFAULT_SIDE :: number()}).
-type shape() :: #shape{}.

%% region Public API
-spec start(list()) -> ok.
start(Args) ->
    Shapes = [#shape{kind = circle, size = 2}, #shape{kind = square}],
    Unused = length(Args),
    io:format("total: ~p~n", [total_area(Shapes)]),
    ok.

-spec area(shape()) -> float().
area(#shape{kind = circle, size = R}) -> math:pi() * R * R;
area(#shape{kind = square, size = S}) -> float(S * S).

total_area(Shapes) ->
    lists:sum([area(S) || S <- Shapes]).
%% endregion

old_api() -> start([]).

helper(X) when X > 0 -> X * 2.
