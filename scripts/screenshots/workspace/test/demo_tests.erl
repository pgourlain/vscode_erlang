-module(demo_tests).
-include_lib("eunit/include/eunit.hrl").

area_square_test() ->
    ?assertEqual(100.0, demo:area({shape, square, 10})).

area_circle_test() ->
    ?assert(demo:area({shape, circle, 1}) > 3.14).

total_area_empty_test() ->
    ?assertEqual(1.0, demo:total_area([])).
