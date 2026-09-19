-module(demo_SUITE).
-export([all/0, area_case/1, total_case/1]).

all() -> [area_case, total_case].

area_case(_Config) ->
    4.0 = demo:area({shape, square, 2}),
    ok.

total_case(_Config) ->
    8.0 = demo:total_area([{shape, square, 2}, {shape, square, 2}]),
    ok.
