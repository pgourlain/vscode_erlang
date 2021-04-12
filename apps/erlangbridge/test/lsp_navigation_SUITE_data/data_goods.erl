-module(data_goods).
-export([get/1]).

get(1) -> 1001;

get(2) -> 1002;

get(3) -> 1003;

get(4) -> 1004;

get(_) -> 0.