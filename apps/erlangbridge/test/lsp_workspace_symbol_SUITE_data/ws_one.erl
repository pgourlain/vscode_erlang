-module(ws_one).
-export([helper_one/0]).

-define(WS_MACRO, 1).

-record(ws_rec, {field_a, field_b}).

-type ws_type() :: integer().

helper_one() -> ok.
