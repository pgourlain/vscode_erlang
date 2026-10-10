-module(symbols_source).

-record(item, {identifier}).

-type item_id() :: pos_integer().

-export([exported_used/0, exported_unused/0]).

call_the_exported() ->
    exported_used().

exported_used() ->
    helper().

exported_unused() ->
    ok.

helper() ->
    ok.

unused_private() ->
    ok.
