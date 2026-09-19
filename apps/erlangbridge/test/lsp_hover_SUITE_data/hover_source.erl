-module(hover_source).

-record(item, {identifier}).

-define(GOODS_ID, 3).

-type item_id() :: pos_integer().

-export([go/1, call_go/0, reversed/1, use_macro/0, use_record/1, use_type/1]).

go(0) ->
    zero;
go(N) ->
    N.

call_go() ->
    go(1).

reversed(List) ->
    lists:reverse(List).

use_macro() ->
    ?GOODS_ID.

use_record(Identifier) ->
    #item{identifier = Identifier}.

-spec use_type(item_id()) -> item_id().
use_type(Id) ->
    Id.
