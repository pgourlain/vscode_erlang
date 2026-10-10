-module(nav_ext_target).
-export([greet/1, use_record/1, use_macro/0, use_type/1]).

-record(item, {identifier}).

-define(GREETING, "hi").

-type item_id() :: pos_integer().

-callback handle(term()) -> term().

greet(Name) ->
    Name.

use_record(Identifier) ->
    #item{identifier = Identifier}.

use_macro() ->
    ?GREETING.

-spec use_type(item_id()) -> item_id().
use_type(Id) ->
    Id.
