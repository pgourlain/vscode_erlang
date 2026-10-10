-module(rename_target).
-export([greet/1]).

-record(item, {identifier}).

-define(GREETING, "hi").

greet(Name) ->
    Local = local_helper(Name),
    Local.

local_helper(Name) ->
    Name.

use_record(Identifier) ->
    #item{identifier = Identifier}.

use_macro() ->
    ?GREETING.
