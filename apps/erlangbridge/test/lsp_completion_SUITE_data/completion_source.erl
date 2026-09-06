-module(completion_source).

-record(item, {identifier, ident_extra}).

-define(GOODS_ID, 3).
-define(MAX_ID, 100).

-export([go/1]).

go(Identifier) ->
    Target = completion_target:alpha(),
    Item = #item{identifier = Identifier},
    ItemField = Item#item.identifier,
    RecordName = #item{},
    MacroRef = ?MAX_ID,
    LocalVar = Identifier,
    Other = completion_target,
    {Target, Item, ItemField, RecordName, MacroRef, LocalVar, Other}.

call_missing_module() ->
    nosuchmodule:go().
