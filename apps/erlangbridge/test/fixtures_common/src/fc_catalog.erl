%% Cross-module calls (fc_worker, fc_utils) plus a deliberately mixed set of
%% call sites for codeLens/inlay-hint style suites:
%%  - total_price/1  : exported, called from fc_worker_tests -> "N references"
%%  - describe/1     : exported, called from fc_worker_SUITE -> "N references"
%%  - clamp/1        : private, called once from total_price/1
%%  - unused_helper/1: private, never called -> "unused"
-module(fc_catalog).

-include("fc_records.hrl").

-export([total_price/1, describe/1]).

-spec total_price([#fc_item{}]) -> number().
total_price(Items) when length(Items) =< ?FC_MAX_ITEMS ->
    clamp(lists:sum([Item#fc_item.price || Item <- Items])).

-spec describe(fc_types:fc_id()) -> iolist().
describe(Id) ->
    Item = fc_worker:start(Id),
    fc_utils:format_item(Item).

clamp(Price) when Price < 0 -> 0;
clamp(Price) -> Price.

unused_helper(_Item) ->
    ok.
