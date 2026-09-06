%% Implements fc_behaviour; uses the shared #fc_item{} record and macros
%% from fc_records.hrl, plus fc_types for -spec argument/return types.
-module(fc_worker).
-behaviour(fc_behaviour).

-include("fc_records.hrl").

-export([start/1, init/1, handle_item/2]).

-spec init(term()) -> {ok, #fc_item{}}.
init(_Args) ->
    {ok, #fc_item{id = 1, name = <<"seed">>, price = ?FC_DEFAULT_PRICE}}.

-spec handle_item(#fc_item{}, term()) -> fc_types:fc_result().
handle_item(#fc_item{price = Price}, _State) ->
    {ok, Price}.

-spec start(fc_types:fc_id()) -> #fc_item{}.
start(Id) ->
    {ok, Item} = init([]),
    Item#fc_item{id = Id}.
