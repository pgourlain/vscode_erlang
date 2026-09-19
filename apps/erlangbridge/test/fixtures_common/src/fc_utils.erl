-module(fc_utils).

-include("fc_records.hrl").

-export([format_item/1]).

-spec format_item(#fc_item{}) -> iolist().
format_item(#fc_item{id = Id, name = Name, price = Price}) ->
    io_lib:format("#~p ~s (~p)", [Id, Name, Price]).
