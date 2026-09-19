%% EUnit module fixture: *_test/0 functions over the fixtures_common project,
%% for suites that need a realistic eunit source file (report/discovery, task 6.1).
-module(fc_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fc_records.hrl").

start_sets_given_id_test() ->
    Item = fc_worker:start(7),
    ?assertEqual(7, Item#fc_item.id).

total_price_sums_all_items_test() ->
    Items = [
        #fc_item{id = 1, name = <<"a">>, price = 10},
        #fc_item{id = 2, name = <<"b">>, price = 5}
    ],
    ?assertEqual(15, fc_catalog:total_price(Items)).
