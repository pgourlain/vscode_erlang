%% Common Test suite fixture: a realistic *_SUITE.erl over the
%% fixtures_common project, for suites that need to parse/discover a CT
%% suite shape (all/0, groups/0, testcases) rather than run it (task 6.1).
-module(fc_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("fc_records.hrl").

-export([all/0, groups/0]).
-export([starts_with_given_id/1, describes_an_item/1]).

all() -> [starts_with_given_id, describes_an_item].

groups() -> [].

starts_with_given_id(_Config) ->
    Item = fc_worker:start(42),
    42 = Item#fc_item.id,
    ok.

describes_an_item(_Config) ->
    _ = fc_catalog:describe(1),
    ok.
