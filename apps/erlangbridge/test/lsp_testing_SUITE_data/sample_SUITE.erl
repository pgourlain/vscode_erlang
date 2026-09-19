-module(sample_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([ok_case/1, failing_case/1]).

all() -> [ok_case, failing_case].

init_per_suite(Config) -> Config.

end_per_suite(Config) -> Config.

ok_case(_Config) ->
    ok.

failing_case(_Config) ->
    ct:fail("boom").
