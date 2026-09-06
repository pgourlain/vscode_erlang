%% Shared record + macro definitions for the fixtures_common project.
%% Pulled in via -include("fc_records.hrl") by every src/ module below,
%% so suites can exercise "go to definition" / "find references" across
%% an -include boundary instead of a single flat file.

-record(fc_item, {
    id :: pos_integer(),
    name :: binary(),
    price = 0 :: number()
}).

-define(FC_DEFAULT_PRICE, 0).
-define(FC_MAX_ITEMS, 100).
