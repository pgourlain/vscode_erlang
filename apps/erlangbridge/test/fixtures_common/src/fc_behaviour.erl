%% Behaviour with a single implementor (fc_worker), for suites covering
%% "implement missing callbacks" (task 2.4) and behaviour <-> implementor
%% navigation (task 4.4).
-module(fc_behaviour).

-callback init(Args :: term()) -> {ok, term()}.
-callback handle_item(Item :: term(), State :: term()) -> fc_types:fc_result().
