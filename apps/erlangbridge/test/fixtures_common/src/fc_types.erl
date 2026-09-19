%% Standalone -type/-opaque definitions, referenced from other modules'
%% -spec attributes (fc_worker, fc_catalog) so navigation/hover suites have
%% a real cross-module type usage to resolve.
-module(fc_types).

-export_type([fc_id/0, fc_result/0]).

-type fc_id() :: pos_integer().
-type fc_result() :: {ok, term()} | {error, term()}.
