%% Fixture parse_transform for lsp_syntax_SUITE:parse_transform_from_rebar_config_is_applied.
%% Injects a helper/0 function right before the module's `eof` marker, so
%% consumer.erl's call to the otherwise-undefined helper/0 lints clean only
%% when this transform actually ran.
-module(inject_helper).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    lists:flatmap(fun
        ({eof, Line} = Eof) ->
            [{function, Line, helper, 0, [{clause, Line, [], [], [{atom, Line, ok}]}]}, Eof];
        (Form) ->
            [Form]
    end, Forms).
