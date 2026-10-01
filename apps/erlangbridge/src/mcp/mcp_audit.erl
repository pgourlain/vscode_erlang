%% Structured lifecycle/security events of the MCP inspector.
%% Only event names and bounded, non-sensitive fields are logged: never the
%% token, request parameters or returned runtime data.
-module(mcp_audit).

-export([event/2]).

-define(ALLOWED_FIELDS, [tool, status, duration_ms, reason, method, code, port, host_ok, count]).

event(Kind, Fields) when is_atom(Kind), is_map(Fields) ->
    Safe = maps:with(?ALLOWED_FIELDS, Fields),
    try
        case erlang:function_exported(logger, info, 2) of
            true -> logger:info("mcp: ~p ~p", [Kind, Safe]);
            false -> error_logger:info_msg("mcp: ~p ~p~n", [Kind, Safe])
        end
    catch _:_ -> ok
    end,
    ok.
