%% Test-only logger handler: forwards every log event to the test process.
-module(mcp_capture_logger).

-export([log/2]).

log(Event, #{config := #{owner := Owner}}) ->
    Owner ! {mcp_log, Event},
    ok.
