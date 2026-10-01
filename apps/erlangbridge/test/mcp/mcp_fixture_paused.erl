%% Test-only module interpreted by the debugger so that a process can be stopped
%% at a breakpoint (see mcp_server_SUITE:paused_process_is_reported_from_debugger_evidence).
-module(mcp_fixture_paused).

-export([run/0, handler/1, break_line/0]).

run() ->
    X = 1,
    Y = X + 1,
    Y.

%% called by the debugger when an interpreted process reaches a breakpoint
handler(_Pid) -> ok.

%% line of `Y = X + 1` in run/0
break_line() -> 9.
