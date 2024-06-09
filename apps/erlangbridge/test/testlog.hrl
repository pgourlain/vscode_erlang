-define(logMsg(S),
        begin
            ct:log(default, 50, "~w:~p", [self(), S], [])
        end).

-define(writeConsole(Fmt, Args),
        error_logger:info_msg(Msg, Args)).

-define(writeConsole(S),
        error_logger:info_msg("~p\n", [S])).