-module(lsp_log).
-export([setup/0, info/3, warning/3, error/3]).

% use logger when OTP >= 21

-ifdef(OTP_RELEASE).
setup() ->
    %% handler dédié aux logs LSP : format épuré
    logger:add_handler(lsp_h, logger_std_h, #{
        level => info,
        filters => [{only_lsp, {fun logger_filters:domain/2,
                                {log, sub, [lsp]}}}],
        filter_default => stop,
        formatter => {logger_formatter,
                      #{single_line => false, template => [{method, ["[", method, "] "], []},
                                     msg, "\n"]}}
    }),
    %% le handler default garde time/level, mais ignore le domaine lsp
    logger:add_handler_filter(default, no_lsp,
        {fun logger_filters:domain/2, {stop, sub, [lsp]}}).

info(Method, Msg, Args) ->
    logger:notice(Msg, Args, #{domain => [lsp], method => Method}).

warning(Method, Msg, Args) ->
    logger:warning(Msg, Args, #{domain => [lsp], method => Method}).

error(Method, Msg, Args) ->
    logger:error(Msg, Args, #{domain => [lsp], method => Method}).

-else.
setup() ->
    ok.

info(Method, Msg, Args) ->
    error_logger:info_msg("[~ts] ~ts", [Method, io_lib:format(Msg, Args)]).

warning(Method, Msg, Args) ->
    error_logger:warning_msg("[~ts] ~ts", [Method, io_lib:format(Msg, Args)]).

error(Method, Msg, Args) ->
    error_logger:error_msg("[~ts] ~ts", [Method, io_lib:format(Msg, Args)]).

-endif.
