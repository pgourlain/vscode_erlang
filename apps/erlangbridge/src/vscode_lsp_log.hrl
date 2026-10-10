-define(LOG(S),
	begin
        vscode_gen_lsp_server:vscode_lsp_log("~p", [S])
	end).
-define(LOG(Fmt, Args),
	begin
        vscode_gen_lsp_server:vscode_lsp_log(Fmt, Args)
	end).
%% Topic-tagged variant: Topic is a binary/string filterable via the
%% erlang.verboseExcludeFilter setting (same mechanism used for LSP method
%% names) - add the topic there to silence it while leaving other logging on.
-define(LOG(Topic, Fmt, Args),
	begin
        vscode_gen_lsp_server:vscode_lsp_log(Topic, Fmt, Args)
	end).
