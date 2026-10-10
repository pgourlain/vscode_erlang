-module(vscode_module_names_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

%% Modules loaded into the user's VM (debugger bridge, MCP inspector, test
%% hooks) share its flat module namespace (issue #362). Every bridge module
%% must therefore carry the extension-specific `vscode_` prefix. The language
%% server VM is not exempt: it loads and runs user modules too (lsp_testing,
%% lsp_syntax).

all() -> [every_bridge_module_is_prefixed].

every_bridge_module_is_prefixed(_Config) ->
    SrcDir = filename:join([code:lib_dir(vscode_lsp), "..", "..", "..", "..", "apps", "erlangbridge", "src"]),
    Files = filelib:wildcard(filename:join(SrcDir, "**/*.erl")),
    true = Files =/= [],
    Bad = [Name || F <- Files,
                   Name <- [filename:basename(F, ".erl")],
                   not allowed(Name)],
    [] = Bad.

allowed("vscode_" ++ _) -> true;
allowed(_) -> false.
