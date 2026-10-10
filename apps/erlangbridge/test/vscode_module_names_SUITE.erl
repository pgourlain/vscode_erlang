-module(vscode_module_names_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

%% Modules loaded into the user's VM (debugger bridge, MCP inspector, test
%% hooks) share its flat module namespace (issue #362). Every bridge module
%% must therefore carry an extension-specific prefix. Only the language server
%% modules, which run in their own VM, keep their lsp_/gen_lsp_ names.

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
allowed("lsp_" ++ _) -> true;
allowed("gen_lsp_" ++ _) -> true;
allowed("hover_doc_layout") -> true;
allowed(_) -> false.
