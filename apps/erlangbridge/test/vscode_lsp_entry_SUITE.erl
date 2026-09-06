-module(vscode_lsp_entry_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Guards vscode_lsp_entry:compile_needed_modules/0's hardcoded module list
%% (apps/erlangbridge/src/vscode_lsp_entry.erl:28-35) against the real
%% src/ directory: every src/lsp_*.erl and src/gen_lsp_*.erl file must be
%% in that list, or the dev-time hot-load path (used when the extension
%% recompiles the bridge in memory) runs it stale or missing while `rebar3
%% compile`/`rebar3 ct`, which compile the whole src/ directory regardless
%% of this list, hide the gap completely.
%%
%% EXPECTED TO FAIL right now: lsp_fun_utils and lsp_signature_doc_layout
%% both exist in src/ but are missing from the list. That failure is the
%% point of this suite (task 0.14) - it is fixed in task 1.0, which simply
%% adds the two missing entries. Until 1.0 lands, `./rebar3 ct` for this
%% suite - and therefore for the whole project - is expected to report
%% this one known failure.

all() -> [every_lsp_module_is_in_compile_needed_modules].

every_lsp_module_is_in_compile_needed_modules(_Config) ->
    SrcDir = filename:join([code:lib_dir(vscode_lsp), "..", "..", "..", "..", "apps", "erlangbridge", "src"]),
    SourceModules = lists:usort(
        [module_name(F) || F <- filelib:wildcard(filename:join(SrcDir, "lsp_*.erl"))] ++
        [module_name(F) || F <- filelib:wildcard(filename:join(SrcDir, "gen_lsp_*.erl"))]
    ),
    ?assert(length(SourceModules) > 0),
    CompiledModules = lists:usort(compile_needed_modules_list()),
    Missing = SourceModules -- CompiledModules,
    ?assertEqual([], Missing).

module_name(File) ->
    filename:basename(File, ".erl").

%% Extracts the literal list of paths passed to do_compile/2 inside
%% vscode_lsp_entry:compile_needed_modules/0, from the already-compiled
%% module's abstract code - without ever calling the function itself
%% (which has real compile-and-hot-load side effects and assumes a cwd
%% this suite does not run from).
compile_needed_modules_list() ->
    BeamFile = code:which(vscode_lsp_entry),
    {ok, {vscode_lsp_entry, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(BeamFile, [abstract_code]),
    {ok, ListArg} = find_do_compile_list_arg(Forms),
    [filename:basename(Path) || Path <- erl_parse:normalise(ListArg)].

find_do_compile_list_arg({call, _, {atom, _, do_compile}, [ListArg, _OptsArg]}) ->
    {ok, ListArg};
find_do_compile_list_arg(Term) when is_tuple(Term) ->
    find_do_compile_list_arg(tuple_to_list(Term));
find_do_compile_list_arg([H | T]) ->
    case find_do_compile_list_arg(H) of
        {ok, _} = Found -> Found;
        not_found -> find_do_compile_list_arg(T)
    end;
find_do_compile_list_arg(_) ->
    not_found.
