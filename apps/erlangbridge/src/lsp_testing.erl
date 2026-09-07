%% Task 6.1/6.3/6.6: `erlang/discoverTests` and `erlang/runTests` custom LSP
%% requests - Erlang-driven Test Explorer support (Phase 6, tasks.md).
%% `run_tests/2` also drives `cover`-based coverage collection when its
%% Params carry `coverage => true` (task 6.6).
%%
%% Discovery walks every project file (reusing gen_lsp_doc_server's project
%% scan) and classifies it as either a Common Test suite (`*_SUITE.erl`) or
%% a plain module that may contain EUnit tests. EUnit tests are looked up in
%% the *dodged* syntax tree (gen_lsp_doc_server:get_dodged_syntax_tree/1) so
%% `-ifdef(TEST).` guarded test functions are found too - the normal,
%% preprocessed tree silently drops them when TEST isn't defined for a
%% normal build.
-module(lsp_testing).

-export([discover_tests/2, run_tests/2]).

-import(lsp_syntax, [fold_in_syntax_tree/4]).

-include("lsp_log.hrl").

%% CT callbacks that show up as exported 0-arity functions but are not
%% themselves test cases.
-define(CT_NON_TESTCASE_CALLBACKS, [
    all, groups, suite,
    init_per_suite, end_per_suite,
    init_per_group, end_per_group,
    init_per_testcase, end_per_testcase
]).

%% ============================================================================
%% erlang/discoverTests
%% ============================================================================

discover_tests(_Socket, _Params) ->
    #{modules => discover_modules()}.

discover_modules() ->
    lists:filtermap(fun scan_module/1, gen_lsp_doc_server:all_project_files()).

scan_module(File) ->
    case filename:extension(File) of
        ".erl" ->
            BaseName = filename:basename(File, ".erl"),
            {Kind, Tests} = case is_ct_suite(BaseName) of
                true -> {<<"ct">>, gather_ct_tests(File)};
                false -> {<<"eunit">>, gather_eunit_tests(File)}
            end,
            case Tests of
                [] -> false;
                _ ->
                    {true, #{
                        module => lsp_utils:to_binary(BaseName),
                        kind => Kind,
                        uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
                        tests => [test_to_map(Name, Range) || {Name, Range} <- Tests]
                    }}
            end;
        _ ->
            false
    end.

test_to_map(Name, {L, C, L1, C1}) ->
    #{
        name => lsp_utils:to_binary(Name),
        arity => 0,
        range => lsp_utils:client_range(L, C, L1, C1)
    }.

is_ct_suite(BaseName) ->
    lists:suffix("_SUITE", BaseName).

%% EUnit convention: 0-arity functions named `..._test` (simple assertion)
%% or `..._test_` (generator). Scanning the dodged tree means these are
%% found even when wrapped in `-ifdef(TEST). ... -endif.`.
%%
%% Every form in the dodged tree - even plain functions with no macro use
%% at all, as the `-module` attribute above them shows - comes back as a
%% generic `erl_syntax` tree ({tree, function, Attr, {func, ...}}), not the
%% raw {function, Anno, Name, Arity, Clauses} tuple `lsp_fun_utils` and the
%% rest of the codebase match on for the *normal*, preprocessed tree. So
%% this walks it through `erl_syntax`'s type-agnostic accessors instead,
%% and only ever needs the function's start position (a point range, like
%% the `record`/`type` entries in lsp_navigation:symbol_info/1) - a real
%% end-of-function range would need reverting each clause individually,
%% which the macro calls inside a typical EUnit test body make lossy.
gather_eunit_tests(File) ->
    DodgedTree = gen_lsp_doc_server:get_dodged_syntax_tree(File),
    Tests = fold_in_syntax_tree(fun
        (Node, CurrentFile, Acc) when CurrentFile =:= File ->
            case erl_syntax:type(Node) of
                function ->
                    case erl_syntax:function_arity(Node) of
                        0 ->
                            FnName = erl_syntax:atom_value(erl_syntax:function_name(Node)),
                            case is_eunit_test_name(FnName) of
                                true ->
                                    {L, C} = erl_syntax:get_pos(Node),
                                    [{FnName, {L, C, L, C}} | Acc];
                                false -> Acc
                            end;
                        _ -> Acc
                    end;
                _ -> Acc
            end;
        (_SyntaxTree, _CurrentFile, Acc) ->
            Acc
    end, [], File, DodgedTree),
    lists:reverse(Tests).

is_eunit_test_name(FnName) ->
    Str = atom_to_list(FnName),
    lists:suffix("_test", Str) orelse lists:suffix("_test_", Str).

%% CT convention: every test case is a 1-arity function (taking `Config`),
%% and `all/0` (plus `groups/0`) usually build the test case list
%% dynamically - so rather than evaluating it, every exported 1-arity
%% function that isn't a standard CT callback (`init_per_suite/1` and
%% `end_per_suite/1` are also 1-arity) is treated as a test case.
gather_ct_tests(File) ->
    {Exports, Funcs} = fold_in_syntax_tree(fun
        ({attribute, {_, _}, export, ExportList}, CurrentFile, {Ex, Fs}) when CurrentFile =:= File ->
            {ExportList ++ Ex, Fs};
        ({function, {_, _}, FnName, 1, _} = F, CurrentFile, {Ex, Fs}) when CurrentFile =:= File ->
            {Ex, [{FnName, F} | Fs]};
        (_SyntaxTree, _CurrentFile, Acc) ->
            Acc
    end, {[], []}, File, gen_lsp_doc_server:get_syntax_tree(File)),
    lists:reverse([
        {FnName, lsp_fun_utils:get_function_range(F)}
        || {FnName, F} <- Funcs,
           lists:member({FnName, 1}, Exports),
           not lists:member(FnName, ?CT_NON_TESTCASE_CALLBACKS)
    ]).

%% ============================================================================
%% erlang/runTests
%% ============================================================================

run_tests(Socket, Params) ->
    Requested = maps:get(tests, Params, []),
    Coverage = maps:get(coverage, Params, false),
    Targets = resolve_targets(Requested),
    {EunitTargets, CtTargets} = lists:partition(fun (#{kind := Kind}) -> Kind =:= eunit end, Targets),
    %% `public`, not `private`/`protected`: eunit's listener and CT's hook
    %% both run in their own separate process, not this one. `named_table`
    %% with a generated name, not an anonymous tid(): `ct_hooks`' own
    %% installation step formats its Opts (to identify/log the hook) and
    %% fails with `bad_installation` if that includes an opaque tid() -
    %% "#Ref<...>" isn't valid Erlang source, so re-parsing it blows up.
    %% An atom round-trips fine.
    TableName = list_to_atom("lsp_testing_results_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Table = ets:new(TableName, [set, public, named_table]),
    %% See `add_project_ebin_paths/0` below: the bridge node's own code path
    %% has nothing of the project under test on it, so any test calling
    %% another module of that project would otherwise die with `undef`.
    AddedPaths = add_project_ebin_paths(),
    Coverage andalso cover:start(),
    try
        run_eunit(Socket, EunitTargets, Table, Coverage),
        run_ct(Socket, CtTargets, Table, Coverage),
        ets:delete(Table, '$socket'),
        Summary = summarize(ets:tab2list(Table)),
        CoverageResult = case Coverage of
            true -> collect_coverage([M || #{module := M} <- Targets]);
            false -> []
        end,
        #{summary => Summary, coverage => CoverageResult}
    after
        ets:delete(Table),
        %% Reverts every cover-compiled module back to its plain, uninstrumented
        %% object code - the next (possibly non-coverage) run must not keep
        %% paying the instrumentation overhead, or keep serving stale line
        %% counts from this run.
        Coverage andalso cover:stop(),
        %% Must run *after* cover:stop/0, which resolves each module's
        %% original (non-instrumented) object code through the code path.
        remove_project_ebin_paths(AddedPaths)
    end.

%% ============================================================================
%% project code path
%% ============================================================================

%% The bridge node's own code path is *only* the extension's own app: it is
%% started with cwd = <extension>/_build/default/lib/vscode_lsp and
%% `-pa src -pa ebin` (lspclientextension.ts:255 -> ErlangShellLSP.ts), so
%% nothing of the project under test is loadable from it. ensure_module_loaded/2
%% compiles and loads the *test module itself* from source, but never anything
%% that module calls - so `foo_tests` calling `foo:start()` dies with `undef`,
%% for a reason that has nothing to do with the test itself.
%%
%% The debugger already solves exactly this for the debuggee node
%% (ErlangShellDebugger.ts findEbinDirs/2 -> one `-pz` per ebin found under
%% `_build`, driven by the `addEbinsToCodepath` launch attribute). Do the same
%% here, for the duration of one run.
-spec project_ebin_dirs() -> [file:filename()].
project_ebin_dirs() ->
    Root = gen_lsp_config_server:root(),
    %% `undefined` means "no rebar.config at the workspace root". Fall back to
    %% the conventional name anyway rather than giving up: a non-rebar project
    %% may still have a `_build`, and if it doesn't, the wildcard below just
    %% returns [] and this costs one failed directory listing.
    BuildDir = case gen_lsp_doc_server:get_build_dir() of
        undefined -> "_build";
        Dir -> Dir
    end,
    %% wildcard/2, not wildcard/1: the pattern must not contain the workspace
    %% root, or a `[`, `{` or `*` anywhere in the user's own directory names
    %% would be interpreted as a glob. Cwd-relative matching keeps the root
    %% itself literal. Results come back relative to Root.
    Patterns = [
        filename:join([BuildDir, "**", "ebin"]),  % rebar3
        "ebin",                                   % flat / erlang.mk layout
        filename:join(["apps", "*", "ebin"])       % umbrella built in place
    ],
    Relative = lists:usort(lists:flatmap(
        fun (Pattern) -> filelib:wildcard(Pattern, Root) end, Patterns)),
    [Abs || Rel <- Relative,
            Abs <- [filename:join(Root, Rel)],
            filelib:is_dir(Abs)].

%% Returns only the directories this call actually added, so the cleanup
%% below can never remove an entry that was already there for another reason.
-spec add_project_ebin_paths() -> [file:filename()].
add_project_ebin_paths() ->
    CurrentPath = code:get_path(),
    Added = [Dir || Dir <- project_ebin_dirs(), not lists:member(Dir, CurrentPath)],
    %% `add_pathsz` (append), NEVER `add_pathsa`. This node is the language
    %% server itself: prepending would let a stale .beam in the project under
    %% test shadow an OTP module or one of the bridge's own modules that
    %% hasn't been lazily loaded yet - not hypothetical, since the person
    %% most likely to hit this is an extension developer with vscode_erlang
    %% itself open as the workspace, whose own `_build/**/lib/vscode_lsp/ebin`
    %% hold other builds of the very modules running this code. Appending
    %% keeps them strictly below the node's own `-pa ebin`.
    code:add_pathsz(Added),
    Added =/= [] andalso ?LOG("lsp_testing: code path + ~p", [Added]),
    Added.

%% Undo add_project_ebin_paths/0. The bridge node outlives every test run, so:
%%  - the extra path entries must not leak into unrelated LSP work afterwards;
%%  - more importantly, any project module auto-loaded *through* those entries
%%    must be unloaded, or a second run would keep serving the copy the first
%%    run happened to load and would silently ignore a `rebar3 compile` done
%%    in between.
%% Only modules whose loaded-from filename is inside one of *our* dirs are
%% touched: the bridge's own modules were loaded through the relative
%% `-pa ebin` and so never match an absolute path here.
-spec remove_project_ebin_paths([file:filename()]) -> ok.
remove_project_ebin_paths([]) ->
    ok;
remove_project_ebin_paths(Dirs) ->
    lists:foreach(fun
        ({Module, Filename}) when is_list(Filename) ->
            case lists:member(filename:dirname(Filename), Dirs) of
                true  -> code:purge(Module), code:delete(Module);
                false -> ok
            end;
        (_) ->
            %% preloaded modules report their filename as an atom
            ok
    end, code:all_loaded()),
    lists:foreach(fun code:del_path/1, Dirs),
    ok.

%% No explicit tests requested => run every test discovery finds.
resolve_targets([]) ->
    [
        #{
            module => lsp_utils:bin_to_atom(maps:get(module, M)),
            kind => kind_atom(maps:get(kind, M)),
            functions => []
        }
        || M <- discover_modules()
    ];
resolve_targets(Requested) ->
    Grouped = lists:foldl(fun (Item, Acc) ->
        Module = lsp_utils:bin_to_atom(maps:get(module, Item)),
        Function = lsp_utils:bin_to_atom(maps:get(function, Item)),
        Kind = case maps:get(kind, Item, undefined) of
            undefined -> infer_kind(Module);
            KindValue -> kind_atom(KindValue)
        end,
        maps:update_with({Module, Kind}, fun (Fns) -> [Function | Fns] end, [Function], Acc)
    end, #{}, Requested),
    [#{module => M, kind => K, functions => Fns} || {{M, K}, Fns} <- maps:to_list(Grouped)].

infer_kind(Module) ->
    case is_ct_suite(atom_to_list(Module)) of
        true -> ct;
        false -> eunit
    end.

kind_atom(<<"ct">>) -> ct;
kind_atom(<<"eunit">>) -> eunit;
kind_atom(ct) -> ct;
kind_atom(eunit) -> eunit.

run_eunit(_Socket, [], _Table, _Coverage) ->
    ok;
run_eunit(Socket, Targets, Table, Coverage) ->
    Specs = lists:flatmap(fun (#{module := M, functions := Fns}) ->
        ensure_module_loaded(M, Coverage),
        case Fns of
            [] -> [M];
            _ -> [{M, F} || F <- Fns]
        end
    end, Targets),
    catch eunit:test(Specs, [{report, {lsp_testing_eunit_report, [{socket, Socket}, {result_table, Table}]}}]),
    ok.

run_ct(_Socket, [], _Table, _Coverage) ->
    ok;
run_ct(Socket, Targets, Table, Coverage) ->
    %% `ct:run_test/1` changes the current working directory to its own
    %% log directory while running. If our own hook module or lsp_utils
    %% etc. haven't been loaded yet, the code server's usual lazy load can
    %% fail right after that chdir - any relative `-pa` entry from the
    %% node's boot arguments (this is exactly how the real bridge node is
    %% started - see lspclientextension.ts/ErlangShellLSP) no longer
    %% resolves once cwd has moved. Force-loading it up front sidesteps
    %% that entirely.
    code:ensure_loaded(lsp_testing_ct_hook),
    code:ensure_loaded(gen_lsp_server),
    code:ensure_loaded(lsp_utils),
    code:ensure_loaded(vscode_jsone),
    LogDir = ct_log_dir(),
    filelib:ensure_dir(filename:join(LogDir, "dummy")),
    %% `ct_hooks` Opts are formatted-then-reparsed by ct's own installation
    %% step (see the `TableName`/`named_table` comment in run_tests/2) -
    %% the socket (a port()) can't survive that round-trip either, so it's
    %% stashed in the results table under a reserved key instead of being
    %% passed through Opts directly.
    ets:insert(Table, {'$socket', Socket}),
    lists:foreach(fun (#{module := Suite, functions := Fns}) ->
        ensure_module_loaded(Suite, Coverage),
        Opts = [
            {suite, Suite},
            {logdir, LogDir},
            {ct_hooks, [{lsp_testing_ct_hook, [{result_table, Table}]}]}
        ] ++ case Fns of
            [] -> [];
            _ -> [{testcase, Fns}]
        end,
        catch ct:run_test(Opts)
    end, Targets),
    ok.

ct_log_dir() ->
    filename:join([gen_lsp_config_server:root(), "_build", "test", "lsp_testing_logs"]).

%% Task 6.6: with coverage requested, always (re)compile through `cover`
%% instead of the plain compile+load path - `cover:compile_module/2`
%% instruments the module for line-hit counting; the ordinary
%% code:is_loaded/1 short-circuit doesn't apply here, since a module
%% loaded by an *earlier*, non-coverage run needs to be swapped out for
%% its instrumented twin.
ensure_module_loaded(Module, true) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            ok;
        SourceFile ->
            %% Unlike compile:file/2 above, `binary`/`report_errors` aren't
            %% valid here - cover:compile_module/2 manages the actual
            %% compile+instrument+load pipeline itself and only wants the
            %% extra options (include paths/defines) layered on top of it.
            %% `{d,'TEST'}`: see the comment on the `false` clause below.
            Options = [{d, 'TEST'} | [{i, Path} || Path <- lsp_parse:get_include_path(SourceFile)]],
            catch cover:compile_module(SourceFile, Options),
            ok
    end;
ensure_module_loaded(Module, false) ->
    case code:is_loaded(Module) of
        false ->
            case gen_lsp_doc_server:get_module_file(Module) of
                undefined ->
                    ok;
                SourceFile ->
                    %% `{d,'TEST'}`: exactly what rebar3's eunit provider (and
                    %% erlang.mk) inject for a test build. Without it, a
                    %% module whose tests live inside
                    %% `-ifdef(TEST). ... -endif.` compiles *without* them -
                    %% discovery still finds them (it reads the dodged tree,
                    %% which ignores preprocessor conditionals entirely), so
                    %% without this define the Test Explorer would offer a
                    %% test that this compile can't possibly produce.
                    Options = [binary, report_errors, {d, 'TEST'}
                               | [{i, Path} || Path <- lsp_parse:get_include_path(SourceFile)]],
                    case compile:file(SourceFile, Options) of
                        {ok, ModuleName, Binary} ->
                            code:load_binary(ModuleName, SourceFile, Binary);
                        _ ->
                            ok
                    end
            end;
        _ ->
            ok
    end.

%% Task 6.6: per-line call counts for every module that was part of this
%% run, via `cover:analyse(Module, calls, line)` - a plain executed/not-
%% executed boolean loses "how many times", which VS Code's
%% StatementCoverage accepts as a number and is strictly more informative.
collect_coverage(Modules) ->
    lists:filtermap(fun module_coverage/1, lists:usort(Modules)).

module_coverage(Module) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            false;
        SourceFile ->
            case cover:analyse(Module, calls, line) of
                {ok, LineCalls} ->
                    Statements = [
                        #{line => Line, executed => Calls}
                        || {{_M, Line}, Calls} <- LineCalls, Line > 0
                    ],
                    case Statements of
                        [] -> false;
                        _ ->
                            {true, #{
                                uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(SourceFile)),
                                statements => Statements
                            }}
                    end;
                {error, _Reason} ->
                    false
            end
    end.

summarize(Results) ->
    lists:foldl(fun ({_Key, Status, _Message, _Line}, Acc) ->
        maps:update_with(Status, fun (N) -> N + 1 end, 1, Acc)
    end, #{<<"passed">> => 0, <<"failed">> => 0, <<"skipped">> => 0}, Results).
