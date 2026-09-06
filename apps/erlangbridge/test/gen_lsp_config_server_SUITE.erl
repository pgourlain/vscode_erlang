-module(gen_lsp_config_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Feeds gen_lsp_config_server:update_config/2 the same section shapes the
%% real client sends - "erlang" is whatever resolveErlangSettings/1
%% (lib/ErlangConfigurationProvider.ts) JSON-round-trips from VS Code's
%% erlang.* settings (so every key from package.json's contributes.
%% configuration, camelCase, no "erlang." prefix), and "computed" is the
%% synthetic section Configuration.computeConfiguration builds itself
%% (lib/lsp/lspclientextension.ts:67-79): autosave/tmpdir/username.

all() -> [
    getters_read_through_a_realistic_erlang_section,
    autosave_reads_through_the_computed_section,
    getters_fall_back_to_their_declared_defaults_when_config_is_absent,
    server_defaults_match_the_declared_package_json_defaults,
    verbose_is_include_reflects_the_computed_exclude_filter
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% The shape resolveErlangSettings/1 actually produces: every erlang.*
%% setting from package.json, camelCase, values non-default so read-through
%% (not a lucky default match) is what's actually being proven.
getters_read_through_a_realistic_erlang_section(_Config) ->
    gen_lsp_config_server:update_config(erlang, #{
        erlangPath => <<"/usr/bin/erl">>,
        erlangArgs => [],
        erlangDistributedNode => false,
        rebarPath => <<"/usr/bin/rebar3">>,
        rebarBuildArgs => [<<"compile">>],
        includePaths => [<<"include">>],
        linting => false,
        cacheManagement => <<"memory">>,
        codeLensEnabled => true,
        inlayHintsEnabled => true,
        verbose => true,
        verboseExcludeFilter => <<"textDocument/inlayHints,textDocument/hover">>,
        debuggerRunMode => <<"external">>,
        formattingLineLength => 40
    }),
    ?assertEqual(true, gen_lsp_config_server:codeLensEnabled()),
    ?assertEqual(true, gen_lsp_config_server:inlayHintsEnabled()),
    ?assertEqual(false, gen_lsp_config_server:linting()),
    ?assertEqual(true, gen_lsp_config_server:verbose()),
    ?assertEqual(40, gen_lsp_config_server:formatting_line_length()).

%% autosave/0 reads a *different* section: "computed", the synthetic one
%% Configuration.computeConfiguration builds itself rather than reading
%% straight from a VS Code setting (there is no erlang.autosave setting -
%% it is derived from files.autoSave === "afterDelay").
autosave_reads_through_the_computed_section(_Config) ->
    gen_lsp_config_server:update_config(computed, #{
        autosave => true,
        tmpdir => <<"/tmp">>,
        username => <<"tester">>
    }),
    ?assertEqual(true, gen_lsp_config_server:autosave()).

%% Before any workspace/configuration exchange has happened at all (no
%% update_config call for a section yet), every getter falls back to its
%% own hardcoded default rather than crashing on a missing key.
getters_fall_back_to_their_declared_defaults_when_config_is_absent(_Config) ->
    gen_lsp_config_server:update_config(erlang, #{}),
    gen_lsp_config_server:update_config(computed, #{}),
    ?assertEqual(false, gen_lsp_config_server:codeLensEnabled()),
    ?assertEqual(false, gen_lsp_config_server:inlayHintsEnabled()),
    ?assertEqual(true, gen_lsp_config_server:linting()),
    ?assertEqual(false, gen_lsp_config_server:verbose()),
    ?assertEqual(100, gen_lsp_config_server:formatting_line_length()),
    ?assertEqual(true, gen_lsp_config_server:autosave()).

%% Cross-checks package.json's contributes.configuration defaults against
%% this module's own hardcoded fallbacks, so the two can never silently
%% drift apart - a genuine risk since nothing else ties them together.
server_defaults_match_the_declared_package_json_defaults(_Config) ->
    PackageJsonFile = filename:join([code:lib_dir(vscode_lsp), "..", "..", "..", "..", "package.json"]),
    {ok, Bin} = file:read_file(filename:absname(PackageJsonFile)),
    Package = jsone_decode_compatible(Bin),
    Properties = maps:get(<<"properties">>, maps:get(<<"configuration">>, maps:get(<<"contributes">>, Package))),
    DeclaredDefault = fun (Key) -> maps:get(<<"default">>, maps:get(<<"erlang.", Key/binary>>, Properties)) end,
    gen_lsp_config_server:update_config(erlang, #{}),
    ?assertEqual(DeclaredDefault(<<"codeLensEnabled">>), gen_lsp_config_server:codeLensEnabled()),
    ?assertEqual(DeclaredDefault(<<"inlayHintsEnabled">>), gen_lsp_config_server:inlayHintsEnabled()),
    ?assertEqual(DeclaredDefault(<<"linting">>), gen_lsp_config_server:linting()),
    ?assertEqual(DeclaredDefault(<<"verbose">>), gen_lsp_config_server:verbose()),
    ?assertEqual(DeclaredDefault(<<"formattingLineLength">>), gen_lsp_config_server:formatting_line_length()).

%% verboseExcludeFilter (a single "a,b;c" string setting) is split by
%% update_config/2's own side effect (compute_erlang_section/1) into an
%% "erlang_computed" section map of {Method => false}; verbose_is_include/1
%% defaults any *other* method to true. Uses package.json's own declared
%% default value for this setting, not a made-up one.
verbose_is_include_reflects_the_computed_exclude_filter(_Config) ->
    gen_lsp_config_server:update_config(erlang, #{
        verboseExcludeFilter => <<"textDocument/inlayHints,textDocument/hover">>
    }),
    ?assertEqual(false, gen_lsp_config_server:verbose_is_include(<<"textDocument/inlayHints">>)),
    ?assertEqual(false, gen_lsp_config_server:verbose_is_include(<<"textDocument/hover">>)),
    ?assertEqual(true, gen_lsp_config_server:verbose_is_include(<<"textDocument/completion">>)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

%% vscode_jsone_decode is already part of this app; reuse it rather than
%% pulling in a separate JSON dependency just for this one test.
jsone_decode_compatible(Bin) ->
    {ok, Term, _} = vscode_jsone_decode:decode(Bin, [{keys, binary}]),
    Term.
