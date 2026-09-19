-module(lsp_inlinevalues_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Drives the real, exported lsp_handlers:textDocument_inlineValues/2
%% (Socket unused, so `undefined` stands in), for a debugger paused-frame
%% scenario over apps/erlangbridge/test/lsp_inlinevalues_SUITE_data/inlinevalues_source.erl:
%%
%%   go(A, B) ->
%%       Sum = A + B,
%%       helper(A + B),
%%       helper(A),
%%       Sum.
%%
%%   helper(X) ->
%%       X.

all() -> [
    plural_method_name_is_what_the_real_client_sends,
    inline_values_reports_every_variable_occurrence_in_the_enclosing_function,
    operator_expression_passed_as_a_call_argument_is_reported_as_expression,
    bare_variable_passed_as_a_call_argument_is_not_reported,
    enclosing_function_lookup_has_no_upper_bound_check
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    File = filename:join(AppDir, "inlinevalues_source.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    [{source_file, File} | Config].

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% CHARACTERIZATION: the real TS client (lib/lsp/lsp-inlinevalues.ts) sends
%% the non-standard, plural 'textDocument/inlineValues' - not the LSP 3.17
%% spec's singular 'textDocument/inlineValue'. The server exports handlers
%% for both names (textDocument_inlineValues/2 is the implementation;
%% textDocument_inlineValue/2 is a one-line alias to it, lsp_handlers.erl:
%% 290-292), but only the plural one is ever reached by the shipped client.
plural_method_name_is_what_the_real_client_sends(Config) ->
    TsFile = filename:join([code:lib_dir(vscode_lsp), "..", "..", "..", "..", "lib", "lsp", "lsp-inlinevalues.ts"]),
    {ok, TsSource} = file:read_file(filename:absname(TsFile)),
    ?assert(binary:match(TsSource, <<"'textDocument/inlineValues'">>) =/= nomatch),
    Exports = lsp_handlers:module_info(exports),
    ?assert(lists:member({textDocument_inlineValues, 2}, Exports)),
    ?assert(lists:member({textDocument_inlineValue, 2}, Exports)),
    %% and they really are interchangeable today: same params in, same result out
    File = ?config(source_file, Config),
    {Line, Character} = paused_in_go(),
    Params = stopped_at_params_for(File, Line, Character),
    ?assertEqual(
        lsp_handlers:textDocument_inlineValues(undefined, Params),
        lsp_handlers:textDocument_inlineValue(undefined, Params)
    ).

%% Paused inside go/2 (on the "helper(A + B)," line): every variable
%% occurrence in the *whole enclosing function* is reported, not just the
%% ones textually before the stopped line - Sum's binding, its use as the
%% return value further down, and both parameters from the function head.
inline_values_reports_every_variable_occurrence_in_the_enclosing_function(Config) ->
    Values = inline_values(Config, paused_in_go()),
    Vars = [maps:get(label, V) || V <- Values, maps:get(kind, V) =:= <<"var">>],
    ?assertEqual([<<"A">>, <<"A">>, <<"B">>, <<"B">>, <<"Sum">>, <<"Sum">>], lists:sort(Vars)).

%% helper(A + B): the operator expression is reported as one 'expression'
%% entry, pretty-printed - not decomposed into its A/B operands.
operator_expression_passed_as_a_call_argument_is_reported_as_expression(Config) ->
    Values = inline_values(Config, paused_in_go()),
    Expressions = [V || V <- Values, maps:get(kind, V) =:= <<"expression">>],
    ?assertEqual([<<"A + B">>], [maps:get(label, V) || V <- Expressions]).

%% CHARACTERIZATION: helper(A) - a variable passed directly (not wrapped in
%% an operator) as a call argument - produces nothing at all. The
%% 'application' branch of find_variables_and_expression/2 only ever
%% extracts operator subtrees (expressions_2/2); a bare variable argument
%% falls through its catch-all and is dropped, unlike the same variable
%% used anywhere outside a call argument position.
bare_variable_passed_as_a_call_argument_is_not_reported(Config) ->
    Values = inline_values(Config, paused_in_go()),
    Vars = [maps:get(label, V) || V <- Values, maps:get(kind, V) =:= <<"var">>],
    %% A appears twice (function head parameter, and inside "Sum = A + B"),
    %% never a third time for the bare argument in "helper(A)"
    ?assertEqual(2, length([A || A <- Vars, A =:= <<"A">>])).

%% CHARACTERIZATION: inlinevalues_info/2 picks "the last function in the
%% file whose start line is <= the stopped line" - with no check that the
%% stopped line actually falls before that function ends. A wildly
%% out-of-range stopped line (past both functions entirely) still resolves
%% to a function - the file's very last one (helper/1), not go/2, and not
%% an empty/error result.
enclosing_function_lookup_has_no_upper_bound_check(Config) ->
    Values = inline_values(Config, {9999, 0}),
    Vars = lists:sort([maps:get(label, V) || V <- Values, maps:get(kind, V) =:= <<"var">>]),
    ?assertEqual([<<"X">>, <<"X">>], Vars).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

%% 1-based line of "    helper(A + B)," in the fixture, matching the raw
%% AST line numbers inlinevalues_info/2 compares against directly.
paused_in_go() -> {6, 0}.

inline_values(Config, {Line, Character}) ->
    File = ?config(source_file, Config),
    Params = stopped_at_params_for(File, Line, Character),
    lsp_handlers:textDocument_inlineValues(undefined, Params).

stopped_at_params_for(File, Line, Character) ->
    #{
        textDocument => #{uri => lsp_utils:file_to_file_uri(File)},
        context => #{stoppedLocation => #{'end' => #{line => Line, character => Character}}}
    }.
