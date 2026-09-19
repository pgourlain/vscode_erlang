-module(lsp_semantic_tokens_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 3.1: full-document semantic tokens, deliberately semantic-only (see
%% lsp_semantic_tokens.erl's own top-of-file note) - string/number/comment/
%% keyword/operator are declared in the legend but never emitted, since the
%% existing TextMate grammar already colors those correctly.
%%
%% tokens_source.erl exercises every emitted category at once: -module,
%% -record (definition + construction + update + field access), -type/
%% -opaque + -spec (+ a builtin type, deliberately NOT tokenized), -define
%% + usage, -deprecated matched against both a definition and (implicitly)
%% any call site, a multi-clause-free function pair with parameter vs.
%% variable vs. re-reference distinctions, a local call and a
%% module-qualified (OTP, so defaultLibrary) call.

all() -> [
    legend_lists_every_type_and_modifier,
    module_declaration_is_tokenized_as_namespace,
    record_declaration_tokenizes_name_and_fields,
    type_and_spec_tokenize_names_but_not_builtin_types,
    macro_definition_and_usage_are_tokenized,
    function_clauses_are_tokenized_as_definitions_and_deprecated_flows_through,
    parameters_and_variables_are_distinguished_and_reuses_carry_no_modifier,
    record_usage_sites_are_tokenized_struct_and_property,
    local_and_remote_calls_are_tokenized_and_otp_calls_get_defaultLibrary,
    unparseable_file_returns_empty_data,
    delta_with_unchanged_content_returns_no_edits,
    delta_after_an_edit_returns_a_single_edit_for_the_changed_region,
    delta_with_a_stale_previous_result_id_falls_back_to_full_data,
    range_tokens_only_includes_the_requested_lines,
    disabling_the_setting_returns_no_tokens_at_all
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

init_per_testcase(_TestCase, Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

legend_lists_every_type_and_modifier(_Config) ->
    ?assertEqual(#{
        tokenTypes => [<<"namespace">>, <<"function">>, <<"macro">>, <<"variable">>,
                       <<"parameter">>, <<"type">>, <<"struct">>, <<"property">>,
                       <<"string">>, <<"number">>, <<"comment">>, <<"keyword">>, <<"operator">>],
        tokenModifiers => [<<"definition">>, <<"declaration">>, <<"readonly">>,
                           <<"deprecated">>, <<"defaultLibrary">>]
    }, lsp_semantic_tokens:legend()).

%% -module(tokens_source). - the attribute's own AST position is the
%% `module` keyword atom (itself an ordinary, unreserved atom token), not
%% the name - a naive "first atom at-or-after" scan would wrongly land on
%% the keyword itself, which is exactly the bug caught while building this.
module_declaration_is_tokenized_as_namespace(Config) ->
    ?assertEqual({1, 9, 13, namespace, [definition]}, token_at(Config, 1, 9)).

%% -record(rec, {a, b = 0}). - the record name needs the same keyword-atom
%% scan as -module; both fields (one with a default value, one without -
%% two different tuple arities for `record_field`) are tokenized directly
%% from their own AST position, no scanning needed.
record_declaration_tokenizes_name_and_fields(Config) ->
    ?assertEqual({4, 9, 3, struct, [definition]}, token_at(Config, 4, 9)),
    ?assertEqual({4, 15, 1, property, [definition]}, token_at(Config, 4, 15)),
    ?assertEqual({4, 18, 1, property, [definition]}, token_at(Config, 4, 18)).

%% -type my_type() :: integer(). / -spec f(my_type()) -> atom(). -
%% my_type's *declaration* (line 5) and its *usage* inside the spec
%% (line 6) are both tokenized as `type`; f's own name inside its -spec is
%% `function`+declaration (distinct from its defining clause, which gets
%% `definition`); the builtin `integer()`/`atom()` types are deliberately
%% never tokenized at all (erl_syntax_lib:fold doesn't even descend into
%% spec/type payloads - user_type references are found with a small
%% generic term walk instead, and only user_type, never the builtin `type`
%% tag, is tokenized).
type_and_spec_tokenize_names_but_not_builtin_types(Config) ->
    ?assertEqual({5, 7, 7, type, [definition]}, token_at(Config, 5, 7)),
    ?assertEqual({6, 7, 1, function, [declaration]}, token_at(Config, 6, 7)),
    ?assertEqual({6, 9, 7, type, []}, token_at(Config, 6, 9)),
    %% "atom()" (the -spec's return type) is a builtin type, never emitted:
    %% my_type's usage above is the *only* `type` token on this line.
    Line6TypeTokens = [T || {6, _, _, type, _} = T <- tokens(Config)],
    ?assertEqual([{6, 9, 7, type, []}], Line6TypeTokens).

%% -define(FOO, 42). / ?FOO usage on line 11 - macros are expanded away by
%% epp before the AST is built, so both are found the same way
%% lsp_navigation:find_macro_reference/2 already finds them: a plain
%% per-line regex, not the AST.
macro_definition_and_usage_are_tokenized(Config) ->
    ?assertEqual({8, 9, 3, macro, [definition]}, token_at(Config, 8, 9)),
    ?assertEqual({11, 26, 3, macro, []}, token_at(Config, 11, 26)).

%% f/1's clause and old/0's clause are both `function`+definition; old/0
%% additionally carries `deprecated` (from -deprecated([{old,0,...}])).
function_clauses_are_tokenized_as_definitions_and_deprecated_flows_through(Config) ->
    ?assertEqual({10, 1, 1, function, [definition]}, token_at(Config, 10, 1)),
    ?assertEqual({17, 1, 1, function, [definition]}, token_at(Config, 17, 1)),
    ?assertEqual({19, 1, 3, function, [definition, deprecated]}, token_at(Config, 19, 1)).

%% f(X) -> ... : X is `parameter`+definition at the clause head (line 10).
%% Every later reference to X or to a body-bound variable (R on line 11,
%% then reused on lines 12-13; Y bound line 12, reused line 13) is a plain
%% `variable` with *no* modifier - never re-flagged as a second
%% definition, and a parameter's own first reference inside the body
%% (X on line 11) is correctly a reference too, not a fresh definition.
parameters_and_variables_are_distinguished_and_reuses_carry_no_modifier(Config) ->
    ?assertEqual({10, 3, 1, parameter, [definition]}, token_at(Config, 10, 3)),
    ?assertEqual({11, 5, 1, variable, [definition]}, token_at(Config, 11, 5)),
    ?assertEqual({11, 18, 1, variable, []}, token_at(Config, 11, 18)),
    ?assertEqual({12, 5, 1, variable, [definition]}, token_at(Config, 12, 5)),
    ?assertEqual({12, 9, 1, variable, []}, token_at(Config, 12, 9)),
    ?assertEqual({13, 20, 1, variable, []}, token_at(Config, 13, 20)).

%% #rec{a = X, b = ?FOO} (construction, line 11), R#rec.a (field access,
%% line 12) and R#rec{a = Y} (update, line 13) all tokenize the record name
%% as `struct` (every one of these AST shapes' own Pos is the '#'
%% character itself, one column before the name) and each field as
%% `property`, with no modifier (only the -record's own field list, task
%% 4's other test, carries `definition`).
record_usage_sites_are_tokenized_struct_and_property(Config) ->
    ?assertEqual({11, 10, 3, struct, []}, token_at(Config, 11, 10)),
    ?assertEqual({11, 14, 1, property, []}, token_at(Config, 11, 14)),
    ?assertEqual({12, 11, 3, struct, []}, token_at(Config, 12, 11)),
    ?assertEqual({12, 15, 1, property, []}, token_at(Config, 12, 15)),
    ?assertEqual({13, 12, 3, struct, []}, token_at(Config, 13, 12)),
    ?assertEqual({13, 16, 1, property, []}, token_at(Config, 13, 16)).

%% lists:reverse(...) (line 14) is module-qualified and `lists` is a real
%% OTP module (known to gen_lsp_config_server:standard_modules/0), so it
%% gets `defaultLibrary`; g(X) (line 15) is a plain local call, `function`
%% with no modifier at all - it is not a definition, and not deprecated.
local_and_remote_calls_are_tokenized_and_otp_calls_get_defaultLibrary(Config) ->
    ?assertEqual({14, 5, 5, namespace, [defaultLibrary]}, token_at(Config, 14, 5)),
    ?assertEqual({14, 11, 7, function, []}, token_at(Config, 14, 11)),
    ?assertEqual({15, 5, 1, function, []}, token_at(Config, 15, 5)).

%% A file gen_lsp_doc_server can't produce a syntax tree for (here: one
%% that plain doesn't exist, so get_syntax_tree/1 has nothing to parse)
%% must not crash the request - just return no tokens.
unparseable_file_returns_empty_data(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "does_not_exist.erl"),
    ?assertEqual(#{data => []}, lsp_semantic_tokens:full_tokens(File)).

%% delta_sample.erl is only touched by the three delta tests below, each of
%% which reopens it with known content and closes it again afterward - so
%% none of them can see a stale in-memory edit left behind by another
%% test, or by test execution order.
delta_with_unchanged_content_returns_no_edits(Config) ->
    File = reopen_delta_sample(Config),
    #{resultId := ResultId1} = lsp_semantic_tokens:full_tokens(File),
    ?assertEqual(#{resultId => ResultId1, edits => []},
                 lsp_semantic_tokens:full_tokens_delta(File, ResultId1)),
    gen_lsp_doc_server:document_closed(File).

%% Appending a new function is a pure insert at the very end of the token
%% stream: the common-prefix/common-suffix diff should reduce to exactly
%% one edit that only adds the new function's own token group, deleting
%% nothing.
delta_after_an_edit_returns_a_single_edit_for_the_changed_region(Config) ->
    File = reopen_delta_sample(Config),
    #{resultId := ResultId1, data := Data1} = lsp_semantic_tokens:full_tokens(File),
    {ok, Original} = file:read_file(File),
    Modified = <<Original/binary, "\ng() -> ok.\n">>,
    gen_lsp_doc_server:document_opened(File, Modified),
    gen_lsp_doc_server:parse_document(File),
    Delta = lsp_semantic_tokens:full_tokens_delta(File, ResultId1),
    ?assertMatch(#{edits := [#{start := _, deleteCount := 0, data := _}]}, Delta),
    #{edits := [#{start := Start, data := NewGroup}]} = Delta,
    ?assertEqual(length(Data1), Start),
    ?assertEqual(5, length(NewGroup)),
    gen_lsp_doc_server:document_closed(File).

delta_with_a_stale_previous_result_id_falls_back_to_full_data(Config) ->
    File = reopen_delta_sample(Config),
    Delta = lsp_semantic_tokens:full_tokens_delta(File, <<"this-id-was-never-issued">>),
    ?assertMatch(#{data := _}, Delta),
    ?assertNot(maps:is_key(edits, Delta)),
    gen_lsp_doc_server:document_closed(File).

reopen_delta_sample(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "delta_sample.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File),
    File.

%% Only tokens on lines 10-13 (1-based) of tokens_source.erl: f/1's own
%% definition, its parameter, and the R/#rec construction on the following
%% lines - nothing from -module/-record/-type/-spec above, nor from g/1 or
%% old/0 below.
range_tokens_only_includes_the_requested_lines(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "tokens_source.erl"),
    #{data := Data} = lsp_semantic_tokens:range_tokens(File, {10, 13}),
    Decoded = decode(Data, 1, 1),
    Lines = lists:usort([L || {L, _, _, _, _} <- Decoded]),
    ?assertEqual([10, 11, 12, 13], Lines).

%% erlang.semanticTokensEnabled (task 3.4, default true - see
%% gen_lsp_config_server:semanticTokensEnabled/0) short-circuits full,
%% delta and range alike to an empty result, with no cache side effect.
disabling_the_setting_returns_no_tokens_at_all(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "tokens_source.erl"),
    gen_lsp_config_server:update_config(erlang, #{verbose => false, semanticTokensEnabled => false}),
    ?assertEqual(#{data => []}, lsp_semantic_tokens:full_tokens(File)),
    ?assertEqual(#{data => []}, lsp_semantic_tokens:full_tokens_delta(File, <<"anything">>)),
    ?assertEqual(#{data => []}, lsp_semantic_tokens:range_tokens(File, {1, 100})),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

tokens(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "tokens_source.erl"),
    #{data := Data} = lsp_semantic_tokens:full_tokens(File),
    decode(Data, 1, 1).

token_at(Config, Line, Col) ->
    case [T || {L, C, _, _, _} = T <- tokens(Config), L =:= Line, C =:= Col] of
        [Token] -> Token;
        [] -> ct:fail({no_token_at, Line, Col});
        Many -> ct:fail({multiple_tokens_at, Line, Col, Many})
    end.

%% Mirrors the client side of the LSP semantic tokens delta encoding
%% (see lsp_semantic_tokens:encode/1) - decodes back into absolute
%% {Line, Col, Length, TypeName, ModifierNames} tuples for readable
%% assertions.
decode([], _Line, _Col) -> [];
decode([DL, DC, Len, Type, Mods | Rest], Line, Col) ->
    NewLine = Line + DL,
    NewCol = case DL of 0 -> Col + DC; _ -> DC + 1 end,
    [{NewLine, NewCol, Len, type_name(Type), mod_names(Mods)} | decode(Rest, NewLine, NewCol)].

type_name(0) -> namespace;
type_name(1) -> function;
type_name(2) -> macro;
type_name(3) -> variable;
type_name(4) -> parameter;
type_name(5) -> type;
type_name(6) -> struct;
type_name(7) -> property;
type_name(N) -> N.

mod_names(Mods) ->
    [Name || {Bit, Name} <- [{1, definition}, {2, declaration}, {4, readonly},
                              {8, deprecated}, {16, defaultLibrary}], Mods band Bit =/= 0].
