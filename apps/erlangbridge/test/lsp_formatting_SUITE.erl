-module(lsp_formatting_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Tasks 5.3 (range) / 5.4 (on-type) / 5.5 (erlang.formatterEnabled).
%% messy.erl's add/2 (lines 3-4) is badly spaced but single-line-bodied;
%% describe/1 (lines 6-10) is a `case ... end` whose own indentation is
%% wrong throughout - both exercise vscode_erlfmt's own "range auto-
%% expands to the enclosing form" behavior (format_string/2's own {range,
%% {StartLine, EndLine}} option), which lsp_formatting.erl leans on
%% entirely rather than re-deriving form boundaries itself.

all() -> [
    range_formatting_reformats_just_the_enclosing_function,
    on_type_formatting_reformats_the_cursor_lines_enclosing_function,
    range_formatting_reformats_a_case_block_through_its_own_end,
    range_formatting_on_an_already_clean_function_returns_no_edit,
    formatting_disabled_returns_no_edits_for_range_or_on_type
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

init_per_testcase(_TestCase, Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(erlang, #{verbose => false, formattingLineLength => 100}),
    gen_lsp_config_server:update_config(root, AppDir),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% add(A,B) -> \n  A+B. (lines 3-4, 0-based 2-3) reformats to erlfmt's own
%% canonical spacing - a single scoped edit, not a whole-document replace.
range_formatting_reformats_just_the_enclosing_function(Config) ->
    File = source_file(Config),
    [Edit] = lsp_formatting:range(File, 3, 4),
    ?assertEqual(
        #{range => #{<<"start">> => #{line => 2, character => 0}, <<"end">> => #{line => 4, character => 0}},
          newText => <<"add(A, B) ->\n    A + B.\n">>},
        Edit
    ).

%% on_type/2 just asks for the single cursor line - erlfmt's own range
%% auto-expansion still reformats the *whole* enclosing function (add/2),
%% same result as explicitly range-formatting lines 3-4 above.
on_type_formatting_reformats_the_cursor_lines_enclosing_function(Config) ->
    File = source_file(Config),
    ?assertEqual(lsp_formatting:range(File, 3, 4), lsp_formatting:on_type(File, 4)).

%% describe/1 (lines 6-10) is entirely mis-indented (2-space case, 6-space
%% clauses) - range-formatting any line inside it reindents the whole
%% `case ... end`, matching vscode_erlfmt's own canonical 4-space style.
range_formatting_reformats_a_case_block_through_its_own_end(Config) ->
    File = source_file(Config),
    [Edit] = lsp_formatting:range(File, 8, 8),
    ?assertEqual(
        #{range => #{<<"start">> => #{line => 6, character => 0}, <<"end">> => #{line => 10, character => 0}},
          newText => <<"    case Value of\n        0 -> zero;\n        _ -> nonzero\n    end.\n">>},
        Edit
    ).

%% Once a function is already in erlfmt's own canonical form, formatting
%% any line inside it again is a true no-op - no edit at all, not an edit
%% that replaces text with itself.
range_formatting_on_an_already_clean_function_returns_no_edit(Config) ->
    File = source_file(Config),
    [Edit] = lsp_formatting:range(File, 3, 4),
    #{range := #{<<"start">> := #{line := SL}, <<"end">> := #{line := EL}}, newText := NewText} = Edit,
    {ok, Original} = file:read_file(File),
    Cleaned = apply_change(Original, SL, EL, NewText),
    CleanedFile = write_temp_copy(Config, Cleaned),
    ?assertEqual([], lsp_formatting:range(CleanedFile, 3, 4)),
    file:delete(CleanedFile).

formatting_disabled_returns_no_edits_for_range_or_on_type(Config) ->
    File = source_file(Config),
    gen_lsp_config_server:update_config(erlang, #{verbose => false, formattingLineLength => 100, formatterEnabled => false}),
    Uri = lsp_utils:file_to_file_uri(File),
    RangeParams = #{textDocument => #{uri => Uri},
                     range => #{start => #{line => 2, character => 0}, 'end' => #{line => 3, character => 0}}},
    OnTypeParams = #{textDocument => #{uri => Uri}, position => #{line => 3, character => 0}},
    ?assertEqual([], lsp_handlers:textDocument_rangeFormatting(undefined, RangeParams)),
    ?assertEqual([], lsp_handlers:textDocument_onTypeFormatting(undefined, OnTypeParams)),
    gen_lsp_config_server:update_config(erlang, #{verbose => false, formattingLineLength => 100}).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

source_file(Config) ->
    AppDir = ?config(data_dir, Config),
    filename:join(AppDir, "messy.erl").

apply_change(Content, StartLine, EndLine, NewText) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    LineStart = fun (N) -> lists:sum([byte_size(L) + 1 || L <- lists:sublist(Lines, N)]) end,
    StartOffset = LineStart(StartLine),
    EndOffset = LineStart(EndLine),
    Before = binary:part(Content, 0, StartOffset),
    After = binary:part(Content, EndOffset, byte_size(Content) - EndOffset),
    <<Before/binary, NewText/binary, After/binary>>.

write_temp_copy(Config, Content) ->
    PrivDir = ?config(priv_dir, Config),
    File = filename:join(PrivDir, "messy_cleaned.erl"),
    ok = file:write_file(File, Content),
    File.
