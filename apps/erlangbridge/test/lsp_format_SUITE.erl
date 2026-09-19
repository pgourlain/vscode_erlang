-module(lsp_format_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Drives the real, exported lsp_handlers:textDocument_formatting/2 (Socket
%% unused, so `undefined` stands in), which dispatches to vscode_erlfmt on
%% OTP >= 21 (lsp_handlers.erl:427-455) - this repo's CI only ever runs on
%% modern OTP (see task 0.16), so the erl_tidy fallback branch is out of
%% scope here.

all() -> [
    formatting_is_idempotent,
    already_formatted_file_is_unchanged,
    formatting_line_length_is_honored,
    result_range_is_the_documents_real_end
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false, formattingLineLength => 100}),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% restore the default width other test cases rely on
    gen_lsp_config_server:update_config(erlang, #{verbose => false, formattingLineLength => 100}),
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% Formatting a formatter's own output must be a no-op: format(format(X)) =:= format(X).
formatting_is_idempotent(Config) ->
    Once = format_file(Config, "messy.erl"),
    Twice = vscode_erlfmt:format_string(binary_to_list(Once), [{print_width, 100}]),
    ?assertEqual({ok, binary_to_list(Once)}, strip_warnings(Twice)).

%% clean.erl is byte-for-byte vscode_erlfmt's own output for messy.erl at
%% width 100 (generated once, not hand-styled) - formatting it again must
%% change nothing.
already_formatted_file_is_unchanged(Config) ->
    AppDir = ?config(data_dir, Config),
    {ok, Content} = file:read_file(filename:join(AppDir, "clean.erl")),
    Formatted = format_file(Config, "clean.erl"),
    ?assertEqual(Content, Formatted).

%% erlang.formattingLineLength (print_width) changes how a long list call
%% wraps: it stays on one line at the default width, but is exploded one
%% element per line once the width can no longer fit it.
formatting_line_length_is_honored(Config) ->
    Wide = format_file(Config, "long_line.erl"),
    ?assert(lists:member(<<"    lists:sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]).">>,
                         binary:split(Wide, <<"\n">>, [global]))),
    gen_lsp_config_server:update_config(erlang, #{verbose => false, formattingLineLength => 20}),
    Narrow = format_file(Config, "long_line.erl"),
    ?assert(lists:member(<<"    lists:sum([">>, binary:split(Narrow, <<"\n">>, [global]))),
    ?assert(lists:member(<<"        1,">>, binary:split(Narrow, <<"\n">>, [global]))).

%% Task 5.3: the result range used to be a hardcoded {0,0}-{999999,255}
%% sentinel, regardless of the document's real length - now the actual
%% end position (messy.erl is 9 lines, 0-based line 8, ending at the
%% byte length of its own last line).
result_range_is_the_documents_real_end(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "messy.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    [Edit] = lsp_handlers:textDocument_formatting(undefined, Params),
    Lines = binary:split(Content, <<"\n">>, [global]),
    ExpectedEndLine = length(Lines) - 1,
    ExpectedEndChar = byte_size(lists:last(Lines)),
    ?assertMatch(
        #{range := #{
            <<"start">> := #{line := 0, character := 0},
            <<"end">> := #{line := ExpectedEndLine, character := ExpectedEndChar}
        }},
        Edit
    ).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

format_file(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, FileName),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    [#{newText := NewText}] = lsp_handlers:textDocument_formatting(undefined, Params),
    NewText.

strip_warnings({ok, Text, _Warnings}) -> {ok, Text};
strip_warnings(Other) -> Other.
