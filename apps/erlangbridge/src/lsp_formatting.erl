-module(lsp_formatting).

-export([document_end/1, range/3, on_type/2]).

%% @doc Tasks 5.3/5.4. vscode_erlfmt already does the hard part for both:
%% format_string/2's own {range, {StartLine, EndLine}} option (plain 1-based
%% line numbers) auto-expands whatever range it is given to cover every
%% top-level form it intersects (format_enclosing_range/verify_ranges -
%% "pick the largest range, so all intersected forms are covered"), then
%% re-injects just that reformatted slice back into the whole document
%% (inject_range). So range formatting needs no enclosing-form lookup of
%% its own, and on-type formatting - by simply asking for the single
%% cursor line - gets a real, AST-aware reindent of its whole enclosing
%% form for free, guard sequences included (replacing the ~17 regex
%% onEnterRules in lib/extension.ts, whose line-ending-in-`;`
%% end-of-clause rule can't tell a guard continuation from a real clause
%% end - see that file's own FIXME).
%%
%% Both return a *scoped* edit (the common-prefix/common-suffix diff below),
%% not a whole-document replace - unlike textDocument/formatting, which has
%% always replaced the whole document and keeps doing so (see
%% lsp_handlers:textDocument_formatting/2 - task 5.3 only fixes its
%% previously-hardcoded result range, not that strategy).
%%
%% CHARACTERIZATION: like textDocument/formatting itself (see
%% lsp_format_SUITE's own note), if the file doesn't currently parse at
%% all, vscode_erlfmt:format_string/2 returns the input unchanged - no
%% error, just silently nothing to reindent yet. This is not a new
%% limitation introduced here.

-spec range(File :: file:filename(), StartLine :: pos_integer(), EndLine :: pos_integer()) -> [map()].
range(File, StartLine, EndLine) ->
    Contents = read_content(File),
    case format(Contents, {StartLine, EndLine}) of
        Contents -> [];
        NewContents -> diff_edits(Contents, NewContents)
    end.

-spec on_type(File :: file:filename(), Line :: pos_integer()) -> [map()].
on_type(File, Line) ->
    range(File, Line, Line).

format(Contents, Range) ->
    Options = [{print_width, gen_lsp_config_server:formatting_line_length()}, {range, Range}],
    case vscode_erlfmt:format_string(binary_to_list(Contents), Options) of
        {ok, Updated, _} -> unicode:characters_to_binary(Updated);
        {ok, Updated} -> unicode:characters_to_binary(Updated);
        _ -> Contents
    end.

%% @doc 1-based {Line, Column} just past the last character of Content,
%% for a whole-document formatting result's real range (task 5.3 - this
%% replaces the previously-hardcoded {999999, 255} sentinel).
-spec document_end(Content :: binary()) -> {pos_integer(), pos_integer()}.
document_end(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    LastLine = lists:last(Lines),
    {length(Lines), byte_size(LastLine) + 1}.

%% Line-based common-prefix/common-suffix diff (the same trick task 3.2's
%% semantic-tokens delta uses, at line-of-text granularity instead of
%% 5-uint32 token groups): finds the smallest single edit that turns Old
%% into New, so a one-line reindent produces a one-line edit, not a
%% whole-document replace.
diff_edits(Old, New) ->
    OldLines = binary:split(Old, <<"\n">>, [global]),
    NewLines = binary:split(New, <<"\n">>, [global]),
    {PrefixLen, OldAfterPrefix, NewAfterPrefix} = common_prefix(OldLines, NewLines, 0),
    {_SuffixLen, OldMiddle, NewMiddle} = common_suffix(OldAfterPrefix, NewAfterPrefix),
    case {OldMiddle, NewMiddle} of
        {[], []} ->
            [];
        _ ->
            StartLine = PrefixLen,
            EndLine = PrefixLen + length(OldMiddle),
            NewText = case NewMiddle of
                [] -> <<>>;
                _ -> <<(iolist_to_binary(lists:join(<<"\n">>, NewMiddle)))/binary, "\n">>
            end,
            [#{
                range => #{
                    <<"start">> => #{line => StartLine, character => 0},
                    <<"end">> => #{line => EndLine, character => 0}
                },
                newText => NewText
            }]
    end.

common_prefix([H | T1], [H | T2], N) -> common_prefix(T1, T2, N + 1);
common_prefix(L1, L2, N) -> {N, L1, L2}.

common_suffix(L1, L2) ->
    {N, RevRest1, RevRest2} = common_prefix(lists:reverse(L1), lists:reverse(L2), 0),
    {N, lists:reverse(RevRest1), lists:reverse(RevRest2)}.

read_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, Bin} = file:read_file(File),
            Bin;
        Bin ->
            Bin
    end.
