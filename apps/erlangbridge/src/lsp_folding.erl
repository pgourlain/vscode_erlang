-module(lsp_folding).

-export([folding_ranges/1]).

-define(COMMENT, <<"comment">>).
-define(REGION, <<"region">>).

%% @doc Task 5.1. Returns {StartLine, EndLine, Kind} triples (1-based,
%% Kind = comment | region | undefined for a plain code fold) covering:
%% every function (all its clauses together) and every one of its
%% clauses individually; every `case`/`if`/`receive`/`try`/`begin`/`fun`
%% block (depth-tracked the same way lsp_codeaction.erl's if<->case
%% conversion finds a construct's own matching `end`); a multi-line
%% `-export(...)` (or any other attribute whose own list/tuple spans more
%% than one line); a run of two or more consecutive comment lines; and
%% `%% region` / `%% endregion` marker pairs.
-spec folding_ranges(File :: file:filename()) -> [{pos_integer(), pos_integer(), binary() | undefined}].
folding_ranges(File) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    case is_list(Tree) of
        true ->
            Content = read_content(File),
            {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
            function_and_clause_ranges(Tree, Tokens) ++
            block_ranges(Content) ++
            attribute_ranges(Content, Tree) ++
            comment_ranges(Content) ++
            region_ranges(Content);
        false ->
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functions and clauses                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CHARACTERIZATION: a clause's real last line is found by scanning its
%% own *tokens* (from the clause's own start position through whichever
%% comes first at depth 0: the `;` before the next clause, or the form's
%% own terminating `.`), not by walking the AST and taking the largest
%% line any node happens to carry - the latter misses a `case`/`if`/
%% `receive`/`try`/`begin`/`fun` block whose own closing `end` sits alone
%% on the line after the last real expression, which is a common Erlang
%% formatting style (see block_ranges/1's own matching-`end` scan, which
%% this reuses the same depth-tracking idea from).
function_and_clause_ranges(Tree, Tokens) ->
    lists:flatmap(fun
        ({function, {StartLine, _}, _Name, _Arity, Clauses}) ->
            ClauseRanges = [{CLine, clause_end_line(Tokens, {CLine, CCol}), undefined}
                             || {clause, {CLine, CCol}, _, _, _} <- Clauses],
            EndLine = lists:max([EL || {_, EL, _} <- ClauseRanges]),
            WholeFunction = case length(Clauses) > 1 of
                true -> [{StartLine, EndLine, undefined}];
                false -> []
            end,
            WholeFunction ++ [R || {SL, EL, _} = R <- ClauseRanges, EL > SL];
        (_) ->
            []
    end, Tree).

clause_end_line(Tokens, {StartLine, StartCol}) ->
    Relevant = lists:dropwhile(fun (T) -> token_pos(T) < {StartLine, StartCol} end, Tokens),
    scan_clause_end(Relevant, 0, StartLine).

scan_clause_end([{Kind, {L, _}} | Rest], Depth, _Last)
        when Kind =:= 'if'; Kind =:= 'case'; Kind =:= 'receive'; Kind =:= 'begin'; Kind =:= 'try'; Kind =:= 'fun' ->
    scan_clause_end(Rest, Depth + 1, L);
scan_clause_end([{'end', {L, _}} | Rest], Depth, _Last) when Depth > 0 ->
    scan_clause_end(Rest, Depth - 1, L);
scan_clause_end([{Sep, {L, _}} | _Rest], 0, _Last) when Sep =:= ';'; Sep =:= dot ->
    L;
scan_clause_end([Tok | Rest], Depth, _Last) ->
    scan_clause_end(Rest, Depth, token_line(Tok));
scan_clause_end([], _Depth, Last) ->
    Last.

token_pos({_Type, Pos}) -> Pos;
token_pos({_Type, Pos, _Value}) -> Pos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case/if/receive/try/begin/fun blocks                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block_ranges(Content) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    block_ranges(Tokens, []).

block_ranges([{Kind, {StartLine, _}} | _] = Tokens, Acc)
        when Kind =:= 'if'; Kind =:= 'case'; Kind =:= 'receive'; Kind =:= 'begin'; Kind =:= 'try'; Kind =:= 'fun' ->
    case find_matching_end(Tokens) of
        {ok, EndLine, Rest} when EndLine > StartLine ->
            block_ranges(Rest, [{StartLine, EndLine, undefined} | Acc]);
        {ok, _EndLine, Rest} ->
            block_ranges(Rest, Acc);
        none ->
            lists:reverse(Acc)
    end;
block_ranges([_ | Rest], Acc) ->
    block_ranges(Rest, Acc);
block_ranges([], Acc) ->
    lists:reverse(Acc).

%% Tokens starts *at* the opener itself - depth 0 becomes 1 immediately,
%% matching lsp_codeaction:find_matching_end/2's own convention.
find_matching_end(Tokens) ->
    find_matching_end(Tokens, 0).

find_matching_end([{Kind, _} | Rest], Depth)
        when Kind =:= 'if'; Kind =:= 'case'; Kind =:= 'receive'; Kind =:= 'begin'; Kind =:= 'try'; Kind =:= 'fun' ->
    find_matching_end(Rest, Depth + 1);
find_matching_end([{'end', {Line, _}} | Rest], 1) ->
    {ok, Line, Rest};
find_matching_end([{'end', _} | Rest], Depth) ->
    find_matching_end(Rest, Depth - 1);
find_matching_end([_ | Rest], Depth) ->
    find_matching_end(Rest, Depth);
find_matching_end([], _Depth) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% multi-line attributes (-export, -record, ...)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attribute_ranges(Content, Tree) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    lists:filtermap(fun
        ({attribute, {StartLine, _}, _Tag, _Payload}) ->
            case attribute_end_line(Tokens, StartLine) of
                EndLine when EndLine > StartLine -> {true, {StartLine, EndLine, undefined}};
                _ -> false
            end;
        (_) ->
            false
    end, Tree).

attribute_end_line(Tokens, StartLine) ->
    Relevant = lists:dropwhile(fun (T) -> token_line(T) < StartLine end, Tokens),
    FormTokens = lists:takewhile(fun (T) -> element(1, T) =/= dot end, Relevant),
    case FormTokens of
        [] -> StartLine;
        _ -> lists:max([token_line(T) || T <- FormTokens])
    end.

token_line({_Type, {L, _C}}) -> L;
token_line({_Type, {L, _C}, _Value}) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comment blocks and %% region / %% endregion markers      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comment_ranges(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    IndexedLines = lists:zip(lists:seq(1, length(Lines)), Lines),
    group_comment_runs(IndexedLines).

group_comment_runs([]) ->
    [];
group_comment_runs([{LineNum, Line} | Rest]) ->
    case is_comment_line(Line) of
        true ->
            {RunEnd, Remaining} = take_comment_run(Rest, LineNum),
            Range = case RunEnd > LineNum of
                true -> [{LineNum, RunEnd, ?COMMENT}];
                false -> []
            end,
            Range ++ group_comment_runs(Remaining);
        false ->
            group_comment_runs(Rest)
    end.

take_comment_run([{LineNum, Line} | Rest], _Last) ->
    case is_comment_line(Line) of
        true -> take_comment_run(Rest, LineNum);
        false -> {LineNum - 1, [{LineNum, Line} | Rest]}
    end;
take_comment_run([], Last) ->
    {Last, []}.

is_comment_line(Line) ->
    case re:run(Line, <<"^\\s*%">>) of
        {match, _} -> true;
        nomatch -> false
    end.

region_ranges(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    IndexedLines = lists:zip(lists:seq(1, length(Lines)), Lines),
    Starts = [LineNum || {LineNum, Line} <- IndexedLines, is_region_marker(Line, <<"region">>)],
    Ends = [LineNum || {LineNum, Line} <- IndexedLines, is_region_marker(Line, <<"endregion">>)],
    pair_regions(Starts, Ends).

is_region_marker(Line, Marker) ->
    case re:run(Line, <<"^\\s*%%\\s*", Marker/binary, "\\b">>) of
        {match, _} -> true;
        nomatch -> false
    end.

%% Pairs each region start with the nearest unused endregion after it -
%% not a full nesting-aware parser, but correct for the common case of
%% non-overlapping (even if nested) region/endregion pairs in source order.
pair_regions([], _Ends) ->
    [];
pair_regions([Start | Rest], Ends) ->
    case lists:filter(fun (End) -> End > Start end, Ends) of
        [] -> pair_regions(Rest, Ends);
        Candidates ->
            EndLine = lists:min(Candidates),
            [{Start, EndLine, ?REGION} | pair_regions(Rest, lists:delete(EndLine, Ends))]
    end.

read_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, Bin} = file:read_file(File),
            Bin;
        Bin ->
            Bin
    end.
