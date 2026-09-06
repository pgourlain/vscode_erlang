-module(lsp_codeaction).

-export([code_actions/3, resolve/1]).

-include("lsp_log.hrl").

%% @doc Task 2.2: builds one quick fix per diagnostic whose `data` carries
%% correlation_data (task 2.1: lsp_syntax:correlation_data/1), by pattern
%% matching directly on the raw erl_lint/epp message body - no re-analysis
%% of the file is needed to know *what* is wrong, only to locate where an
%% edit (e.g. an -export or -record's closing bracket) needs to land.
%%
%% Diagnostics arrive here exactly as the client echoes them back (they
%% were themselves produced by lsp_handlers:send_diagnostics/3, then
%% round-tripped through JSON), so atoms from the original correlation_data
%% (module names, erl_lint message-body tags) are now binaries.
%%
%% The edit is always computed eagerly here rather than deferred to
%% resolve/1 - every fix below is cheap (no re-parsing), so there is
%% nothing to gain from the laziness resolveProvider => true allows for.
%%
%% Task 2.3 adds a second, independent source of actions: ones offered from
%% the cursor's own position (Range) regardless of any diagnostic - adding/
%% removing a function from -export, and generating a -spec from its
%% inferred clause heads.
%%
%% Task 2.4 adds a third source: "Implement missing callbacks" for a
%% -behaviour(X) that is missing one or more mandatory callbacks. Unlike
%% every fix above, this one is not one-action-per-diagnostic - erl_lint
%% already emits a separate undefined_behaviour_func diagnostic per missing
%% callback (all pointing at the same -behaviour(...) line), so
%% actions_for_behaviours/2 groups every diagnostic that shares the same
%% behaviour module into a single bulk action that stubs all of them at
%% once. There is deliberately no separate query of Module's full callback
%% list (via behaviour_info/1 or EEP-48 docs): erl_lint has already computed
%% exactly which ones are missing, for both OTP and project-local behaviours
%% alike (lsp_syntax:validate_parsed_source_file/1 loads project-local
%% behaviour modules before linting).
-spec code_actions(File :: file:filename(), Range :: term(), Context :: map()) -> [map()].
code_actions(File, Range, Context) ->
    Diagnostics = maps:get(diagnostics, Context, []),
    DiagnosticActions = lists:flatmap(fun (Diagnostic) -> actions_for_diagnostic(File, Diagnostic) end, Diagnostics),
    DiagnosticActions ++ actions_for_behaviours(File, Diagnostics) ++ actions_for_cursor(File, Range).

%% @doc Identity: no fix defers any work to resolve/1 (see code_actions/3).
-spec resolve(CodeAction :: map()) -> map().
resolve(CodeAction) ->
    CodeAction.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dispatch, one per fix  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actions_for_diagnostic(File, #{data := #{module := Module, messageBody := MessageBody}} = Diagnostic) ->
    fix(File, Diagnostic, Module, MessageBody);
actions_for_diagnostic(_File, _Diagnostic) ->
    [].

%% unused variable -> prefix with _
%% The diagnostic's own range already points at the variable's exact
%% occurrence (erl_lint reports the unused binding's own position), so no
%% lookup is needed at all beyond the diagnostic itself.
fix(File, #{range := #{start := #{line := Line, character := Character}}} = Diagnostic,
        <<"erl_lint">>, [<<"unused_var">>, VarName]) ->
    NewText = <<"_", VarName/binary>>,
    EndChar = Character + byte_size(VarName),
    Edit = lsp_rename:build_workspace_edit([{File, Line + 1, Character + 1, EndChar + 1, NewText}]),
    [action(<<"Prefix unused variable with _">>, Diagnostic, Edit)];

%% unused function -> add it to the file's -export list
fix(File, Diagnostic, <<"erl_lint">>, [<<"unused_function">>, [Name, Arity]]) ->
    case find_attribute_insertion_point(File, export, fun (_) -> true end, $]) of
        {ok, {Line, Col}} ->
            InsertText = iolist_to_binary(io_lib:format(", ~s/~p", [Name, Arity])),
            Edit = lsp_rename:build_workspace_edit([{File, Line, Col, Col, InsertText}]),
            [action(export_title(Name, Arity), Diagnostic, Edit)];
        undefined ->
            []
    end;

%% undefined function -> create a stub clause at the end of the file
fix(File, Diagnostic, <<"erl_lint">>, [<<"undefined_function">>, [Name, Arity]]) ->
    {Line, Col} = end_of_file_insertion_point(File),
    StubText = stub_function_text(Name, Arity),
    Edit = lsp_rename:build_workspace_edit([{File, Line, Col, Col, StubText}]),
    [action(<<"Create stub for ", Name/binary, "/", (integer_to_binary(Arity))/binary>>, Diagnostic, Edit)];

%% unbound/undefined record field -> add the missing field to the record's
%% own -record(...) definition
fix(File, Diagnostic, <<"erl_lint">>, [<<"undefined_field">>, RecordName, FieldName]) ->
    RecordAtom = binary_to_atom(RecordName, utf8),
    MatchesRecord = fun ({Name, _Fields}) -> Name =:= RecordAtom end,
    case find_attribute_insertion_point(File, record, MatchesRecord, $}) of
        {ok, {Line, Col}} ->
            InsertText = iolist_to_binary(io_lib:format(", ~s", [FieldName])),
            Edit = lsp_rename:build_workspace_edit([{File, Line, Col, Col, InsertText}]),
            [action(<<"Add field ", FieldName/binary, " to record #", RecordName/binary>>, Diagnostic, Edit)];
        undefined ->
            []
    end;

%% missing -include/-include_lib: the target file doesn't exist, so the
%% only generally-safe fix is to remove the broken line entirely (there is
%% nothing to point the include at instead).
fix(File, #{range := #{start := #{line := Line}}} = Diagnostic,
        <<"epp">>, [<<"include">>, <<"file">>, MissingFile]) ->
    Edit = lsp_rename:build_workspace_edit([{File, Line + 1, 1, Line + 2, 1, <<>>}]),
    [action(<<"Remove include of missing file \"", MissingFile/binary, "\"">>, Diagnostic, Edit)];

%% no -module(...) attribute at all: unlike every fix above, erl_lint's own
%% messageBody here is a bare atom (undefined_module), not a tagged tuple -
%% there is nothing else to inspect, the fix is always the same. The name
%% is inferred from the file's own basename, matching the Erlang convention
%% that a module's name always matches its filename.
fix(File, Diagnostic, <<"erl_lint">>, <<"undefined_module">>) ->
    ModuleName = module_name_for_file(File),
    InsertText = iolist_to_binary(io_lib:format("-module(~s).~n", [ModuleName])),
    Edit = lsp_rename:build_workspace_edit([{File, 1, 1, 1, 1, InsertText}]),
    [action(<<"Add -module(", ModuleName/binary, ")">>, Diagnostic, Edit)];

fix(_File, _Diagnostic, _Module, _MessageBody) ->
    [].

module_name_for_file(File) ->
    unicode:characters_to_binary(filename:basename(File, ".erl")).

action(Title, Diagnostic, Edit) ->
    #{
        title => Title,
        kind => <<"quickfix">>,
        diagnostics => [Diagnostic],
        edit => Edit
    }.

export_title(Name, Arity) ->
    <<"Export ", Name/binary, "/", (integer_to_binary(Arity))/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 2.4: behaviour "implement missing callbacks" %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actions_for_behaviours(File, Diagnostics) ->
    [implement_callbacks_action(File, Behaviour, FuncsAndDiags)
     || {Behaviour, FuncsAndDiags} <- group_missing_callbacks(Diagnostics)].

%% Groups every undefined_behaviour_func diagnostic by its Behaviour module,
%% collecting the missing {Name, Arity} callback and the diagnostic itself
%% for each. Funcs/Diags each come out in original diagnostic order (the
%% fold prepends, so the final maps:to_list/lists:reverse pairing undoes
%% that).
group_missing_callbacks(Diagnostics) ->
    Entries = lists:filtermap(fun missing_callback_entry/1, Diagnostics),
    Grouped = lists:foldl(fun ({Behaviour, Func, Diagnostic}, Acc) ->
        {Funcs, Diags} = maps:get(Behaviour, Acc, {[], []}),
        Acc#{Behaviour => {[Func | Funcs], [Diagnostic | Diags]}}
    end, #{}, Entries),
    [{Behaviour, {lists:reverse(Funcs), lists:reverse(Diags)}}
     || {Behaviour, {Funcs, Diags}} <- maps:to_list(Grouped)].

missing_callback_entry(#{data := #{module := <<"erl_lint">>,
                                    messageBody := [<<"undefined_behaviour_func">>, [Name, Arity], Behaviour]}} = Diagnostic) ->
    {true, {Behaviour, {binary_to_atom(Name, utf8), Arity}, Diagnostic}};
missing_callback_entry(_) ->
    false.

implement_callbacks_action(File, Behaviour, {Funcs, Diags}) ->
    {Line, Col} = end_of_file_insertion_point(File),
    StubsText = iolist_to_binary([stub_function_text(Name, Arity) || {Name, Arity} <- Funcs]),
    Edit = lsp_rename:build_workspace_edit([{File, Line, Col, Line, Col, StubsText}]),
    #{
        title => <<"Implement missing callbacks for ", Behaviour/binary>>,
        kind => <<"quickfix">>,
        diagnostics => Diags,
        edit => Edit
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 2.3: cursor-based export/spec actions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Neither of these needs a diagnostic at all - just knowing which function
%% the cursor is currently inside (find_function_with_line/2's own
%% "last function whose start line is <= cursor line" heuristic, same one
%% inlinevalues/hover already rely on elsewhere in this codebase).
actions_for_cursor(File, #{start := #{line := Line0}} = Range) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    FunctionActions = case is_list(Tree) andalso lsp_navigation:find_function_with_line(Tree, Line0 + 1) of
        {function, Pos, Name, Arity, Clauses} ->
            export_toggle_actions(File, Tree, Pos, Name, Arity) ++
            generate_spec_actions(File, Tree, Pos, Name, Arity, Clauses) ++
            inline_variable_actions(File, Clauses, Line0) ++
            extract_function_actions(File, Tree, Clauses, Range);
        _ ->
            []
    end,
    FunctionActions ++ if_case_actions(File, Line0) ++ organize_export_actions(File, Tree, Line0);
actions_for_cursor(_File, _Range) ->
    [].

%% Offers exactly one of "export this" / "remove from export", never both.
export_toggle_actions(File, Tree, _Pos, Name, Arity) ->
    NameBin = atom_to_binary(Name, utf8),
    case is_exported(Tree, Name, Arity) of
        true -> remove_from_export_action(File, Tree, NameBin, Name, Arity);
        false -> add_to_export_action(File, Tree, NameBin, Name, Arity)
    end.

is_exported(Tree, Name, Arity) ->
    lists:any(fun
        ({attribute, _, export, Exports}) -> lists:member({Name, Arity}, Exports);
        (_) -> false
    end, Tree).

add_to_export_action(File, Tree, NameBin, Name, Arity) ->
    case find_attribute_insertion_point(File, export, fun (_) -> true end, $]) of
        {ok, {Line, Col}} ->
            InsertText = iolist_to_binary(io_lib:format(", ~s/~p", [Name, Arity])),
            Edit = lsp_rename:build_workspace_edit([{File, Line, Col, Col, InsertText}]),
            [source_action(export_title(NameBin, Arity), <<"refactor">>, Edit)];
        undefined ->
            %% no -export attribute exists at all yet: add a brand new one
            %% right after -module(...).
            case module_attribute_line(Tree) of
                {ok, Line} ->
                    InsertText = iolist_to_binary(io_lib:format("-export([~s/~p]).~n", [Name, Arity])),
                    Edit = lsp_rename:build_workspace_edit([{File, Line + 1, 1, Line + 1, 1, InsertText}]),
                    [source_action(export_title(NameBin, Arity), <<"refactor">>, Edit)];
                undefined ->
                    []
            end
    end.

module_attribute_line(Tree) ->
    case [Line || {attribute, {Line, _}, module, _} <- Tree] of
        [Line | _] -> {ok, Line};
        [] -> undefined
    end.

%% CHARACTERIZATION: removes exactly one neighbouring comma (whichever
%% side has one) along with the Name/Arity entry itself, so the export
%% list stays syntactically valid regardless of where in the list the
%% entry sits - it does not attempt to also tidy up surrounding whitespace
%% or reformat the remaining entries.
remove_from_export_action(File, Tree, NameBin, Name, Arity) ->
    case [Pos || {attribute, Pos, export, Exports} <- Tree, lists:member({Name, Arity}, Exports)] of
        [Pos | _] ->
            Content = read_content(File),
            case find_export_removal_span(Content, Pos, Name, Arity) of
                {ok, {StartLine, StartCol}, {EndLine, EndCol}} ->
                    Edit = lsp_rename:build_workspace_edit([{File, StartLine, StartCol, EndLine, EndCol, <<>>}]),
                    [source_action(<<"Remove ", NameBin/binary, "/", (integer_to_binary(Arity))/binary,
                                     " from export">>, <<"refactor">>, Edit)];
                not_found ->
                    []
            end;
        [] ->
            []
    end.

%% Scans the real tokens of the specific -export(...) form starting at Pos
%% for the Name/Arity entry, and returns the [Start, End) span to delete -
%% End is always the *start* of whatever token follows the removed span,
%% so there is never a need to compute an individual token's own width.
find_export_removal_span(Content, Pos, Name, Arity) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    RelevantTokens = lists:dropwhile(fun (T) -> token_pos(T) < Pos end, Tokens),
    FormTokens = lists:takewhile(fun (T) -> element(1, T) =/= dot end, RelevantTokens),
    scan_for_export_entry(Name, Arity, undefined, FormTokens).

scan_for_export_entry(Name, Arity, PrevToken,
        [{atom, NamePos, Name}, {'/', _}, {integer, _, Arity} | Rest]) ->
    NextPos = case Rest of [Next | _] -> token_pos(Next); [] -> NamePos end,
    case {PrevToken, Rest} of
        {{',', PrevPos}, _} ->
            {ok, PrevPos, NextPos};
        {_, [{',', _} | AfterComma]} ->
            NextAfterComma = case AfterComma of [N2 | _] -> token_pos(N2); [] -> NextPos end,
            {ok, NamePos, NextAfterComma};
        _ ->
            {ok, NamePos, NextPos}
    end;
scan_for_export_entry(Name, Arity, _Prev, [Tok | Rest]) ->
    scan_for_export_entry(Name, Arity, Tok, Rest);
scan_for_export_entry(_Name, _Arity, _Prev, []) ->
    not_found.

%% Generate a -spec from the function's own clause heads: reuses
%% lsp_navigation's find_function_with_line/2 result (already the same
%% {function, Pos, Name, Arity, Clauses} shape function_clauses/3 would
%% give for this one function) and lsp_inlayhints:extract_function_args/1
%% (the exact "pick the most informative arg name across every clause"
%% logic inlay hints already rely on) - not offered again if a -spec for
%% this Name/Arity already exists.
generate_spec_actions(File, Tree, {Line, _Col}, Name, Arity, Clauses) ->
    case has_spec(Tree, Name, Arity) of
        true ->
            [];
        false ->
            Args = lsp_inlayhints:extract_function_args(Clauses),
            ArgsText = spec_args_text(Args),
            NameBin = atom_to_binary(Name, utf8),
            SpecText = iolist_to_binary(
                io_lib:format("-spec ~s(~s) -> term().~n", [Name, ArgsText])),
            Edit = lsp_rename:build_workspace_edit([{File, Line, 1, Line, 1, SpecText}]),
            [source_action(<<"Generate -spec for ", NameBin/binary, "/",
                             (integer_to_binary(Arity))/binary>>, <<"source">>, Edit)]
    end.

has_spec(Tree, Name, Arity) ->
    lists:any(fun
        ({attribute, _, spec, {{SpecName, SpecArity}, _}}) -> SpecName =:= Name andalso SpecArity =:= Arity;
        (_) -> false
    end, Tree).

spec_args_text(Args) ->
    Indexed = lists:zip(lists:seq(1, length(Args)), Args),
    Named = [iolist_to_binary(io_lib:format("~s :: term()", [spec_arg_name(Arg, Index)])) || {Index, Arg} <- Indexed],
    iolist_to_binary(lists:join(<<", ">>, Named)).

spec_arg_name({var, _, '_'}, Index) -> arg_placeholder_name(Index);
spec_arg_name({var, _, Name}, _Index) -> atom_to_list(Name);
spec_arg_name(_Other, Index) -> arg_placeholder_name(Index).

arg_placeholder_name(Index) -> io_lib:format("Arg~p", [Index]).

source_action(Title, Kind, Edit) ->
    #{
        title => Title,
        kind => Kind,
        edit => Edit
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 2.5 (minimal scope): inline variable, if<->case %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CHARACTERIZATION (deliberately minimal, see tasks.md 2.5): only offered
%% when the bound variable is referenced exactly once anywhere else in its
%% clause. This is not a general-purpose limitation for cosmetic reasons -
%% it is what keeps this safe without any purity/side-effect analysis of
%% the right-hand side: inlining into exactly one call site can never
%% change how many times that expression is evaluated. Two or more usages,
%% zero usages (dead binding), a non-variable (pattern) left-hand side, or
%% the binding being the clause's own last statement (nothing left to
%% shift into) all simply offer no action.
inline_variable_actions(File, Clauses, Line0) ->
    case find_binding_at_line(Clauses, Line0 + 1) of
        {ok, Clause, Name, MatchPos, Rhs, NextPos} ->
            inline_variable_action(File, Clause, Name, MatchPos, Rhs, NextPos);
        undefined ->
            []
    end.

find_binding_at_line([], _CursorLine) ->
    undefined;
find_binding_at_line([{clause, _, _, _, Body} = Clause | Rest], CursorLine) ->
    case find_binding_in_body(Body, CursorLine) of
        undefined -> find_binding_at_line(Rest, CursorLine);
        {Name, MatchPos, Rhs, NextPos} -> {ok, Clause, Name, MatchPos, Rhs, NextPos}
    end.

%% Needs a statement right after it (element(2, Next) is valid for every
%% erl_parse abstract-format node, since annotation/position always sits
%% in the 2nd tuple element) to shift the removal's end into - a match on
%% the clause's own last statement is left alone.
find_binding_in_body([{match, {Line, _} = Pos, {var, _, Name}, Rhs}, Next | _], CursorLine)
        when Line =:= CursorLine, Name =/= '_' ->
    {Name, Pos, Rhs, element(2, Next)};
find_binding_in_body([_ | Rest], CursorLine) ->
    find_binding_in_body(Rest, CursorLine);
find_binding_in_body([], _CursorLine) ->
    undefined.

inline_variable_action(File, Clause, Name, MatchPos, Rhs, {NextLine, NextCol}) ->
    AllPositions = lists:usort(erl_syntax_lib:fold(fun
        ({var, Pos, N}, Acc) when N =:= Name -> [Pos | Acc];
        (_, Acc) -> Acc
    end, [], Clause)),
    case lists:delete(MatchPos, AllPositions) of
        [{UsageLine, UsageCol}] ->
            {MatchLine, MatchCol} = MatchPos,
            RhsText = iolist_to_binary(io_lib:format("(~ts)", [erl_pp:expr(Rhs)])),
            NameLen = length(atom_to_list(Name)),
            %% Order matters: the usage (further down the file) is applied
            %% first, so its positions are still the original ones; only
            %% then is the earlier binding line removed - an edit never
            %% shifts anything positioned before it.
            UsageEdit = {File, UsageLine, UsageCol, UsageLine, UsageCol + NameLen, RhsText},
            RemovalEdit = {File, MatchLine, MatchCol, NextLine, NextCol, <<>>},
            Edit = lsp_rename:build_workspace_edit([UsageEdit, RemovalEdit]),
            [#{
                title => <<"Inline variable ", (atom_to_binary(Name, utf8))/binary>>,
                kind => <<"refactor">>,
                edit => Edit
            }];
        _ ->
            []
    end.

%% if -> case is always offered (case true of _ when G -> B end is always a
%% valid, side-effect-free rewrite of an if). case -> if is only offered
%% when it is equally safe: the switched expression is a bare variable
%% reference (no side effect can be dropped) and every clause pattern is
%% already an unconditional wildcard (so no pattern-match semantics are
%% lost by moving to a guard-only construct) - a general case with real
%% patterns has no if-equivalent at all.
if_case_actions(File, Line0) ->
    CursorLine = Line0 + 1,
    case find_if_or_case_at_line(File, CursorLine) of
        {if_node, Pos, Clauses} -> convert_if_to_case_action(File, Pos, Clauses);
        {case_node, Pos, Expr, Clauses} -> convert_case_to_if_action(File, Pos, Expr, Clauses);
        undefined -> []
    end.

find_if_or_case_at_line(File, CursorLine) ->
    Matches = lsp_syntax:fold_in_syntax_tree(fun
        ({'if', {Line, _} = Pos, Clauses}, _CurFile, Acc) when Line =:= CursorLine ->
            [{if_node, Pos, Clauses} | Acc];
        ({'case', {Line, _} = Pos, Expr, Clauses}, _CurFile, Acc) when Line =:= CursorLine ->
            [{case_node, Pos, Expr, Clauses} | Acc];
        (_, _CurFile, Acc) ->
            Acc
    end, [], File),
    case Matches of
        [Node | _] -> Node;
        [] -> undefined
    end.

convert_if_to_case_action(File, Pos, Clauses) ->
    NewClauses = [{clause, CPos, [{var, CPos, '_'}], Guards, Body}
                  || {clause, CPos, [], Guards, Body} <- Clauses],
    NewNode = {'case', Pos, {atom, Pos, true}, NewClauses},
    [convert_action(File, Pos, <<"Convert if to case">>, NewNode)].

convert_case_to_if_action(File, Pos, {var, _, _}, Clauses) ->
    case lists:all(fun ({clause, _, [{var, _, '_'}], _, _}) -> true; (_) -> false end, Clauses) of
        true ->
            NewClauses = [{clause, CPos, [], guards_or_true(Guards, CPos), Body}
                          || {clause, CPos, [{var, _, '_'}], Guards, Body} <- Clauses],
            NewNode = {'if', Pos, NewClauses},
            [convert_action(File, Pos, <<"Convert case to if">>, NewNode)];
        false ->
            []
    end;
convert_case_to_if_action(_File, _Pos, _Expr, _Clauses) ->
    [].

guards_or_true([], Pos) -> [[{atom, Pos, true}]];
guards_or_true(Guards, _Pos) -> Guards.

convert_action(File, {Line, Col} = Pos, Title, NewNode) ->
    Content = read_content(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    {EndLine, EndCol} = find_matching_end(Tokens, Pos),
    PrintedText = reindent(erl_pp:expr(NewNode), Col),
    Edit = lsp_rename:build_workspace_edit([{File, Line, Col, EndLine, EndCol + 3, PrintedText}]),
    #{title => Title, kind => <<"refactor">>, edit => Edit}.

%% Depth-tracks every construct that closes with a bare `end` keyword
%% (if/case/receive/begin/try/fun all do - a `fun M:F/A` shorthand never
%% contains an `end` of its own, so counting every `fun` as an opener stays
%% balanced regardless of which form it is) from the node's own start
%% position, to find the specific `end` that matches it - not just the
%% first `end` token encountered, which could belong to a nested construct.
find_matching_end(Tokens, StartPos) ->
    Relevant = lists:dropwhile(fun (T) -> token_pos(T) < StartPos end, Tokens),
    scan_matching_end(Relevant, 0).

scan_matching_end([{Kind, _} | Rest], Depth)
        when Kind =:= 'if'; Kind =:= 'case'; Kind =:= 'receive'; Kind =:= 'begin';
             Kind =:= 'try'; Kind =:= 'fun' ->
    scan_matching_end(Rest, Depth + 1);
scan_matching_end([{'end', Pos} | _Rest], 1) ->
    Pos;
scan_matching_end([{'end', _} | Rest], Depth) ->
    scan_matching_end(Rest, Depth - 1);
scan_matching_end([_ | Rest], Depth) ->
    scan_matching_end(Rest, Depth).

%% erl_pp always prints flush-left from column 0 - fine for the first line
%% (the edit's own start column already places it correctly), but every
%% following line needs the original construct's own indentation added
%% back, or it visually collapses to the left margin.
reindent(Text, Col) ->
    Indent = binary:copy(<<" ">>, Col - 1),
    [First | Rest] = binary:split(unicode:characters_to_binary(Text), <<"\n">>, [global]),
    iolist_to_binary(lists:join(<<"\n">>, [First | [<<Indent/binary, L/binary>> || L <- Rest]])).

%% CHARACTERIZATION (deliberately minimal, see tasks.md 2.5): only offered
%% for a selection of one or more *complete* top-level statements from a
%% function clause's body, with at least one statement still left after it
%% in the same clause (that following statement's own start position is
%% what both the removal and the new function body's own text are bounded
%% by - extracting a clause's own trailing statement is not supported, nor
%% is a selection spanning more than one clause). Free variables use a
%% deliberately coarse heuristic: any variable referenced in the selection
%% that also appears anywhere in the clause's head patterns or an earlier
%% statement becomes an argument - no attempt is made to tell a "bound"
%% occurrence from a merely-referenced one there, which only ever widens
%% the argument list, never narrows away a real dependency. If a variable
%% *bound inside* the selection (via a top-level `=` only - a binding
%% introduced by a nested case/if/receive clause is not tracked) is
%% referenced again afterward, no action is offered at all, since the
%% extracted function would need to return more than its own trailing
%% value to support that. The extracted body is copied verbatim from the
%% source (not reformatted/reindented) and always named extracted_N, for
%% the lowest N not already used as a function name in the file.
extract_function_actions(File, Tree, Clauses, #{'end' := #{line := EndLine0}} = Range) ->
    #{start := #{line := StartLine0}} = Range,
    case find_selected_statements(Clauses, StartLine0 + 1, EndLine0 + 1) of
        {ok, Clause, Before, Selected, After} ->
            build_extract_action(File, Tree, Clause, Before, Selected, After);
        undefined ->
            []
    end;
extract_function_actions(_File, _Tree, _Clauses, _Range) ->
    [].

find_selected_statements([], _StartLine, _EndLine) ->
    undefined;
find_selected_statements([{clause, _, _, _, Body} = Clause | Rest], StartLine, EndLine) ->
    case split_selected(Body, StartLine, EndLine) of
        {ok, Before, Selected, After} -> {ok, Clause, Before, Selected, After};
        undefined -> find_selected_statements(Rest, StartLine, EndLine)
    end.

%% Body statements always appear in increasing source-position order, so
%% picking every statement whose own start line falls in [StartLine,
%% EndLine] always yields a single contiguous run - never one with a gap.
split_selected(Body, StartLine, EndLine) ->
    InRange = fun (Stmt) ->
        {Line, _} = element(2, Stmt),
        Line >= StartLine andalso Line =< EndLine
    end,
    {Before, AtAndAfter} = lists:splitwith(fun (Stmt) -> not InRange(Stmt) end, Body),
    {Selected, After} = lists:splitwith(InRange, AtAndAfter),
    case {Selected, After} of
        {[], _} -> undefined;
        {_, []} -> undefined;
        {_, _} -> {ok, Before, Selected, After}
    end.

%% The selection's own last statement is special-cased: if it binds a
%% variable that *is* referenced afterward, that is exactly the ordinary
%% "use the extracted computation's result" shape (the new function's own
%% implicit return value becomes that binding at the call site) - not a
%% multi-return situation. Any *other* bound variable still being
%% referenced afterward is the real disqualifying case.
build_extract_action(File, Tree, {clause, _, Patterns, _, _}, Before, Selected, [Next | _] = After) ->
    ReferencedInSelection = ordered_unique_var_names(Selected),
    BoundEarlier = sets:from_list(collect_var_names(Patterns) ++ collect_var_names(Before)),
    Args = [Name || Name <- ReferencedInSelection, sets:is_element(Name, BoundEarlier)],
    UsedAfter = sets:from_list(collect_var_names(After)),
    LastBoundVar = last_bound_var(lists:last(Selected)),
    ExcludedFromCheck = case LastBoundVar of {ok, N} -> [N]; undefined -> [] end,
    BoundInSelection = collect_bound_names(Selected) -- ExcludedFromCheck,
    case [Name || Name <- BoundInSelection, sets:is_element(Name, UsedAfter)] of
        [] ->
            ResultVar = case LastBoundVar of
                {ok, N2} ->
                    case sets:is_element(N2, UsedAfter) of true -> {ok, N2}; false -> undefined end;
                undefined ->
                    undefined
            end,
            build_extract_edit(File, Tree, Args, Selected, Next, ResultVar);
        [_ | _] ->
            []
    end.

last_bound_var({match, _, {var, _, Name}, _}) when Name =/= '_' -> {ok, Name};
last_bound_var(_) -> undefined.

collect_var_names(Nodes) when is_list(Nodes) ->
    lists:usort(lists:flatmap(fun collect_var_names/1, Nodes));
collect_var_names(Node) ->
    erl_syntax_lib:fold(fun
        ({var, _, '_'}, Acc) -> Acc;
        ({var, _, Name}, Acc) -> [Name | Acc];
        (_, Acc) -> Acc
    end, [], Node).

ordered_unique_var_names(Stmts) ->
    Names = lists:flatmap(fun ordered_var_names/1, Stmts),
    dedup_preserve_order(Names).

ordered_var_names(Node) ->
    lists:reverse(erl_syntax_lib:fold(fun
        ({var, _, '_'}, Acc) -> Acc;
        ({var, _, Name}, Acc) -> [Name | Acc];
        (_, Acc) -> Acc
    end, [], Node)).

dedup_preserve_order(Names) ->
    {Result, _Seen} = lists:foldl(fun (Name, {Acc, Seen}) ->
        case sets:is_element(Name, Seen) of
            true -> {Acc, Seen};
            false -> {[Name | Acc], sets:add_element(Name, Seen)}
        end
    end, {[], sets:new()}, Names),
    lists:reverse(Result).

%% Only a top-level `Var = ...` statement counts as a binding here -
%% deliberately shallow, see the CHARACTERIZATION comment above.
collect_bound_names(Stmts) ->
    lists:usort([Name || {match, _, {var, _, Name}, _} <- Stmts, Name =/= '_']).

build_extract_edit(File, Tree, Args, Selected, Next, ResultVar) ->
    NewName = fresh_function_name(Tree),
    NameBin = atom_to_binary(NewName, utf8),
    {FirstLine, FirstCol} = element(2, hd(Selected)),
    {NextLine, NextCol} = element(2, Next),
    Content = read_content(File),
    RawBody = text_between(Content, {FirstLine, FirstCol}, {NextLine, NextCol}),
    NewBodyText = terminate_with_dot(RawBody),
    ArgsText = iolist_to_binary(lists:join(<<", ">>, [atom_to_binary(A, utf8) || A <- Args])),
    NewFunctionText = iolist_to_binary(io_lib:format("~n~s(~s) ->~n    ~s~n", [NameBin, ArgsText, NewBodyText])),
    Indent = binary:copy(<<" ">>, NextCol - 1),
    CallText = case ResultVar of
        {ok, VarName} ->
            iolist_to_binary(io_lib:format("~s = ~s(~s),~n~s", [VarName, NameBin, ArgsText, Indent]));
        undefined ->
            iolist_to_binary(io_lib:format("~s(~s),~n~s", [NameBin, ArgsText, Indent]))
    end,
    {EofLine, EofCol} = end_of_file_insertion_point(File),
    CallEdit = {File, FirstLine, FirstCol, NextLine, NextCol, CallText},
    NewFunctionEdit = {File, EofLine, EofCol, EofLine, EofCol, NewFunctionText},
    %% Bottom-of-file edit first, then the (earlier) call-site edit - same
    %% "process edits bottom-to-top" ordering as inline_variable_action/6.
    Edit = lsp_rename:build_workspace_edit([NewFunctionEdit, CallEdit]),
    [#{title => <<"Extract function ", NameBin/binary>>, kind => <<"refactor">>, edit => Edit}].

terminate_with_dot(Text) ->
    Trimmed = string:trim(Text, trailing),
    case binary:last(Trimmed) of
        $, -> <<(binary:part(Trimmed, 0, byte_size(Trimmed) - 1))/binary, ".">>;
        $. -> Trimmed;
        _ -> <<Trimmed/binary, ".">>
    end.

fresh_function_name(Tree) ->
    Existing = sets:from_list([FName || {function, _, FName, _, _} <- Tree]),
    fresh_function_name(Existing, 1).

fresh_function_name(Existing, N) ->
    Candidate = list_to_atom("extracted_" ++ integer_to_list(N)),
    case sets:is_element(Candidate, Existing) of
        true -> fresh_function_name(Existing, N + 1);
        false -> Candidate
    end.

%% 1-based {Line, Column} pair (matching every other AST-derived position
%% in this module) -> the exact byte range of source text between them.
text_between(Content, {L1, C1}, {L2, C2}) ->
    Offset1 = pos_to_offset(Content, L1, C1),
    Offset2 = pos_to_offset(Content, L2, C2),
    binary:part(Content, Offset1, Offset2 - Offset1).

pos_to_offset(Content, Line, Col) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    {Before, _} = lists:split(Line - 1, Lines),
    LineStart = lists:foldl(fun (L, Acc) -> Acc + byte_size(L) + 1 end, 0, Before),
    LineStart + (Col - 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 2.6: source action - sort -export %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cursor on a specific -export(...) attribute's own line: offered only
%% when its entries are not already sorted (Name, then Arity - exactly
%% lists:sort/1's own term order on {Name, Arity} tuples), so a
%% already-tidy -export never shows a no-op action.
organize_export_actions(File, Tree, Line0) ->
    CursorLine = Line0 + 1,
    case [{Pos, Exports} || {attribute, {Line, _} = Pos, export, Exports} <- Tree, Line =:= CursorLine] of
        [{Pos, Exports} | _] ->
            Sorted = lists:sort(Exports),
            case Sorted =:= Exports of
                true -> [];
                false -> [organize_export_action(File, Pos, Sorted)]
            end;
        [] ->
            []
    end.

organize_export_action(File, Pos, Sorted) ->
    Content = read_content(File),
    {OpenLine, OpenCol} = find_opening_bracket_after(Content, Pos, $[),
    {CloseLine, CloseCol} = find_closing_bracket_before_dot(Content, Pos, $]),
    NewText = iolist_to_binary(lists:join(<<", ">>, [export_entry_text(N, A) || {N, A} <- Sorted])),
    Edit = lsp_rename:build_workspace_edit([{File, OpenLine, OpenCol + 1, CloseLine, CloseCol, NewText}]),
    #{title => <<"Sort -export list">>, kind => <<"source">>, edit => Edit}.

export_entry_text(Name, Arity) ->
    iolist_to_binary(io_lib:format("~s/~p", [Name, Arity])).

%% Mirrors find_closing_bracket_before_dot/3, but for the *first* token
%% matching OpenChar instead of the last one before the form's dot.
find_opening_bracket_after(Content, {StartLine, StartCol}, OpenChar) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    RelevantTokens = lists:dropwhile(fun (T) -> token_pos(T) < {StartLine, StartCol} end, Tokens),
    FormTokens = lists:takewhile(fun (T) -> element(1, T) =/= dot end, RelevantTokens),
    Brackets = [T || T <- FormTokens, element(1, T) =:= list_to_atom([OpenChar])],
    token_pos(hd(Brackets)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

%% @doc Locate the 1-based {Line, Column} of the character just before the
%% closing bracket (CloseChar, e.g. $] for -export, ${ for... no, $} for
%% -record) of the first top-level attribute of the given Kind (export or
%% record) whose own payload satisfies Matches/1. Attributes are always
%% top-level forms, so no deep tree walk is needed - just the module's own
%% form list.
find_attribute_insertion_point(File, Kind, Matches, CloseChar) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    case [Pos || {attribute, Pos, K, Payload} <- Tree, K =:= Kind, Matches(Payload)] of
        [Pos | _] ->
            Content = read_content(File),
            {ok, find_closing_bracket_before_dot(Content, Pos, CloseChar)};
        [] ->
            undefined
    end.

%% From the attribute's own AST position (which erl_parse reports at the
%% attribute keyword atom, e.g. `export`/`record` itself, not the leading
%% `-`), scan real tokens forward (so multi-line lists work exactly like
%% single-line ones) up to the form's terminating dot, and return the
%% position of the *last* token matching CloseChar seen before it - i.e.
%% the list/tuple's own closing bracket, not some nested one that happens
%% to close earlier.
find_closing_bracket_before_dot(Content, {StartLine, StartCol}, CloseChar) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    RelevantTokens = lists:dropwhile(fun (T) -> token_pos(T) < {StartLine, StartCol} end, Tokens),
    FormTokens = lists:takewhile(fun (T) -> element(1, T) =/= dot end, RelevantTokens),
    Brackets = [T || T <- FormTokens, element(1, T) =:= list_to_atom([CloseChar])],
    token_pos(lists:last(Brackets)).

token_pos({_Type, Pos}) -> Pos;
token_pos({_Type, Pos, _Value}) -> Pos.

%% @doc 1-based {Line, Column} just past the last character of File,
%% suitable as a zero-width "append" insertion point.
end_of_file_insertion_point(File) ->
    Content = read_content(File),
    Lines = binary:split(Content, <<"\n">>, [global]),
    NumLines = length(Lines),
    LastLine = lists:last(Lines),
    {NumLines, byte_size(LastLine) + 1}.

stub_function_text(Name, Arity) ->
    Args = iolist_to_binary(lists:join(<<", ">>,
        [iolist_to_binary(io_lib:format("_Arg~p", [N])) || N <- lists:seq(1, Arity)])),
    iolist_to_binary(io_lib:format("~n~s(~s) ->~n    ok.~n", [Name, Args])).

read_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, Bin} = file:read_file(File),
            Bin;
        Bin ->
            Bin
    end.
