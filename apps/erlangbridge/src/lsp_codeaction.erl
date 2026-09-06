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

fix(_File, _Diagnostic, _Module, _MessageBody) ->
    [].

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
actions_for_cursor(File, #{start := #{line := Line0}}) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    case is_list(Tree) andalso lsp_navigation:find_function_with_line(Tree, Line0 + 1) of
        {function, Pos, Name, Arity, Clauses} ->
            export_toggle_actions(File, Tree, Pos, Name, Arity) ++
            generate_spec_actions(File, Tree, Pos, Name, Arity, Clauses);
        _ ->
            []
    end;
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
