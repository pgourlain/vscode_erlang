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
-spec code_actions(File :: file:filename(), Range :: term(), Context :: map()) -> [map()].
code_actions(File, _Range, Context) ->
    Diagnostics = maps:get(diagnostics, Context, []),
    lists:flatmap(fun (Diagnostic) -> actions_for_diagnostic(File, Diagnostic) end, Diagnostics).

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
