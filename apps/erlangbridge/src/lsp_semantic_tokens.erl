-module(lsp_semantic_tokens).

-export([legend/0, full_tokens/1, full_tokens_delta/2, range_tokens/2]).

-include("lsp_log.hrl").

-define(NAMESPACE, 0).
-define(FUNCTION, 1).
-define(MACRO, 2).
-define(VARIABLE, 3).
-define(PARAMETER, 4).
-define(TYPE, 5).
-define(STRUCT, 6).
-define(PROPERTY, 7).

-define(MOD_DEFINITION, 1).
-define(MOD_DECLARATION, 2).
-define(MOD_DEPRECATED, 8).
-define(MOD_DEFAULT_LIBRARY, 16).

%% @doc Task 3.1's legend. Deliberately semantic-only, per this task's own
%% "must not fight the TextMate grammar" bar: string, number, comment,
%% keyword and operator are declared here (a legend must list every type a
%% client might ever see) but never actually emitted by full_tokens/1 -
%% grammar/Erlang.plist already colors those correctly from pure lexical
%% structure, and re-deriving them from a second, independent pass over
%% the source risks disagreeing with it at the edges for zero benefit.
%% `readonly` is declared but never emitted for the same reason: every
%% Erlang binding is single-assignment, so marking "some" variables
%% readonly would be noise, not signal, without a real definition of what
%% it would even mean here. `declaration` IS used, narrowly: a function
%% name inside its own -spec (a type declaration, distinct from the
%% function's actual defining clause, which gets `definition`).
legend() ->
    #{
        tokenTypes => [<<"namespace">>, <<"function">>, <<"macro">>, <<"variable">>,
                       <<"parameter">>, <<"type">>, <<"struct">>, <<"property">>,
                       <<"string">>, <<"number">>, <<"comment">>, <<"keyword">>, <<"operator">>],
        tokenModifiers => [<<"definition">>, <<"declaration">>, <<"readonly">>,
                           <<"deprecated">>, <<"defaultLibrary">>]
    }.

%% @doc Full-document semantic tokens (task 3.1). Stores its own result
%% under a fresh resultId (task 3.2 - a plain hash of the encoded data,
%% not a counter: identical tokens naturally get the identical id, no
%% per-document sequence state to maintain), so a later
%% semanticTokens/full/delta request can diff against it.
-spec full_tokens(File :: file:filename()) -> #{resultId := binary(), data := [integer()]} | #{data := []}.
full_tokens(File) ->
    case compute(File) of
        {ok, Data} ->
            ResultId = result_id(Data),
            gen_lsp_doc_server:store_semantic_tokens_cache(File, ResultId, Data),
            #{resultId => ResultId, data => Data};
        error ->
            #{data => []}
    end.

%% @doc `textDocument/semanticTokens/full/delta` (task 3.2). If
%% PreviousResultId still matches what this document's own cache holds
%% (see get_semantic_tokens_cache/1 - only ever populated by full_tokens/1
%% or this function itself, never by a plain reparse), respond with a
%% single edit covering just the differing middle region (a common-
%% prefix/common-suffix diff at the whole-token granularity, not a
%% minimal LCS - simple, always correct, and small for the common case of
%% a localized edit). Otherwise - first request, a server restart lost the
%% cache, or the client is out of sync - fall back to a full result, which
%% the protocol always allows.
-spec full_tokens_delta(File :: file:filename(), PreviousResultId :: binary()) ->
    #{resultId := binary(), edits := [map()]} | #{resultId := binary(), data := [integer()]} | #{data := []}.
full_tokens_delta(File, PreviousResultId) ->
    Previous = gen_lsp_doc_server:get_semantic_tokens_cache(File),
    case compute(File) of
        {ok, NewData} ->
            NewResultId = result_id(NewData),
            gen_lsp_doc_server:store_semantic_tokens_cache(File, NewResultId, NewData),
            case Previous of
                {PreviousResultId, OldData} -> #{resultId => NewResultId, edits => diff_tokens(OldData, NewData)};
                _ -> #{resultId => NewResultId, data => NewData}
            end;
        error ->
            #{data => []}
    end.

%% @doc `textDocument/semanticTokens/range` (task 3.2) - the same tokens
%% full_tokens/1 would produce, filtered to just the requested (1-based)
%% line range. Not cached and never diffed - the range variant exists
%% purely so a client can paint a large file's visible region immediately
%% without waiting on (or contributing a resultId for) the whole document.
-spec range_tokens(File :: file:filename(), {StartLine :: integer(), EndLine :: integer()}) -> #{data := [integer()]}.
range_tokens(File, {StartLine, EndLine}) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    case is_list(Tree) of
        true ->
            InRange = [T || {Line, _, _, _, _} = T <- collect_tokens(File, Tree),
                             Line >= StartLine, Line =< EndLine],
            #{data => encode(InRange)};
        false ->
            #{data => []}
    end.

compute(File) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    case is_list(Tree) of
        true -> {ok, encode(collect_tokens(File, Tree))};
        false -> error
    end.

result_id(Data) ->
    integer_to_binary(erlang:phash2(Data, 4294967296)).

%% Diffs two already-encoded (flat, 5-uint32-groups) token arrays and
%% returns the smallest single [start, deleteCount, data] edit whose
%% boundaries land on token-group edges (5-element chunks), by trimming
%% the common prefix and common suffix, both measured in whole groups so
%% a cut can never land in the middle of one token's own 5 numbers.
diff_tokens(OldData, NewData) ->
    OldGroups = group5(OldData),
    NewGroups = group5(NewData),
    {PrefixLen, OldAfterPrefix, NewAfterPrefix} = common_prefix(OldGroups, NewGroups, 0),
    {_SuffixLen, OldMiddle, NewMiddle} = common_suffix(OldAfterPrefix, NewAfterPrefix),
    case {OldMiddle, NewMiddle} of
        {[], []} ->
            [];
        _ ->
            [#{start => PrefixLen * 5, deleteCount => length(OldMiddle) * 5, data => lists:append(NewMiddle)}]
    end.

group5([A, B, C, D, E | Rest]) -> [[A, B, C, D, E] | group5(Rest)];
group5([]) -> [].

common_prefix([H | T1], [H | T2], N) -> common_prefix(T1, T2, N + 1);
common_prefix(L1, L2, N) -> {N, L1, L2}.

common_suffix(L1, L2) ->
    {N, RevRest1, RevRest2} = common_prefix(lists:reverse(L1), lists:reverse(L2), 0),
    {N, lists:reverse(RevRest1), lists:reverse(RevRest2)}.

collect_tokens(File, Tree) ->
    Content = read_content(File),
    Deprecated = deprecated_set(Tree),
    module_tokens(Content, Tree) ++
    record_definition_tokens(Content, Tree) ++
    type_definition_tokens(Content, Tree) ++
    spec_name_tokens(Content, Tree) ++
    function_definition_tokens(Tree, Deprecated) ++
    variable_tokens(Tree) ++
    user_type_tokens(Tree) ++
    whole_file_tokens(File, Deprecated) ++
    macro_tokens(Content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encoding: our own {Line, Col, Length, Type, Modifiers}   %%
%% 5-tuples (1-based, like every AST position in this repo) %%
%% -> the LSP wire format's flat, delta-encoded uint32 list %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode(Tokens) ->
    Sorted = lists:sort(fun ({L1, C1, _, _, _}, {L2, C2, _, _, _}) -> {L1, C1} =< {L2, C2} end, Tokens),
    %% Defensive only: two independent classification passes landing on the
    %% exact same position would otherwise emit overlapping tokens, which
    %% violates the spec - keep whichever came first.
    Deduped = dedupe_by_position(Sorted),
    lists:flatten(encode_deltas(Deduped, {1, 1})).

dedupe_by_position([{L, C, _, _, _} = T, {L, C, _, _, _} | Rest]) ->
    dedupe_by_position([T | Rest]);
dedupe_by_position([T | Rest]) ->
    [T | dedupe_by_position(Rest)];
dedupe_by_position([]) ->
    [].

encode_deltas([], _Prev) ->
    [];
encode_deltas([{Line, Col, Length, Type, Mods} | Rest], {PrevLine, PrevCol}) ->
    DeltaLine = Line - PrevLine,
    DeltaCol = case DeltaLine of 0 -> Col - PrevCol; _ -> Col - 1 end,
    [[DeltaLine, DeltaCol, Length, Type, Mods] | encode_deltas(Rest, {Line, Col})].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% attribute-declared names (scanned)  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -module/-record/-type/-opaque/-spec attribute positions all point at
%% the attribute *keyword* (module/record/type/...), never at the name
%% that follows it - the name is always the first atom token after that
%% keyword, so a single token scan covers every one of them.
module_tokens(Content, Tree) ->
    case [Pos || {attribute, Pos, module, _Name} <- Tree] of
        [Pos | _] ->
            {Line, Col, Length} = first_atom_after(Content, Pos),
            [{Line, Col, Length, ?NAMESPACE, ?MOD_DEFINITION}];
        [] ->
            []
    end.

record_definition_tokens(Content, Tree) ->
    lists:flatmap(fun
        ({attribute, Pos, record, {_Name, Fields}}) ->
            {Line, Col, Length} = first_atom_after(Content, Pos),
            [{Line, Col, Length, ?STRUCT, ?MOD_DEFINITION} | record_field_tokens(Fields, ?MOD_DEFINITION)];
        (_) ->
            []
    end, Tree).

type_definition_tokens(Content, Tree) ->
    lists:filtermap(fun
        ({attribute, Pos, Tag, {_Name, _TypeDef, _Args}}) when Tag =:= type; Tag =:= opaque ->
            {Line, Col, Length} = first_atom_after(Content, Pos),
            {true, {Line, Col, Length, ?TYPE, ?MOD_DEFINITION}};
        (_) ->
            false
    end, Tree).

%% Only the common local-function spec shape (`-spec name(...) -> ...`) is
%% handled - a remote spec (`-spec Mod:name(...) -> ...`, rarely used) would
%% have this scan land on the module name instead, which is a known,
%% accepted limitation for this minimal a pass.
spec_name_tokens(Content, Tree) ->
    lists:filtermap(fun
        ({attribute, Pos, spec, _}) ->
            {Line, Col, Length} = first_atom_after(Content, Pos),
            {true, {Line, Col, Length, ?FUNCTION, ?MOD_DECLARATION}};
        (_) ->
            false
    end, Tree).

%% Pos is the attribute keyword's own position (e.g. `module` in
%% `-module(sample)`) - which is itself an ordinary atom token, not a
%% reserved word, so it would otherwise be found as its own "first atom
%% after Pos". Tokens at-or-before Pos are dropped so the scan starts
%% strictly after the keyword, landing on the real name that follows it.
first_atom_after(Content, {StartLine, StartCol}) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Content), {1, 1}),
    RelevantTokens = lists:dropwhile(fun (T) -> token_pos(T) =< {StartLine, StartCol} end, Tokens),
    FormTokens = lists:takewhile(fun (T) -> element(1, T) =/= dot end, RelevantTokens),
    [{atom, {Line, Col}, Name} | _] = [T || T <- FormTokens, element(1, T) =:= atom],
    {Line, Col, length(atom_to_list(Name))}.

token_pos({_Type, Pos}) -> Pos;
token_pos({_Type, Pos, _Value}) -> Pos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -deprecated(...) - marks matching function tokens %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Only the common {Name, Arity} / {Name, Arity, Description} entries are
%% tracked - the '_' (all arities) and whole-module (`-deprecated(module)`)
%% forms are not, since matching those against every call site in the file
%% is real extra work for a rarely-used form.
deprecated_set(Tree) ->
    lists:foldl(fun
        ({attribute, _, deprecated, Entries}, Acc) when is_list(Entries) ->
            lists:foldl(fun deprecated_entry/2, Acc, Entries);
        (_, Acc) ->
            Acc
    end, sets:new(), Tree).

deprecated_entry({Name, Arity}, Acc) when is_integer(Arity) -> sets:add_element({Name, Arity}, Acc);
deprecated_entry({Name, Arity, _Desc}, Acc) when is_integer(Arity) -> sets:add_element({Name, Arity}, Acc);
deprecated_entry(_, Acc) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function definitions - one token per clause (each      %%
%% clause repeats the name textually, so each needs its    %%
%% own token, not just the top-level {function,...} node)  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function_definition_tokens(Tree, Deprecated) ->
    lists:flatmap(fun
        ({function, _, Name, Arity, Clauses}) ->
            Mod = deprecated_modifier(Name, Arity, Deprecated),
            NameLen = length(atom_to_list(Name)),
            [{Line, Col, NameLen, ?FUNCTION, ?MOD_DEFINITION bor Mod}
             || {clause, {Line, Col}, _, _, _} <- Clauses];
        (_) ->
            []
    end, Tree).

deprecated_modifier(Name, Arity, Deprecated) ->
    case sets:is_element({Name, Arity}, Deprecated) of
        true -> ?MOD_DEPRECATED;
        false -> 0
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameters and variables - needs clause-level scoping,  %%
%% so this walks {function,...} forms directly rather than %%
%% going through the generic whole-file fold below          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

variable_tokens(Tree) ->
    lists:flatmap(fun
        ({function, _, _, _, Clauses}) -> lists:flatmap(fun clause_variable_tokens/1, Clauses);
        (_) -> []
    end, Tree).

%% CHARACTERIZATION: a clause's head patterns are always `parameter`
%% (definition, every time - each clause is its own binding site). Inside
%% guards/body, a name's *first* occurrence (in source order) is its
%% `definition` and every later one is a plain `variable` reference - this
%% is exactly right, not just a heuristic, because Erlang requires a
%% body-scope variable to be bound before any use of it. Parameter names
%% are pre-seeded as "already seen" so a parameter's first *use* inside the
%% body is correctly a reference, not treated as a fresh definition.
clause_variable_tokens({clause, _, Patterns, Guards, Body}) ->
    ParamVars = [V || {var, _, Name} = V <- collect_vars(Patterns), Name =/= '_'],
    ParamNames = sets:from_list([Name || {var, _, Name} <- ParamVars]),
    ParamTokens = [var_token(Pos, Name, ?PARAMETER, ?MOD_DEFINITION) || {var, Pos, Name} <- ParamVars],
    BodyVars = collect_vars(lists:append(Guards)) ++ collect_vars(Body),
    {_, VarTokensRev} = lists:foldl(fun ({var, Pos, Name}, {Seen, Acc}) ->
        case Name of
            '_' ->
                {Seen, Acc};
            _ ->
                case sets:is_element(Name, Seen) of
                    true -> {Seen, [var_token(Pos, Name, ?VARIABLE, 0) | Acc]};
                    false -> {sets:add_element(Name, Seen), [var_token(Pos, Name, ?VARIABLE, ?MOD_DEFINITION) | Acc]}
                end
        end
    end, {ParamNames, []}, BodyVars),
    ParamTokens ++ lists:reverse(VarTokensRev).

var_token({Line, Col}, Name, Type, Mods) ->
    {Line, Col, length(atom_to_list(Name)), Type, Mods}.

%% erl_syntax_lib:fold visits in source order but *conses* onto the
%% accumulator as it goes, so the raw result comes out latest-first - the
%% final reverse restores real source order (needed here to tell a
%% variable's first, defining occurrence from its later uses).
collect_vars(Nodes) ->
    lists:flatmap(fun (Node) ->
        lists:reverse(erl_syntax_lib:fold(fun
            ({var, Pos, Name}, Acc) -> [{var, Pos, Name} | Acc];
            (_, Acc) -> Acc
        end, [], Node))
    end, Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -spec/-type/-opaque payloads: erl_syntax_lib:fold doesn't %%
%% descend into these (confirmed empirically - their payload %%
%% is opaque to it), so user_type references are found with  %%
%% a small generic term walk instead: recurse into any tuple %%
%% or list, collecting every {user_type, Pos, Name, Args} -   %%
%% simpler than modeling the whole type-AST grammar.          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_type_tokens(Tree) ->
    lists:flatmap(fun
        ({attribute, _, spec, Payload}) -> user_type_tokens_in(Payload);
        ({attribute, _, Tag, Payload}) when Tag =:= type; Tag =:= opaque -> user_type_tokens_in(Payload);
        (_) -> []
    end, Tree).

user_type_tokens_in(Payload) ->
    [{Line, Col, length(atom_to_list(Name)), ?TYPE, 0} || {{Line, Col}, Name} <- find_user_types(Payload)].

find_user_types({user_type, Pos, Name, Args}) ->
    [{Pos, Name} | find_user_types(Args)];
find_user_types(Term) when is_tuple(Term) ->
    lists:flatmap(fun find_user_types/1, tuple_to_list(Term));
find_user_types(Term) when is_list(Term) ->
    lists:flatmap(fun find_user_types/1, Term);
find_user_types(_Term) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% everything else: a single generic whole-file walk for %%
%% record construction/update/field-access and function  %%
%% call sites (local and module-qualified)                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

whole_file_tokens(File, Deprecated) ->
    lsp_syntax:fold_in_syntax_tree(fun
        %% #rec{...} construction
        ({record, Pos, RecName, Fields}, _CurFile, Acc) ->
            [hash_offset_token(Pos, RecName) | record_field_tokens(Fields, 0)] ++ Acc;
        %% Expr#rec{...} update
        ({record, Pos, _Expr, RecName, Fields}, _CurFile, Acc) ->
            [hash_offset_token(Pos, RecName) | record_field_tokens(Fields, 0)] ++ Acc;
        %% Expr#rec.field access
        ({record_field, Pos, _Expr, RecName, {atom, FPos, FName}}, _CurFile, Acc) ->
            [hash_offset_token(Pos, RecName), atom_token(FPos, FName, ?PROPERTY, 0) | Acc];
        %% local call
        ({call, _, {atom, FPos, FName}, Args}, _CurFile, Acc) ->
            Mod = deprecated_modifier(FName, length(Args), Deprecated),
            [atom_token(FPos, FName, ?FUNCTION, Mod) | Acc];
        %% module-qualified call
        ({call, _, {remote, _, {atom, MPos, ModName}, {atom, FPos, FName}}, _Args}, _CurFile, Acc) ->
            NsMod = case is_default_library(ModName) of true -> ?MOD_DEFAULT_LIBRARY; false -> 0 end,
            [atom_token(MPos, ModName, ?NAMESPACE, NsMod), atom_token(FPos, FName, ?FUNCTION, 0) | Acc];
        (_, _CurFile, Acc) ->
            Acc
    end, [], File).

%% Every #rec(...) occurrence's own Pos - construction, update or field
%% access alike - is the position of the '#' character itself, one
%% character before the record name it always immediately precedes.
hash_offset_token({Line, Col}, RecName) ->
    {Line, Col + 1, length(atom_to_list(RecName)), ?STRUCT, 0}.

record_field_tokens(Fields, Modifier) ->
    lists:filtermap(fun
        ({record_field, _, {atom, FPos, FName}, _Value}) -> {true, atom_token(FPos, FName, ?PROPERTY, Modifier)};
        ({record_field, _, {atom, FPos, FName}}) -> {true, atom_token(FPos, FName, ?PROPERTY, Modifier)};
        ({typed_record_field, RecordField, _Type}) ->
            case record_field_tokens([RecordField], Modifier) of
                [Token] -> {true, Token};
                [] -> false
            end;
        (_) ->
            false
    end, Fields).

atom_token({Line, Col}, Name, Type, Mods) ->
    {Line, Col, length(atom_to_list(Name)), Type, Mods}.

is_default_library(ModName) ->
    lists:member(atom_to_list(ModName), gen_lsp_config_server:standard_modules()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% macros: expanded away by epp before the AST is built, so %%
%% both definitions and usages are found the same way the   %%
%% rest of this codebase already finds them for go-to-       %%
%% definition (lsp_navigation:find_macro_reference/2) - a    %%
%% plain per-line regex, not the AST.                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

macro_tokens(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    IndexedLines = lists:zip(lists:seq(1, length(Lines)), Lines),
    lists:flatmap(fun ({LineNum, LineContent}) -> macro_tokens_in_line(LineNum, LineContent) end, IndexedLines).

macro_tokens_in_line(LineNum, LineContent) ->
    define_tokens(LineNum, LineContent) ++ usage_tokens(LineNum, LineContent).

define_tokens(LineNum, LineContent) ->
    case re:run(LineContent, <<"-define\\(\\s*([A-Za-z_][A-Za-z0-9_]*)">>, [global, {capture, [1], index}]) of
        {match, Matches} ->
            [{LineNum, Pos + 1, Len, ?MACRO, ?MOD_DEFINITION} || [{Pos, Len}] <- Matches];
        nomatch ->
            []
    end.

usage_tokens(LineNum, LineContent) ->
    case re:run(LineContent, <<"\\?[A-Za-z_][A-Za-z0-9_]*">>, [global]) of
        {match, Matches} ->
            [{LineNum, Pos + 2, Len - 1, ?MACRO, 0} || [{Pos, Len}] <- Matches];
        nomatch ->
            []
    end.

read_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, Bin} = file:read_file(File),
            Bin;
        Bin ->
            Bin
    end.
