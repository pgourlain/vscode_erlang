-module(lsp_fun_utils).


-export([get_function_range/1, get_type_range/1]).
-export([get_function_infos/3]).

-include("lsp_log.hrl").


get_function_range({function, {L, C}, _FnName, _FnArity, Body}) ->
    LastClause = lists:last(Body),
    case get_latest_lc(LastClause) of
        {-1,-1} -> {L,C,L,C};
        {L1,C1} -> {L,C,L1+1,C1}
    end.

get_type_range({attribute, {L, C}, type, {_Type, TypDef,_}}) ->
    case get_latest_lc(TypDef) of
        {-1,-1} -> {L,1,L,1};
        {L1,C1} -> {L,C,L1+1,C1}
    end.

% basic filter to get the latest line/column
get_latest_lc({clause, {L, C}, _, _, Body}) ->
    case Body of
        [] -> {L,C};
        _ -> get_latest_lc(lists:last(Body))
    end;
get_latest_lc({T, {L, C}, _, Args}) when T =:= call orelse T =:= 'case' orelse T =:= record ->
    case Args of
        [] -> {L,C};
        _ -> get_latest_lc(lists:last(Args))
    end;
get_latest_lc({M, {_, _}, _, Args}) when M =:= match orelse M =:= cons orelse M =:= map_field_assoc 
                                    orelse M =:= record_field ->
    get_latest_lc(Args);
get_latest_lc({'try', {_, _}, A1, A2, A3, A4}) ->
    List = A1 ++ A2 ++ A3 ++ A4,
    get_latest_lc(lists:last(List));
get_latest_lc({T, {L, C}, Clauses}=_Other) when T =:= tuple orelse T =:= 'if' orelse T =:= map ->
    case Clauses of
        [] -> {L,C};
        _ -> get_latest_lc(lists:last(Clauses))
    end;
get_latest_lc({type, {L, C}, _Type, TypeDefList}) when is_list(TypeDefList) ->
    case TypeDefList of
        [] -> {L,C};
        _ -> get_latest_lc(lists:last(TypeDefList))
    end;
get_latest_lc({_, {L, C}}=_Other) ->
    %?LOG("get_latest_lc: 0 token: ~p", [_Other]),
    {L,C};
get_latest_lc({_, {L, C}, _}=_Other) ->
    %?LOG("get_latest_lc: 1 token: ~p", [_Other]),
    {L,C};
get_latest_lc({_, {L, C}, _, _}=_Other) ->
    %?LOG("get_latest_lc: 2 token: ~p", [_Other]),
    {L,C};
get_latest_lc({_, {L, C}, _, _, _}=_Other) ->
    %?LOG("get_latest_lc: 3 token: ~p", [_Other]),
    {L,C};
get_latest_lc(_Other) ->
    ?LOG("get_latest_lc: unknown token: ~p", [_Other]),
    {-1,-1}.

% list of signatures of functions specified by Name and Arity
 get_function_infos(File, Function, Arity) ->
    SpecInfos = get_function_spec_infos(File, Function, Arity),
    Result = case SpecInfos of
        {[], []} -> 
            ?LOG("No spec found for ~p/~p", [Function, Arity]),
            undefined;
        {ClauseList, SpecList} ->
            ?LOG("merge_spec_clauses: ~p,~p", [ClauseList, SpecList]),
            merge_spec_clauses(Function, ClauseList, SpecList)
    end,
    ?LOG(" => ~p", [Result]),
    FuncAsString = lsp_utils:to_string(Function),
    #{
        name => FuncAsString,
        arity => Arity,
        return_type => #{
            label => "any",
            type => any
            },
        signatures => [
            #{
                label => FuncAsString ++ "/" ++ integer_to_list(Arity),
                parameters => [
                    #{
                        label => "Module",
                        name => "Module",
                        documentation => "Module name",
                        type => any
                    },
                    #{
                        label => "Args",
                        name => "Args",
                        documentation => "List of arguments",
                        type => any
                    }
                ]
            }
        ]
    }.


get_function_spec_infos(File, Function, Arity) ->
    % extract spec and clauses for a specific function
    lsp_syntax:fold_in_syntax_tree(fun
        ({function, _LC, FoundFunction, FoundArity, Clauses}, _CurrentfFile, {AccClauses, AccSpec})
                when FoundFunction =:= Function andalso (Arity =:= any orelse FoundArity == Arity) ->
            {[Clauses | AccClauses] , AccSpec};
        ({attribute, _, spec, {{Fn,FnArity},_}=FnSpec}, _CurrentFile, {AccClauses, AccSpec}) 
                                  when Fn =:= Function andalso (Arity=:= any orelse Arity=:=FnArity) ->
            {AccClauses, [FnSpec | AccSpec]};
        (_SyntaxTree, _CurrentFile, Acc) ->
            Acc
    end, {[], []}, File).

merge_spec_clauses(Function, ClauseList, SpecList) ->    
    LightSpecList = spec_to_signature(SpecList),
    ClausesLightSpec = clause_to_signature(Function, ClauseList),  
    lists:sort(fun ({{_,A1},_,_}, {{_,A2},_,_}) -> A1 < A2 end
        ,lists:foldl(fun ({Key,_,_ }=X, Acc) ->
        case lists:keyfind(Key,1,Acc) of
            false -> [X|Acc];
            _ -> Acc
            end
        end,
        LightSpecList, ClausesLightSpec)). 

spec_to_signature(SpecList) ->
    [].

clause_to_signature(Function, ClauseList) ->
        lists:map(fun
        ([{clause, _, Args, _,_}|_T]) -> {{Function, length(Args)}, map_clause_args(Args),any};
        (_Other) -> undefined
        end, ClauseList).

map_clause_args(Args) ->
    lists:map(fun         
        ({match, _, {tuple, _, _}, {var ,_ ,ArgName}}) -> {ArgName, tuple};
        ({match, _, {cons, _, _, _}, {var ,_ ,ArgName}}) -> {ArgName, list};
        ({match, _, {map, _, _}, {var ,_ ,ArgName}}) -> {ArgName, map};
        ({match, _, {record, _, RecType, _}, {var ,_ ,ArgName}}) -> {ArgName, record_name(RecType)};
        ({match, _, {nil, _}, {var ,_ ,ArgName}}) -> {ArgName, nil};
        ({map, _,_}) -> {'Map', map};        
        ({tuple, _, _}) -> {'Tuple', tuple};
        ({record, _, RecType, _}) -> {'Record', record_name(RecType)};
        ({cons, _, _, _}) -> {'List', list};
        ({nil, _}) -> {'List', nil};
        ({_,_,ArgName}) -> {ArgName, any};
        (_Other) -> 
             ?LOG("unknown_clause_arg:~p",[_Other]),
            {undefined, not_yet_implemented}
        end, Args).

record_name(RecType) ->
    list_to_atom(atom_to_list('#') ++ atom_to_list(RecType)).
