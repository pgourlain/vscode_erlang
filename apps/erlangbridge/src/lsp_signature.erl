-module(lsp_signature).

-export([eep48_render_signature/5]).
-export([disable_signature_help/0, signature_help_fromtokens/2]).

-include("lsp_log.hrl").

disable_signature_help() ->
    [].

signature_help_fromtokens(FileModule, Tokens) ->
    RTokens = lists:reverse(Tokens),
    % find index and function name from tokens
    M = lists:foldl(
        fun
            ({atom, _, _} = Token, #{argLevel := Level} = Acc) when Level == 0 ->
                HasModule = maps:get(hasModule, Acc),
                if
                    HasModule == false -> Acc#{function => erl_scan:symbol(Token)};
                    true -> Acc#{module => erl_scan:symbol(Token)}
                end;
            ({':', _}, #{argLevel := Level} = Acc) when Level == 0 ->
                Acc#{hasModule => true};
            ({'(', _}, #{argLevel := Level} = Acc) ->
                Acc#{argLevel => (Level - 1)};
            ({')', _}, #{argLevel := Level} = Acc) ->
                Acc#{argLevel => (Level + 1)};
            ({',', _}, #{argIndex := ArgIndex, argLevel := Level} = Acc) when Level == 1 ->
                Acc#{argIndex => (ArgIndex + 1)};
            (_, Acc) ->
                Acc
        end,
        #{hasModule => false, argIndex => 0, argLevel => 1, function => null, module => null},
        RTokens
    ),
    case M of
        #{function := Fun, module := Module, argIndex := Index} when Fun =/= null ->
            TargetModule =
                if
                    Module == null -> FileModule;
                    true -> Module
                end,
            function_signature(FileModule, TargetModule, Fun, any, Index);
        _ ->
            disable_signature_help()
    end.

function_signature(EditingModule, TargetModule, Function, Arity, Index) ->
    case gen_lsp_doc_server:get_module_file(TargetModule) of
        undefined ->
            get_generic_help(EditingModule, TargetModule, Function, Index);
        File ->
            case lsp_utils:is_erlang_lib_file(File) of
                false ->
                    case function_doc_syntax(File, Function, Arity) of
                        {[],[]} ->
                            case lists:keyfind(Function, 1, erlang:module_info(exports)) of
                                {Function, _} ->
                                    gen_lsp_help_server:get_help(erlang, Function, {EditingModule, Index}, lsp_signature, eep48_render_signature);
                                _ ->
                                    <<>>
                            end;
                        {ClausesList, SpecList} ->
                            % % using edoc should be on compilable file...
                            % % so get latest syntax tree and parse spec should be better solution
                            % EDOC = edoc:get_doc(File, [{hidden, true}, {private, true}]),
                            Merged = merge_spec_clauses(Function, ClausesList, SpecList),                            
                            spec_to_signatures(Function, Index, Merged,[]);
                        _Other ->                            
                            #{
                                signatures => [],
                                activeSignature => 0,
                                activeParameter => 0
                            }
                    end;
                _ ->
                    get_generic_help(EditingModule, TargetModule, Function, Index)
            end
    end.

spec_to_signatures(Function, Index, SpecList, FDocEEp48) ->
    ActiveSignature = lsp_utils:index_of(fun 
        ({{Fn, FnArity},_,_}) when Fn =:= Function andalso Index+1 =< FnArity -> true;
        (_) -> false 
    end, SpecList),
    Signatures = lists:map(fun ({{_, _}, Args, _}=Fn) ->
    #{
        label => function_label(Fn),
        documentation => function_documentation(FDocEEp48),
        parameters => function_parameters(Args) 
    }
    end, SpecList),
    #{
        signatures => Signatures,
        activeSignature => ActiveSignature,
        activeParameter => Index
    }.

function_documentation([]) ->
    #{
        kind => <<"markdown">>,
        value => <<"">>
    };
function_documentation(_FDoc) ->
    #{
        kind => <<"markdown">>,
        value => <<"">>
    }.

function_doc_syntax(File, Function, Arity) ->
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


function_label({{FnName, _FnArity}, Args, ResType}) ->
    FnStart = io_lib:format("~p(~s) -> ~s",[FnName, function_label_parameters(Args),spectype_to_string(ResType)]),
    list_to_binary(FnStart).

function_parameters(Args) ->
    lists:map(fun({N,T}) ->
        #{
            label => list_to_binary(spectype_to_string(N,T))
        }
        ;(_) -> #{
                label => <<"not yet implemented">>
            }
        end, Args).

function_label_parameters(Args) ->
    lists:flatten(lists:join(", ",
        lists:map(fun
            ({N,T}) -> spectype_to_string(N,T);
            (_) -> "not yet implemented"
        end, Args))).

spectype_to_string(L) when is_list(L) ->
    Res = lists:flatten(
    lists:join(" | ", 
        lists:map(fun (Item) ->
            io_lib:format("~s",[Item])
        end, L)
    )),
    Res;
spectype_to_string({A, undefined}) ->
    io_lib:format("~s",[A]);
spectype_to_string({A, T}) ->
    io_lib:format("~s :: ~s",[A, T]);
spectype_to_string(A) ->
    io_lib:format("~s",[A]).

spectype_to_string(N,undefined) ->
    io_lib:format("~s",[N]);
spectype_to_string(N,T) when is_atom(N)->   
    io_lib:format("~s :: ~s",[N, spectype_to_string(T)]);
spectype_to_string(N,T) when is_list(N) ->
    io_lib:format("{~s}",[tuplelisttype_to_string(N, spectype_to_string(T))]);
spectype_to_string(N,T) ->   
    io_lib:format("~p :: ~s",[N, spectype_to_string(T)]).

tuplelisttype_to_string(List, Type)->
    lists:join(", ", 
        lists:map(fun (Item) ->
            tupletype_to_string(Item, Type)
        end, List)).

tupletype_to_string({var, _, VarName}, Type) when is_atom(VarName) ->
    io_lib:format("~s :: ~s",[VarName, Type]);
tupletype_to_string({_, _, VarName}, Type) ->
    io_lib:format("~p :: ~s",[VarName, Type]).

merge_spec_clauses(Function, ClausesList, SpecList) ->
    LightSpecList = to_light_speclist(SpecList),
    ClausesLightSpec = clause_to_light_speclist(Function, ClausesList),  
    lists:sort(fun ({{_,A1},_,_}, {{_,A2},_,_}) -> A1 < A2 end
        ,lists:foldl(fun ({Key,_,_ }=X, Acc) ->
        case lists:keyfind(Key,1,Acc) of
            false -> [X|Acc];
            _ -> Acc
            end
        end,
        LightSpecList, ClausesLightSpec)).           

%% @doc return a list of 'light' spec info
%% {{FunctionName, Arity}, [{ParamName, ParamType}], ReturnType}
to_light_speclist(SpecList) ->
    lists:map(fun 
        ({{FnName,Arity}, [{type, _, 'fun', _}=Type|_T]}) -> 
            {Args, Res} = one_type(Type, direct, []),
            {{FnName,Arity}, Args,  Res};
        ({{FnName,Arity}, [{type,_, bounded_fun,[{type, _, 'fun', _}=Type|TypeArgs]}]=_Bounded}) -> 
            Constraints = many_constraints(lists:flatten(TypeArgs)),
            {Args, Res} = one_type(Type, direct, Constraints),
            {{FnName,Arity}, Args,  Res};
        (_) -> undefined
        end, SpecList).
clause_to_light_speclist(Function, ClausesList) ->
    lists:map(fun
        ([{clause, _, Args, _,_}|_T]) -> {{Function, length(Args)}, map_clause_args(Args),any};
        (_Other) -> undefined
        end, ClausesList).

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
        ({tuple, _, _}) -> {'Tuple', tuple};
        ({cons, _, _, _}) -> {'List', list};
        ({nil, _}) -> {'List', nil};
        ({_,_,ArgName}) -> {ArgName, any};
        (_Other) -> 
             ?LOG("unknown_clause_arg:~p",[_Other]),
            {undefined, not_yet_implemented}
        end, Args).

record_name(RecType) ->
    list_to_atom(atom_to_list('#') ++ atom_to_list(RecType)).

one_type({type, _, 'fun', [Args, Res]}, direct, _Contraints) ->
    {one_type(Args, direct, _Contraints), one_type(Res, direct, _Contraints)};
one_type({type, _, product, ProductTypes}, direct, _Contraints) ->
    lists:map(fun (T) -> one_type(T,direct, _Contraints) end, ProductTypes);
one_type({var, _, VarName}, direct, _Contraints) ->
    VarType = case lists:keyfind(VarName, 1, _Contraints) of
        {_,Type} -> Type;
        _ -> undefined
    end,
    {VarName, VarType};
one_type({var, _, VarName}, union, _Contraints) ->
    VarName;
one_type({ann_type, _, [{var,_,VarName}, AnnTypes]}, State, _Contraints) ->
    case State of
        union -> one_type(AnnTypes, State, _Contraints);
        _ -> {VarName, one_type(AnnTypes, State, _Contraints)}
    end;
one_type({type, _, Type, []}, _, _Contraints) ->
    Type;
one_type({type, _, union, [{type,_,FirstType,[]},B]}, _State, _Contraints) ->
    SubTypes = one_type(B, union, _Contraints),
    ArrayTypes = if 
        is_list(SubTypes) == false -> [SubTypes];
        true ->SubTypes
    end,
    [FirstType | ArrayTypes];
one_type({type, _, union, [A,B]}, _State, _Contraints) ->
    FirstType = one_type(A, union, _Contraints),
    SubTypes = one_type(B, union, _Contraints),
    ArrayTypes = if 
        is_list(SubTypes) == false -> [SubTypes];
        true ->SubTypes
    end,
    [FirstType | ArrayTypes];
one_type({type, _, map, _MapAssoc}, _State, _Contraints) ->
    %todo 
    % {type,{304,14},
    %     map,
    %     [{type,{304,20},
    %             map_field_assoc,
    %             [{var,{304,16},'Key'},{var,{304,23},'Value'}]},
    %         {type,{304,32},
    %             map_field_assoc,
    %             [{var,{304,30},'_'},{var,{304,35},'_'}]}]}
    map;
one_type({type, _, Type, _SubTypes}, _State, _Contraints) ->
    Type;
one_type({atom, _, Value}, _, _Contraints) ->
    Value;
one_type({remote_type, _, [M, MType,_]}, _, _Contraints) ->
    list_to_atom(atom_to_list(one_type(M, direct, _Contraints)) 
        ++ ":" ++ 
    atom_to_list(one_type(MType, direct, _Contraints)));
one_type({user_type, _, Value, _SubTypes}, _, _Contraints) ->
    Value;
one_type(_A1,_A2, _Contraints) ->
    ?LOG("signature:unmapped one_type(~p,~p,_)", [_A1,_A2]),
    undefined.

many_constraints([{type, _, constraint, C}]) ->
    [one_constraint(C)];
many_constraints([{type, _, constraint, C}|T]) ->
    [one_constraint(C) | many_constraints(T)];
many_constraints([]) ->
    [].

one_constraint([{atom, _,_}, [{var, _, VarName},T]]) ->
    {VarName, one_type(T, union,[])}.

get_generic_help(EditingModule, Module, Function, Index) ->
    Help = gen_lsp_help_server:get_help(Module, Function, {EditingModule, Index}, lsp_signature, eep48_render_signature),
    case Help of
        undefined -> <<>>;
        _ -> Help
    end.

eep48_render_signature(_Module, Function, FDocs, _Docs,  _State) ->
    {_EditingModule, Index} = _State,
    % when Module <> EditingModule, we can get body documentation
    Grouping =
        lists:foldl(
          fun({Group,_Anno,_Sig,FnDoc,#{ signature := Signature }} = _Func,Acc) ->
                SignAndDoc = maps:get(Group, Acc, #{}),
                Signatures = maps:get(signatures, SignAndDoc, []),
                FnDocs = maps:get(docs, SignAndDoc, []),
                Acc#{ Group => #{ signatures => [Signature|Signatures], docs => [FnDoc|FnDocs]} };
             ({_Group, _Anno, _Sig, _Doc, _Meta} = _Func, Acc) -> Acc
          end, #{}, lists:sort(FDocs)),
    MAP = lists:flatten(lists:map(
      fun({{_, _Fn, _FnArity} = _Group, #{ signatures:= Signatures, docs := _FnDocs}}) ->
        SpecList = lists:map(fun ({attribute, _, spec, {{_,_},_}=FnSpec}) ->
                FnSpec
            end, lists:flatten(Signatures)),
        SpecList
      end, maps:to_list(Grouping))),
    
    spec_to_signatures(Function, Index, to_light_speclist(MAP), []).

