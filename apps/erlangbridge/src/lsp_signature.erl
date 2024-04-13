-module(lsp_signature).

-export([signatures_sample/1]).
-export([eep48_render_signature/5]).
-export([disable_signature_help/0, signature_help_fromtokens/2]).

-define(LOG(S), begin
    gen_lsp_server:lsp_log("~p", [S])
end).
-define(LOG(Fmt, Args), begin
    gen_lsp_server:lsp_log(Fmt, Args)
end).

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
            function_signature(TargetModule, Fun, any, Index);
        _ ->
            disable_signature_help()
    end.

function_signature(Module, Function, Arity, Index) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            get_generic_help(Module, Function, Index);
        File ->
            case lsp_utils:is_erlang_lib_file(File) of
                false ->
                    case function_doc_syntax(File, Function, Arity) of
                        {[],[]} ->
                            case lists:keyfind(Function, 1, erlang:module_info(exports)) of
                                {Function, _} ->
                                    gen_lsp_help_server:get_help(erlang, Function, Index, lsp_signature, eep48_render_signature);
                                _ ->
                                    <<>>
                            end;
                        {ClausesList, SpecList} ->
                            % % using edoc should be on compilable file...
                            % % so get latest syntax tree and parse spec should be better solution
                            % EDOC = edoc:get_doc(File, [{hidden, true}, {private, true}]),
                            Merged = merge_spec_clauses(Function, ClausesList, SpecList),
                            spec_to_signatures(Function, Index, Merged);
                        Other ->
                            %?LOG("other match clause : ~p",[Other]),
                            #{
                                signatures => [],
                                activeSignature => 0,
                                activeParameter => 0
                            }

                    end;
                _ ->
                    get_generic_help(Module, Function, Index)
            end
    end.

spec_to_signatures(Function, Index, SpecList) ->
    ActiveSignature = lsp_utils:index_of(fun 
        ({{Fn, FnArity},_,_}) when Fn =:= Function andalso Index+1 =< FnArity -> true;
        (_) -> false 
    end, SpecList),
    Signatures = lists:map(fun ({{_, _}, Args, _}=Fn) ->
    #{
        label => function_label(Fn),
        parameters => function_parameters(Args) 
    }
    end, SpecList),
    #{
        signatures => Signatures,
        activeSignature => ActiveSignature,
        activeParameter => Index
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
    end, {[], []}, File, gen_lsp_doc_server:get_syntax_tree(File)).


function_label({{FnName, _FnArity}, Args, ResType}) ->
    list_to_binary(io_lib:format("~p(~p) -> ~p",[FnName, function_label_parameters(Args),ResType])).

function_parameters(Args) ->
    lists:map(fun({N,T}) ->
        #{
            label => list_to_binary(io_lib:format("~p::~p",[N,T]))
        }
        end, Args).

function_label_parameters(Args) ->
    lists:flatten(lists:join(",",
        lists:map(fun({N,T}) ->
            io_lib:format("~p::~p",[N,T])
            end, Args))).

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
        ({{FnName,Arity}, [{type, _, 'fun', Args}|TypeRes]}) -> {{FnName,Arity}, map_spec_args(Args),  map_spec_res(TypeRes)};
        ({{FnName,Arity}, [{type,_, bounded_fun,[{type, _, 'fun', Args}|TypeArgs]}]}) -> 
            TypeTuples = bounded_type_tuples(lists:flatten(TypeArgs)),            
            {{FnName,Arity}, map_spec_args_from_bounded(Args, TypeTuples),  map_spec_res_from_bounded(Args, TypeTuples)};
        (_) -> undefined
        end, SpecList).
clause_to_light_speclist(Function, ClausesList) ->
    lists:map(fun
        ([{clause, _, Args, _,_}|_T]) -> {{Function, length(Args)}, map_clause_args(Args),any};
        (_) -> undefined
        end, ClausesList).

map_clause_args(Args) ->
    lists:map(fun 
        ({_,_,ArgName}) -> {ArgName, any};
        (_) -> undefined
        end, Args).

map_spec_args([{type,_,product,Args}|_T]) ->
    lists:map(fun 
        ({ann_type,_, [{_,_, ArgName},{_,_,ArgType,_}]}) -> { ArgName, ArgType};
        ({var, _,ArgName}) -> {ArgName, undefined};
        (_) -> undefined
        end, Args);

map_spec_args(_Args) -> undefined.

map_spec_res([_,{_ResType, _,ResValue}]) ->
    ResValue;
map_spec_res([_,{type, _,ResType,_}]) ->
    ResType;
map_spec_res(_Args) -> 
    ?LOG(_Args),
    undefined.

% extract all arg and result types
bounded_type_tuples(TypeArgs) ->
    lists:map(fun ({type,_, _Constraint, 
                                [{_,_,_}, [{var,_,ArgName},{type,_,ArgType,_}]]}) -> {ArgName, ArgType};
        (_) -> undefined
        end, TypeArgs).

map_spec_args_from_bounded([{type,_,product,Args}|_Res], TypeTuples) ->
        lists:filtermap(fun({var,_,VarName}) ->
            case lists:keyfind(VarName, 1, TypeTuples) of
                false -> false;
                Tuple -> {true, Tuple}
            end
            end, Args);
map_spec_args_from_bounded(_, _) ->
    [].

map_spec_res_from_bounded([{type,_,product,_Args}|[{var, _, VarName}]], TypeTuples) ->
    case lists:keyfind(VarName, 1, TypeTuples) of
        {_, VarType} -> VarType;
        _ -> undefined        
    end;
map_spec_res_from_bounded(_, _) ->
    undefined.

get_generic_help(Module, Function, Index) ->
    Help = gen_lsp_help_server:get_help(Module, Function, Index, lsp_signature, eep48_render_signature),
    case Help of
        undefined -> <<>>;
        _ -> Help
    end.

eep48_render_signature(_Module, Function, FDocs, _Docs, Index) ->
    Grouping =
        lists:foldl(
          fun({Group,_Anno,_Sig,_Doc,#{ signature := Signature }} = _Func,Acc) ->
                  Signatures = maps:get(Signature, Acc, []),
                  Acc#{ Group => [Signature|Signatures] };
             ({_Group, _Anno, _Sig, _Doc, _Meta} = _Func, Acc) -> Acc
          end, #{}, lists:sort(FDocs)),
    MAP = lists:flatten(lists:map(
      fun({{_, _Fn, _FnArity} = _Group, Signatures}) ->
        SpecList = lists:map(fun ({attribute, _, spec, {{_,_},_}=FnSpec}) ->
                FnSpec
            end, lists:flatten(Signatures)),
        SpecList
      end, maps:to_list(Grouping))),
    spec_to_signatures(Function, Index, to_light_speclist(MAP)).

signatures_sample(Documentation) ->
    [
        #{
            label => <<"coming soon 3">>,
            documentation => #{
                kind => <<"markdown">>,
                value => doc_sample(Documentation)
            },
            parameters => [
                #{
                    label => <<"parameter 1 label">>,
                    documentation => <<"doc of parameter 1">>
                }
            ]
        },
        #{
            label => <<"coming soon 3">>,
            documentation => #{
                kind => <<"markdown">>,
                value => <<"# Header \n Some text \n ```typescript\nsomecode();\n```">>
            },
            parameters => [
                #{
                    label => <<"parameter 1 label">>,
                    documentation => <<"doc of parameter 1">>
                },
                #{
                    label => <<"parameter 2 label">>,
                    documentation => <<"doc of parameter 2">>
                }
            ]
        }
    ].

doc_sample(ExtraDoc) ->
    Result =
        "# Header \n Some text \n ```typescript\nsomecode();\n```" ++ lsp_utils:to_string(ExtraDoc),
    list_to_binary(Result).
