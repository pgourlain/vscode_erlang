-module(lsp_inlayhints).

-export([inlayhint_analyze/3, generate_inlayhints/2]).

-define(LOG(S),
	begin
        gen_lsp_server:lsp_log("~p", [S])
	end).
-define(LOG(Fmt, Args),
	begin
        gen_lsp_server:lsp_log(Fmt, Args)
	end).


inlayhint_analyze(SyntaxTree, _CurrentFile, #{defs := Defs, calls := Calls} = Dict) ->
    case SyntaxTree of
        %TODO, get args from spec if exists
        {function, _Location, FuncName, Arity, Content} when Arity > 0 ->
            F = #{
                func_name => FuncName,
                arity => Arity,
                args => extract_function_args(Content)
            },
            maps:put(defs, Defs ++ [F], Dict);
        {call, _LocationCall, {atom, _, FName}, Args} when length(Args) > 0 ->
            %sample Args
            %functionName:test_literal_guard, args: [{integer,{11,24},5}]
            %functionName:test_literal_guard), args: [{var,{12,24},'B'}]
            F = #{
                func_name => FName,
                args => index_args(Args)
            },
            maps:put(calls, Calls ++ [F], Dict);
       % {remote,{32,15},{atom,{32,5},sample_lib},{atom,{32,16},fn_utils1}}
       %{call,{32,5},{remote,{32,15},{atom,{32,5},sample_lib}, {atom,{32,16},fn_utils1}},[{var,{32,26},'X'}]}
       {call, _LocationCall, {remote, _, {atom, _,ModuleName}, {atom, _, FName}}, Args} ->
            %to avoid collision with local function name, add module as prefix 
            FuncName = lists:flatten(io_lib:format("~s.~s", [ModuleName, FName])),
            case get_remote_function_content(ModuleName, FName) of
                {true, RemoteFuns} ->
                    F = #{
                        func_name => FuncName,
                        args => index_args(Args)
                    },
                    NewDict = maps:put(calls, Calls ++ [F], Dict),
                    FDefs = [#{
                        func_name => FuncName,
                        arity => Arity,
                        args => extract_function_args(Content)
                    } || {Arity, Content} <- RemoteFuns],
                    %?LOG("remote_functions:~p <==> ~p",[F, FDefs]),
                    maps:put(defs, Defs ++ FDefs, NewDict);
                    
            _ -> Dict
            end;
        _Other ->
            Dict
    end.

get_remote_function_content(ModuleName, FName) ->
    SModuleName = lsp_utils:to_string(ModuleName),
    FilteredModules = lists:filter(fun (X) -> X =:= SModuleName end, 
    gen_lsp_doc_server:project_modules()),
    case FilteredModules of
        [] -> undefined;
        [_FindModule] ->
            case gen_lsp_doc_server:get_module_file(ModuleName) of
            undefined -> undefined;
            SourceFile ->
                %check is file under workspace
                Functions = lsp_navigation:functions(SourceFile, FName),
                {true, [{Arity, Content} || 
                    {function, _, _FName, Arity, Content} <- Functions]}
            end;
        _ -> undefined
    end.    

index_args(Args) ->
    %%add index for each arg, will use later to match with definition
    {LR, _} = lists:mapfoldl(fun(A, Acc) -> {{Acc, A}, Acc + 1} end, 0, Args),
    LR.

extract_function_args([]) ->
    [];
extract_function_args(Clauses) ->
    ArgsList = lists:filtermap(fun (X) ->
        case X of
            {clause, _, Args, _, _} -> {true, Args};
            _ -> false
         end
        end,  Clauses),
    zip_args(ArgsList).

%% match the better human readable args, by skipping '_xxx' var names
zip_args([]) -> [];
zip_args([Args]) -> 
    Args;
zip_args([Args1,Args2|Tail]) -> 
    %?LOG("zip_args(~p,~p)",[Args1, Args2]),
    Res = lists:zipwith(fun (Arg1,Arg2) ->
        %compare two args, if var is '_' => skip it
        case Arg1 of
            {var, _, VarName} -> choose_better_arg(lsp_utils:to_string(VarName), Arg1, Arg2);
            _ -> Arg2
        end
        end, Args1, Args2),
    zip_args([Res]++Tail).

choose_better_arg(VarName, Arg1, Arg2) ->
    F = string:find(VarName, "_"),
    if 
        F =:= VarName -> Arg2;
        true -> Arg1
    end.


generate_inlayhints([], _Defs) ->
    [];
generate_inlayhints([#{args := Args, func_name := FName} | RestCalls], Defs) ->
    L = length(Args),
    %% filter calls and definitions by arity and name
    case
        lists:filter(
            fun(#{arity := Arity, func_name := Dfn}) -> Arity =:= L andalso FName =:= Dfn end, Defs
        )
    of
        [#{args := DArgs}] -> filter_and_map_args(Args, DArgs);
        _ -> []
    end ++
        generate_inlayhints(RestCalls, Defs).

filter_and_map_args([], _Defs) ->
    [];
filter_and_map_args([{Index, Call} | RestCalls], Defs) ->
    %get corresponding argument in definition
    D = lists:nth(Index + 1, Defs),
    %?LOG("try_match:~p, ~p", [Call, D]),
    try_match_parameter(Call, D) ++ filter_and_map_args(RestCalls, Defs).

try_match_parameter({var, _, VarName}, {var, _, DefVarName}) when VarName =:= DefVarName ->
    % call var name and definition var name are equal, no inlay for this argument
    [];
try_match_parameter({_, Position, _}, DefArg) ->
   new_inlay(Position, DefArg);
try_match_parameter({call, Position, _,_}, DefArg)  ->
   new_inlay(Position, DefArg);
try_match_parameter(_, _)  ->
    [].

new_inlay(Position, DefArg) ->
    Label = case DefArg of
        {match, _, _,_} ->  "match: ";
        {map, _,_} -> "map: ";
        {cons, _,_,_} -> "cons: ";
        {var, _, DefVarName} -> lsp_utils:to_string(DefVarName) ++ ": ";
        _ -> false
    end,
    if
        Label =:= false -> [];
        true ->
            [{
            Position,
            Label,
            "parameter"
            }]
    end.
