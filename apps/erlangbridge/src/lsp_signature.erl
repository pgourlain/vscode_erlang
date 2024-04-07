-module(lsp_signature).

-export([signatures_sample/1]).
-export([eep48_render_signature/4]).
-export([disable_signature_help/0, signature_help_fromtokens/2]).

-define(LOG(S),
	begin
        gen_lsp_server:lsp_log("~p", [S])
	end).
-define(LOG(Fmt, Args),
	begin
        gen_lsp_server:lsp_log(Fmt, Args)
	end).


disable_signature_help() ->
    [].

signature_help_fromtokens(FileModule, Tokens) ->
    RTokens = lists:reverse(Tokens),
    M = lists:foldl(fun
        ({atom,_,_}=Token, #{argLevel := Level}=Acc) when Level == 0 ->
            HasModule = maps:get(hasModule, Acc), 
            if 
                HasModule == false -> Acc#{ function => erl_scan:symbol(Token)};
                true -> Acc#{ module => erl_scan:symbol(Token)}
            end;
        ({':',_}, #{argLevel := Level}=Acc)  when Level == 0  ->
            Acc#{ hasModule => true};
        ({'(',_}, #{argLevel := Level}=Acc) -> Acc#{argLevel => (Level-1)};
        ({')',_}, #{argLevel := Level}=Acc) -> Acc#{argLevel => (Level+1)};
        ({',',_}, #{argIndex := ArgIndex, argLevel := Level}=Acc) when Level == 1 -> Acc#{argIndex => (ArgIndex+1)};
        (_,Acc) -> Acc
        end, #{hasModule => false, argIndex => 0, argLevel=>1, function=>null, module => null}, RTokens),
    %?LOG(M),
    case M of
        #{ function := Fun, module := Module} when Fun=/=null -> 
            TargetModule = if Module == null -> FileModule;
                            true -> Module
                        end,
            D=function_signature(TargetModule, Fun, any),
             ?LOG(D),
            signatures_sample(io_lib:format("~p:~p",[TargetModule,Fun]));
        _ -> disable_signature_help()
    end.



function_signature(Module, Function, Arity) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            get_generic_help(Module, Function);
        File ->
            case lsp_utils:is_erlang_lib_file(File) of
                false ->
                    case lsp_navigation:function_clauses(File, Function, Arity) of
                        [] ->
                            case lists:keyfind(Function, 1, erlang:module_info(exports)) of
                                {Function, _} -> gen_lsp_help_server:get_help(erlang, Function, lsp_signature, eep48_render_signature);
                                _  -> <<>>
                            end;
                        ClausesList -> ClausesList
                            % lists:map(fun(Clauses) ->
                            %     edoc:get_doc(File, [{hidden, true}, {private, true}]),
                            %     end, ClausesList)
                            % iolist_to_binary(lists:map(fun (Clauses) ->
                            %     DocAsString = try edoc:layout(edoc:get_doc(File, [{hidden, true}, {private, true}]), 
                            %         [{layout, hover_doc_layout}, {filter, [{function, {Function, Arity}}]} ]) of
                            %             _Any -> _Any
                            %         catch
                            %             _Err:_Reason -> ""
                            %         end,                                                                        
                            %     FunctionHeaders = join_strings(lists:map(fun ({clause, _Location, Args, _Guard, _Body}) ->
                            %         function_header(Function, Args)
                            %     end, Clauses), "  \n") ++ "  \n" ++ DocAsString,
                            %     list_to_binary(FunctionHeaders)
                            % end, ClausesList))
                    end;
                _ ->
                    get_generic_help(Module, Function)
            end
    end.

get_generic_help(Module, Function) ->
    Help = gen_lsp_help_server:get_help_tokens(Module, Function),
    case Help of
        undefined -> <<>>;
        _ -> Help
    end.


eep48_render_signature(_Module, Function, FnDoc, Docs) ->
    % FnDoc = lists:filter(fun({{function, F, _},_Anno,_Sig,_Doc,_Meta}) ->
    %                          F =:= Function;
    %                     (_) ->
    %                          false
    %                  end, Docs),
    "eep48_render_signature".

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
    Result = "# Header \n Some text \n ```typescript\nsomecode();\n```"++lsp_utils:to_string(ExtraDoc),
    list_to_binary(Result).
