-module(lsp_syntax).

-export([validate_parsed_source_file/1, fold_in_syntax_tree/4,find_in_syntax_tree/2]).
-export([fold_in_syntax_tree/3]).

validate_parsed_source_file(File) ->
    FileSyntaxTree = gen_lsp_doc_server:get_syntax_tree(File),
    BehaviourModulea = behaviour_modules(FileSyntaxTree),
    ParseTranformModules = parse_transforms(FileSyntaxTree),
    ModulesToDelete = load_not_loaded_modules(BehaviourModulea ++ ParseTranformModules),
    NewFileSyntaxTree = parse_transform(FileSyntaxTree, ParseTranformModules),
    Result = lint(NewFileSyntaxTree, File),
    code_delete(ModulesToDelete),
    Result.

behaviour_modules(undefined) ->
    [];
behaviour_modules(FileSyntaxTree) ->
    lists:filtermap(fun 
        ({attribute, _, behaviour, Module}) -> {true, Module};
		(_) -> false
	end, FileSyntaxTree).

parse_transforms(undefined) ->
    [];
parse_transforms(FileSyntaxTree) ->
    lists:filtermap(fun 
        ({attribute, _, compile, {parse_transform, Module}}) -> {true, Module}; 
        (_) -> false
    end, FileSyntaxTree).

load_not_loaded_modules(Modules) ->
    lists:foldl(fun (Module, Acc) ->
        case code:is_loaded(Module) of
            false ->
                case find_source(Module) of
                    undefined ->
                        gen_lsp_server:lsp_log("cannot find module '~p'", [Module]),
                        Acc;
                    SourceFile ->
                        Options = [binary, report_errors, report_warningsk | [{i, Path} || Path <- lsp_parse:get_include_path(SourceFile)]],
                        case compile:file(SourceFile, Options) of
                            {ok, ModuleName, Binary} -> 
                                case code:load_binary(ModuleName, lists:flatten(io_lib:format("~p.beam", [ModuleName])), Binary) of
                                    {module, _} ->
                                        [Module | Acc];
                                    Error -> 
                                        gen_lsp_server:lsp_log("loading binary of compiled module '~p' failed: ~p", [SourceFile, Error]),
                                        Acc
                                end;
                            Error ->
                                gen_lsp_server:lsp_log("compilation of module '~p' failed: ~p", [SourceFile, Error]),
                                Acc
                        end
                end;
            _ ->
                Acc
        end
    end, [], Modules).

find_source(Module) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            BuildDir = filename:join(gen_lsp_config_server:root(), gen_lsp_doc_server:get_build_dir()),
            case filelib:wildcard(BuildDir ++ "/**/" ++ atom_to_list(Module) ++ ".erl") of
                [SourceFile | _] -> SourceFile;
                [] -> undefined
            end;
        SourceFile ->
            SourceFile
    end.

code_delete([Module | Modules]) ->
    case code:purge(Module) of
        true -> code:delete(Module);
        _ -> undefined
    end,
    code_delete(Modules);
code_delete([]) -> ok.

parse_transform(FileSyntaxTree, undefined) -> FileSyntaxTree;
parse_transform(FileSyntaxTree, Transformers) ->
    lists:foldl(fun(M, Syntax) -> 
            %call transform/2 on 'Transformer' module
            apply(M, parse_transform, [Syntax, []]) 
        end, 
    FileSyntaxTree, Transformers).
    %FileSyntaxTree.

lint(FileSyntaxTree, File) ->
    LintResult = case lsp_utils:is_erlang_lib_file(File) of
        false when FileSyntaxTree /= undefined ->
            erl_lint:module(FileSyntaxTree, File, [strong_validation, {feature, all, enable}]);
        _ ->
            {ok, []}
    end,
    case LintResult of
        {ok, []} ->
            #{parse_result => true};
        {ok, [Warnings]} ->
            ErrorsWarnings = extract_error_or_warning(<<"warning">>, filter_unused_functions(Warnings)),
	        #{parse_result => true, errors_warnings => ErrorsWarnings};
        {error, [Errors], []} ->
            ErrorsWarnings = extract_error_or_warning(<<"error">>, Errors),
	        #{parse_result => true, errors_warnings => ErrorsWarnings};
        {error, [Errors], [Warnings]} ->
            ErrorsWarnings = extract_error_or_warning(<<"error">>, Errors) ++
                    extract_error_or_warning(<<"warning">>, filter_unused_functions(Warnings)),
	        #{parse_result => true, errors_warnings => ErrorsWarnings};
        {error, [], [Warnings]} ->
            ErrorsWarnings = extract_error_or_warning(<<"warning">>, filter_unused_functions(Warnings)),
	        #{parse_result => true, errors_warnings => ErrorsWarnings};
        _Any ->
	        #{parse_result => false, error_message => <<"lint error">>}
    end.

filter_unused_functions({_, []}) ->
    [];
filter_unused_functions({File, Warnings}) ->
    %Filter unused function that ends with "_test", to avoid unwanted warnings in unit tests modules
    %%TODO: if more than one filter to exclude, it should be configurable
    Result = {
        File, 
        lists:filter(fun (X) ->
                case X of
                    {_, _, {unused_function, {FuncName,_}}} -> 
                        FuncNameStr = atom_to_list(FuncName),
                        FuncNameStrLen = length(FuncNameStr),
                        FindResult = FuncNameStrLen =< 4 orelse string:substr(FuncNameStr, FuncNameStrLen-4) =/= "_test",
                        %gen_lsp_server:lsp_log("FuncName:~p, ~p",[FuncName, FindResult]),
                        FindResult;
                    _ -> true 
                end 
            end,
            Warnings)
    }, 
    Result.

extract_error_or_warning(_Type, {_, []}) ->
    [];
extract_error_or_warning(Type, ErrorsOrWarnings) ->
    [#{type => Type,
        file =>
        erlang:list_to_binary(element(1, ErrorsOrWarnings)),
        info => extract_info(X)}
        || X <- element(2, ErrorsOrWarnings)].

extract_info({Line, Module, MessageBody}) when is_number(Line) ->
    extract_info({{Line, 1}, Module, MessageBody});
extract_info({{Line, Column}, Module, MessageBody}) ->
    % samples of X
    %{20,erl_parse,["syntax error before: ","load_xy"]}
    %{11,erl_lint,{undefined_function,{load_xy,1}}}]}
    #{
        line => Line, 
        character => Column,
        message => erlang:list_to_binary(lists:flatten(apply(Module, format_error, [MessageBody]), []))
    }.

fold_in_syntax_tree(Fun, StartAcc, File) ->
    fold_in_syntax_tree(Fun, StartAcc, File, gen_lsp_doc_server:get_syntax_tree(File)).
fold_in_syntax_tree(_Fun, StartAcc, _File, undefined) ->
    StartAcc;
fold_in_syntax_tree(Fun, StartAcc, File, SyntaxTree) ->
    {Result, _, _} = lists:foldl(fun (TopLevelElementSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun
                        ({attribute, _, file, {NewFile, _}} = Tree, {ValueAcc, _CurrentFile, undefined}) ->
                            {Fun(Tree, File, ValueAcc), File, NewFile};
                        ({attribute, _, file, {NewFile, _}} = Tree, {ValueAcc, _CurrentFile, FirstFile}) when NewFile =:= FirstFile ->
                            {Fun(Tree, File, ValueAcc), File, FirstFile};
                        ({attribute, _, file, {NewFile, _}} = Tree, {ValueAcc, _CurrentFile, FirstFile}) ->
                            {Fun(Tree, NewFile, ValueAcc), NewFile, FirstFile};
                        (Tree, {ValueAcc, CurrentFile, FirstFile}) ->
                            {Fun(Tree, CurrentFile, ValueAcc), CurrentFile, FirstFile}
        end, Acc, TopLevelElementSyntaxTree)
    end, {StartAcc, File, undefined}, SyntaxTree),
    Result.

find_in_syntax_tree(Fun, File) ->
    fold_in_syntax_tree(fun
        (SyntaxTree, CurrentFile, undefined) -> Fun(SyntaxTree, CurrentFile);
        (_SyntaxTree, _CurrentFile, Value) -> Value
    end, undefined, File, gen_lsp_doc_server:get_syntax_tree(File)).