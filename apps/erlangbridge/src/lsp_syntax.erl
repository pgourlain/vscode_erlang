-module(lsp_syntax).

-export([validate_parsed_source_file/1, fold_in_syntax_tree/4,find_in_syntax_tree/2]).
-export([fold_in_syntax_tree/3, get_macros/1]).

-include("lsp_log.hrl").
-define(LINTER, <<"erl lsplint">>).

validate_parsed_source_file(File) ->
    FileSyntaxTree = gen_lsp_doc_server:get_syntax_tree(File),
    BehaviourModulea = behaviour_modules(FileSyntaxTree),
    ParseTranformModules = parse_transforms(FileSyntaxTree, File),
    ModulesToDelete = load_not_loaded_modules(BehaviourModulea ++ ParseTranformModules),
    NewFileSyntaxTree = parse_transform(FileSyntaxTree, ParseTranformModules),
    IncludeDirectiveLines = include_directive_lines(gen_lsp_doc_server:get_dodged_syntax_tree(File)),
    Result = lint(NewFileSyntaxTree, File, IncludeDirectiveLines),
    % Too long and bad effect on performance with large project
    %Result = combine_lint(ErlLintResult, NewFileSyntaxTree, File),
    code_delete(ModulesToDelete),
    Result.

behaviour_modules(undefined) ->
    [];
behaviour_modules(FileSyntaxTree) ->
    lists:filtermap(fun 
        ({attribute, _, behaviour, Module}) -> {true, Module};
		(_) -> false
	end, FileSyntaxTree).

%% @doc Modules to run as parse transforms before linting: the
%% `-compile({parse_transform, M})` attribute in the file itself, unioned
%% with `{parse_transform, M}` declared in `erl_opts` in rebar.config, which
%% carries no attribute in the module at all (#216).
parse_transforms(FileSyntaxTree, File) ->
    lists:usort(parse_transforms_attribute(FileSyntaxTree) ++
        lsp_parse:get_parse_transforms_from_rebar_config(File)).

parse_transforms_attribute(undefined) ->
    [];
parse_transforms_attribute(FileSyntaxTree) ->
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
                        Options = [binary, report_errors, report_warnings | [{i, Path} || Path <- lsp_parse:get_include_path(SourceFile)]],
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

lint(FileSyntaxTree, File, IncludeDirectiveLines) ->
    LintResult = case lsp_utils:is_erlang_lib_file(File) of
        false when FileSyntaxTree /= undefined ->
            erl_lint:module(FileSyntaxTree, File, [strong_validation, {feature, all, enable}]);
        _ ->
            {ok, []}
    end,
    %% erl_lint groups what it reports by file: the module itself, plus one
    %% group per included file that has something to say.
    %% The module is whatever file epp started from - named in the tree's first
    %% -file attribute - rather than File, which differs from it should the
    %% tree still carry the name of a temporary copy of the buffer.
    Main = main_file(File, FileSyntaxTree),
    IncludeLines = include_lines(Main, FileSyntaxTree, IncludeDirectiveLines),
    Extract = fun (Type, Groups) ->
        lists:flatmap(fun (Group) ->
            extract_group(Type, Main, IncludeLines, filter_unused_functions(Group))
        end, Groups)
    end,
    case LintResult of
        {ok, Warnings} ->
            lint_result(Extract(<<"warning">>, Warnings));
        {error, Errors, Warnings} ->
            lint_result(Extract(<<"error">>, Errors) ++ Extract(<<"warning">>, Warnings));
        _Any ->
            #{parse_result => false, error_message => <<"lint error">>}
    end.

main_file(_File, [{attribute, _, file, {Main, _}} | _]) -> Main;
main_file(File, _Forms) -> File.

lint_result([]) -> #{parse_result => true};
lint_result(ErrorsWarnings) -> #{parse_result => true, errors_warnings => ErrorsWarnings}.

extract_group(_Type, _File, _IncludeLines, []) ->
    [];
extract_group(Type, File, _IncludeLines, {File, _} = Group) ->
    extract_error_or_warning(Type, Group);
extract_group(Type, _File, IncludeLines, {IncludedFile, Infos}) ->
    %% A problem in an included file has no place of its own in the module:
    %% it goes on the -include line that brought the file in, says where it
    %% really is, and links there. Its positions are the header's, so it
    %% carries no correlation data - a quick fix would edit the module at them.
    IncludeLine = maps:get(IncludedFile, IncludeLines, 1),
    [begin
        #{line := Line, character := Column, message := Message} = extract_info(Info),
        Prefix = unicode:characters_to_binary(
                     io_lib:format("~ts:~p:~p: ", [filename:basename(IncludedFile), Line, Column])),
        #{type => Type,
          file => unicode:characters_to_binary(IncludedFile),
          info => #{line => IncludeLine, character => 1,
                    message => <<Prefix/binary, Message/binary>>},
          related => #{file => IncludedFile, line => Line, character => Column, message => Message}}
     end || Info <- Infos].

%% @doc Every file included by File, directly or through another header,
%% mapped to the line of the -include in File that brought it in. epp marks
%% entering an included file with -file(Included, _) and coming back with
%% -file(File, L), L being where File resumes - the line after the -include
%% when its `.` is followed by a bare newline, but the -include line itself
%% with CRLF line endings, a comment after the `.`, or no final newline. So L
%% only bounds it: the -include is the last one at or before L, read from the
%% epp_dodger tree, which keeps the directives epp expands.
include_lines(_File, undefined, _IncludeDirectiveLines) ->
    #{};
include_lines(File, Forms, IncludeDirectiveLines) ->
    {Lines, _Pending} = lists:foldl(fun
        ({attribute, _, file, {F, L}}, {Acc, Pending}) when F =:= File ->
            IncludeLine = include_line_before(L, IncludeDirectiveLines),
            {lists:foldl(fun (Included, In) ->
                             maps:merge(#{Included => IncludeLine}, In)
                         end, Acc, Pending), []};
        ({attribute, _, file, {F, _}}, {Acc, Pending}) ->
            {Acc, [F | Pending]};
        (_, Acc) ->
            Acc
    end, {#{}, []}, Forms),
    Lines.

include_line_before(L, IncludeDirectiveLines) ->
    case [Line || Line <- IncludeDirectiveLines, Line =< L] of
        [] -> max(L - 1, 1); % no dodged tree: assume a bare newline
        Before -> lists:last(Before)
    end.

%% Sorted lines of the -include and -include_lib directives in a dodged tree.
include_directive_lines(undefined) ->
    [];
include_directive_lines(DodgedForms) ->
    lists:usort(lists:filtermap(fun (Form) ->
        try erl_syntax:type(Form) =:= attribute andalso
                lists:member(erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
                             [include, include_lib]) of
            true -> {true, erl_anno:line(erl_syntax:get_pos(Form))};
            false -> false
        catch _:_ -> false
        end
    end, DodgedForms)).

% combine_lint(LintResult, SyntaxTree, File) ->
%     case LintResult of
%         #{parse_result := ParseResult, errors_warnings := ErrorsWarnings} 
%             when ParseResult =:= true -> 
%               % combine only if previous parse is successfull
%               #{
%                 parse_result => ParseResult, 
%                 errors_warnings => lsp_lint(ErrorsWarnings, SyntaxTree, File)
%             };
%         _ -> LintResult
%     end.

% lsp_lint(PreviousLintResult, SyntaxTree, File) ->
%     RootWorkspace = gen_lsp_config_server:root(),
%     %%process remote call
%     fold_in_syntax_tree(fun
%         ({call, {_, _}, {remote, {_, _}, {atom, {Line, Column}, FnModule}, {atom, {_, _}, FnName}}, Args}, _CurrentFile, Acc)
%             -> 
%                 % Incr = 1 for last right parenthesis
%                 Acc ++ check_if_remote_fun_exists(RootWorkspace, FnModule, FnName, length(Args), range_of(Line, Column, FnModule, FnName, Args, 1));
%         ({'fun', {_, _}, {function, {atom, {Line, Column}, FnModule}, {atom, {_,_}, FnName}, {integer, {_, End}, FnArity}}}, _CurrentFile, Acc)
%              ->
%             Width = length(lsp_utils:to_string("~w",[FnArity])),
%             Acc ++ check_if_remote_fun_exists(RootWorkspace, FnModule, FnName, FnArity,{Line, Column, Line, End + Width});

%     (_, _, Acc) -> Acc
%     end,
%     PreviousLintResult, File, SyntaxTree).

% range_of(Line, Column, MName, FName, Args, Incr) ->
%     {L,C, NewIncr} = if 
%         length(Args) > 0 -> max_lc_root(lists:last(Args), {Line, Column, Incr});
%         true -> 
%             % +2 for parenthesis
%             {Line, Column, Incr + 2 + length(atom_to_list(MName)) +length(atom_to_list(FName))}
%     end,
%    {Line, Column, L, C+NewIncr}.


% max_lc_root({_,_,Atom}=Item, {Line, Column, Incr}) when is_atom(Atom) ->
%     max_lc(Item, {Line, Column, Incr + length(atom_to_list(Atom))});
% max_lc_root({_,_,Int}=Item, {Line, Column, Incr}) when is_number(Int) ->
%     ArgLength = length(lsp_utils:to_string("~w",[Int])),
%     max_lc(Item, {Line, Column, Incr + ArgLength});
% max_lc_root(Item, Acc) ->
%     max_lc(Item, Acc).

% -spec max_lc(Item:: term(), Acc :: term()) -> Acc1 :: term().
% max_lc({_, {L,C}, _}, {CL, CC, Incr}) when L > CL orelse (L =:= CL andalso C >= CC) ->
%     {L, C, Incr};
% max_lc({_, {_,_}, Item}, Acc) ->
%     max_lc(Item, Acc);
% max_lc({call, {L,C}, Remote, Args}, {CL, CC, Incr}) when L > CL orelse (L =:= CL andalso C >= CC) ->
%     if 
%         length(Args) > 0 -> max_lc_root(lists:last(Args), {L, C, Incr+1});
%         true -> 
%             case Remote of
%                 {remote, _, {atom, _, _}, {atom,{RL, RC}, Atom}} -> {RL, RC, Incr + 1 + length(atom_to_list(Atom))};
%                 _ -> {L, C, Incr}
%             end            
%     end;
% max_lc(_, Acc) -> 
%     Acc.

%check_if_remote_fun_exists(RootWorkspace, FnModule, FnName, FnArity, {Line, Column, LE,LC}) ->
%    % check if module is under workspace
%    case gen_lsp_doc_server:get_module_file(FnModule) of
%        undefined -> [];
%        TargetFile ->  
%            case string:str(TargetFile, RootWorkspace) of
%                1 ->
%                    case available_functions(TargetFile, FnName, FnArity) of
%                        {[],[],[]} -> % no match => function doesn't exists
%                            [
%                            #{info =>
%                                    #{line => Line,
%                                    message => lsp_utils:to_binary("function ~p:~p/~p undefined", [FnModule,FnName,FnArity]),
%                                    character => Column,
%                                    line_end => LE,
%                                    character_end => LC},
%                                type => <<"error">>,
%                                file => lsp_utils:to_binary(TargetFile),
%                                source => ?LINTER,
%                                correlation_data => #{ action => <<"create_function">>, arguments => [FnModule, FnName, FnArity]}
%                            }
%                            ];
%
%                        {MatchFns, _, _} when length(MatchFns) > 0 -> % function is found, so no error
%                            [];
%                        {_, DefMatchFns, _} when length(DefMatchFns) > 0 -> % function is found, but not exported
%                            [
%                            #{info =>
%                                    #{line => Line,
%                                    message =>
%                                        lsp_utils:to_binary(lsp_utils:to_string("function ~s:~s/~p  is missing in export.",[FnModule, FnName, FnArity])),
%                                    character => Column,
%                                    line_end => LE,
%                                    character_end => LC},
%                                type => <<"error">>,
%                                file => lsp_utils:to_binary(TargetFile),
%                                source => ?LINTER,
%                                correlation_data => #{ action => <<"export_function">>, arguments => [FnModule, FnName, FnArity]}
%                            }
%                            ];
%                        {[], [], NotMatchFns} when length(NotMatchFns) > 0 -> 
%                            %funtions with other arity
%                            AvailableFns = lists:flatten(lists:join(",",
%                                lists:filtermap(fun 
%                                    ({exported, Fn, Fa}) -> {true, lsp_utils:to_string("~s/~p\n",[Fn, Fa])};
%                                    (_) -> false
%                                end, NotMatchFns))),
%                            Message = lsp_utils:to_binary("function ~s:~s/~p arity mismatch.\navailable arity are :\n ~s\n",
%                                                        [FnModule, FnName, FnArity, AvailableFns]),
%                            [
%                            #{info =>
%                                    #{line => Line,
%                                    message => Message,
%                                    character => Column,
%                                    line_end => LE,
%                                    character_end => LC},
%                                type => <<"error">>,
%                                file => lsp_utils:to_binary(TargetFile),
%                                source => ?LINTER
%                            }];
%
%                        _ -> []
%                    end;
%                _ -> []
%            end     
%    end.

% get functions that match and nearly match (by arity)
%available_functions(File, FunName, FunArity) ->
%    Functions =fold_in_syntax_tree(fun
%            ({function, {_, _}, FName, FnArity, _}, _CurrentFile, {M, DM, NM}) when FName =:= FunName, FnArity =:= FunArity ->
%                {M, [{definition_match, FName, FunArity} | DM], NM};
%            ({function, {_, _}, FName, Arity, _}, _CurrentFile, {M, DM, NM}) when FName =:= FunName ->
%                {M, DM, [{definition, FName, Arity} | NM]};
%            ({attribute, {_L, _C}, export, Exports}, _CurrentFile, {M, DM, NM}=Acc) ->
%            case lists:keyfind(FunName, 1, Exports)  of
%                {_, Arity} when Arity =:= FunArity -> {[{exported_match, FunName, FunArity} | M], DM, NM};
%                {_, Arity} -> {M, DM, [{exported, FunName, Arity} | NM]};
%                _ -> Acc
%            end; 
%            (_SyntaxTree, _CurrentFile, Acc) ->
%                Acc
%        end,
%        {[], [], []}, File),
%        Functions.

filter_unused_functions({_, []}) ->
    [];
filter_unused_functions({File, Warnings}) ->
    %% Filter unused-function warnings for names EUnit exports on our
    %% behalf: plain tests ("foo_test"), generators ("foo_test_", #89), and
    %% the generated "_assert*" helpers from assert macros.
    %%TODO: if more than one filter to exclude, it should be configurable
    Result = {
        File,
        lists:filter(fun (X) ->
                case X of
                    {_, _, {unused_function, {FuncName,_}}} ->
                        FuncNameStr = atom_to_list(FuncName),
                        not (lists:suffix("_test", FuncNameStr) orelse
                             lists:suffix("_test_", FuncNameStr) orelse
                             lists:prefix("_assert", FuncNameStr));
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
        unicode:characters_to_binary(element(1, ErrorsOrWarnings)),
        info => extract_info(X),
        correlation_data => correlation_data(X)}
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
        message => unicode:characters_to_binary(apply(Module, format_error, [MessageBody]))
    }.

%% @doc Task 2.1 infrastructure: a JSON-safe, machine-readable identity for
%% the diagnostic, echoed back verbatim by the client as
%% context.diagnostics[].data on a later textDocument/codeAction request -
%% so a quick fix (task 2.2+) can act on the *exact* erl_lint/erl_parse
%% term that produced the diagnostic without re-running any analysis.
%% Generic on purpose: it does not know about any specific lint check.
correlation_data({Line, Module, MessageBody}) when is_number(Line) ->
    correlation_data({{Line, 1}, Module, MessageBody});
correlation_data({{_Line, _Column}, Module, MessageBody}) ->
    #{module => Module, messageBody => to_json_safe(MessageBody)}.

%% Recursively reshape an arbitrary Erlang term (as found in erl_lint/
%% erl_parse error/warning info) into something vscode_jsone can encode:
%% tuples become arrays, printable char lists become binaries, anything
%% else unrecognized is dumped via ~p as a last resort.
to_json_safe(Tuple) when is_tuple(Tuple) ->
    [to_json_safe(E) || E <- tuple_to_list(Tuple)];
to_json_safe(List) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true -> unicode:characters_to_binary(List);
        false -> [to_json_safe(E) || E <- List]
    end;
to_json_safe(Atom) when is_atom(Atom) ->
    Atom;
to_json_safe(Number) when is_number(Number) ->
    Number;
to_json_safe(Binary) when is_binary(Binary) ->
    Binary;
to_json_safe(Other) ->
    unicode:characters_to_binary(io_lib:format("~p", [Other])).

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

% get all macros from syntax tree
-spec get_macros(DodgedSyntaxTree :: erl_syntax:syntaxtree()) -> { erl_anno:location(), string(), integer() }.
get_macros(DodgedSyntaxTree) ->
    lists:foldl(fun (TopLevelElementSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun find_macros_in_file/2, Acc, TopLevelElementSyntaxTree)
    end, [], DodgedSyntaxTree).


find_macros_in_file({tree, macro, {attr,LC,_,_},
             {macro,{tree,variable, _, MacroName}, _}}, Acc) ->
    Acc ++ [{LC, MacroName, length(atom_to_list(MacroName))}];
find_macros_in_file(_, Acc) ->
    Acc.