-module(lsp_syntax).

-export([file_syntax_tree/1, module_syntax_tree/1, parse_config_file/2, parse_source_file/2, validate_parsed_source_file/1]).

parse_source_file(File, ContentsFile) ->
    case epp_parse_file(ContentsFile, get_include_path(File), get_define_from_rebar_config(File)) of
        {ok, FileSyntaxTree} ->
            UpdatedSyntaxTree = update_file_in_forms(File, ContentsFile, FileSyntaxTree),
            gen_lsp_doc_server:set_document_attribute(File, syntax_tree, UpdatedSyntaxTree),
            #{parse_result => true};
        _ ->
            #{parse_result => false, error_message => <<"Cannot open file">>}
    end.

validate_parsed_source_file(File) ->
    FileSyntaxTree = gen_lsp_doc_server:get_document_attribute(File, syntax_tree),
    ModuleToDelete = case should_load_behaviour_module(FileSyntaxTree) of
		undefined -> undefined;
		BehaviourModule -> load_behaviour_module(BehaviourModule)
	end,
    % apply parse_tranform compile directive if exists
    ParseTranformModulesToDelete = case should_load_parse_transform(FileSyntaxTree) of
		undefined -> undefined;
		ParseTranformsModule -> load_transform_modules(ParseTranformsModule)
	end,
    NewFileSyntaxTree = parse_transform(FileSyntaxTree, ParseTranformModulesToDelete),
    Result = lint(NewFileSyntaxTree, File),
    case ModuleToDelete of
        undefined -> void;
        _ -> code:delete(ModuleToDelete)
    end,
    case ParseTranformModulesToDelete of
        undefined -> void;
        _ -> code_delete(ParseTranformModulesToDelete)
    end,
    Result.

parse_config_file(File, ContentsFile) ->
    case file:path_consult(filename:dirname(ContentsFile), ContentsFile) of
      {ok,_, _} -> #{parse_result => true};
      {error, Reason} -> #{
            parse_result => true,
            errors_warnings => [#{type => <<"error">>,
            file => list_to_binary(File),
		    info => extract_info(Reason)}] }
    end.

file_syntax_tree(File) ->
    case gen_lsp_doc_server:get_document_attribute(File, syntax_tree) of
      undefined ->
	  case epp_parse_file(File, get_include_path(File), get_define_from_rebar_config(File)) of
	    {ok, FileSyntaxTree} -> FileSyntaxTree;
	    _Err -> throw("Cannot parse file " ++ File)
	  end;
      FileSyntaxTree ->
          FileSyntaxTree
    end.

module_syntax_tree(Module) ->
    File = gen_lsp_doc_server:get_module_file(Module),
    case File of
      undefined -> undefined;
      _ -> {file_syntax_tree(File), File}
    end.

update_file_in_forms(File, File, FileSyntaxTree) ->
    FileSyntaxTree;
update_file_in_forms(File, ContentsFile, FileSyntaxTree) ->
    lists:map(fun 
        ({attribute, A1, file, {FunContentsFile, A2}}) when FunContentsFile =:= ContentsFile ->
		      {attribute, A1, file, {File, A2}};
		  (Form) ->
              Form
	      end, FileSyntaxTree).

should_load_behaviour_module(FileSyntaxTree) ->
    BehaviourModule = lists:foldl(fun 
        ({attribute, _, behaviour, Module}, undefined) -> Module;
		(_, Acc) -> Acc
	end, undefined, FileSyntaxTree),
    case BehaviourModule of
        undefined -> undefined;
        _ ->
	        case code:is_loaded(BehaviourModule) of
	            false -> BehaviourModule;
	            _ -> undefined
	        end
    end.

load_behaviour_module(BehaviourModule) ->
    case gen_lsp_doc_server:get_module_file(BehaviourModule) of
        undefined -> undefined;
        SourceFile ->
        case lists:reverse(filename:split(SourceFile)) of
            [FilenameErl, "src" | T] ->
                RootBeamName = filename:join(lists:reverse([filename:rootname(FilenameErl), "ebin" | T])),
		        case code:load_abs(RootBeamName) of
		            {module, _} -> BehaviourModule;
		            _ -> undefined
		        end;
	        _ -> undefined
	    end
    end.

should_load_parse_transform(FileSyntaxTree) ->
    PasreTransform = lists:filtermap(fun (X) ->
            case X of
                {attribute, _, compile, {parse_transform, Module}} -> {true, Module}; 
                _ -> false
            end
		end,
		FileSyntaxTree),
    %gen_lsp_server:lsp_log("should_load_parse_transform: ~p",
	%		   [PasreTransform]),
    PasreTransform.

load_transform_module(TransformModule) ->
    case gen_lsp_doc_server:get_module_file(TransformModule) of
      undefined -> false;
      SourceFile -> 
        case compile:file(SourceFile, [binary]) of
            {ok, ModuleName, Binary} -> 
                case code:load_binary(ModuleName, lists:flatten(io_lib:format("~p.beam", [ModuleName])), Binary) of
                    {module, _} -> true;
                    Error -> 
                        gen_lsp_server:lsp_log("loading binary of parse_transform module '~p' failed: ~p",
                    		   [SourceFile, Error]),
                        false
                end;
            Error ->
                gen_lsp_server:lsp_log("compilation of parse_transform module '~p' failed: ~p",
                    		   [SourceFile, Error]),
                false
        end        
    end.
load_transform_modules(Modules) ->
    [M || M <- Modules, load_transform_module(M)].

code_delete([Module | Modules]) ->
    case code:purge(Module) of
        true -> code:delete(Module);
        _ -> undefined
    end,
    code_delete(Modules);
code_delete([]) -> ok.

epp_parse_file(File, IncludePath, Defines) ->
    case file:open(File, [read]) of
    {ok, FIO} ->
	    Ret = do_epp_parse_file(File, FIO, IncludePath, Defines),
	    file:close(FIO),
	    Ret;
    _Err -> 
        gen_lsp_server:lsp_log("epp_parse_file error : ~p", [_Err]),
        {error, file_could_not_opened}
    end.

do_epp_parse_file(File, FIO, IncludePath, Defines) ->
    case epp:open(as_string(File), FIO, {1,1}, IncludePath, Defines) of
        {ok, Epp} -> {ok, epp:parse_file(Epp)};
        {error, _Err} -> {error, _Err}
    end.

as_string(Text) when is_binary(Text) ->
    binary_to_list(Text);
as_string(Text) -> 
    Text.

get_include_path(File) ->
    Candidates = get_standard_include_paths() ++
		        get_settings_include_paths() ++
		        get_file_include_paths(File) ++
		        get_include_paths_from_rebar_config(File),
    Paths = lists:filter(fun filelib:is_dir/1, Candidates),
    gen_lsp_server:lsp_log("get_include_path: ~p", [Paths]),
    Paths.

get_standard_include_paths() ->
    RootDir = gen_lsp_config_server:root(),
    [
        filename:join([RootDir, "_build", "default", "lib"]),
        filename:join([RootDir, "_build", "default", "plugins"]),
        filename:join([RootDir, "apps"]),
        filename:join([RootDir, "lib"])
    ].

get_settings_include_paths() ->
    SettingPaths = gen_lsp_config_server:includePaths(),
    RootDir = gen_lsp_config_server:root(),
    lists:map(fun (Path) -> 
        abspath(RootDir, Path) 
    end, SettingPaths).

get_file_include_paths(File) ->
    Paths = [filename:dirname(File), filename:rootname(File)],
    case get_file_include_directory(File) of
        undefined -> 
            Paths;
        Path -> 
            [Path|Paths]
    end.

get_file_include_directory(File) ->
    case lists:reverse(filename:split(filename:dirname(File))) of
        [DirName|Rest] when DirName =:= "src" orelse DirName =:= "test" ->
	        filename:join(lists:reverse(["include"|Rest]));
        [_, DirName|Rest] when DirName =:= "src" orelse DirName =:= "test" ->
	        filename:join(lists:reverse(["include"|Rest]));
        _Other -> 
            undefined
    end.

get_define_from_rebar_config(File) ->
    RebarConfig = find_rebar_config(filename:dirname(File)),
    case RebarConfig of
        undefined -> 
            [];
        _ ->
	        Consult = file:consult(RebarConfig),
	        ErlOptsDefines = case Consult of
			    {ok, Terms} ->
				ErlOpts = proplists:get_value(erl_opts, Terms, []),
				 Defines = rebar_define_to_epp_define(proplists:lookup_all(d, ErlOpts)),
				 gen_lsp_server:lsp_log("get_defines: ~p", [Defines]),
				 Defines;
			     _ -> 
                     []
			   end,
	  DefaultDefines = [],
	  ErlOptsDefines ++ DefaultDefines
    end.

rebar_define_to_epp_define([]) -> 
    [];
rebar_define_to_epp_define([none]) -> 
    [];
rebar_define_to_epp_define([H|T]) ->
    case H of
      {d, Atom, Value} -> [{Atom, Value}] ++ rebar_define_to_epp_define(T);
      {d, Atom} -> [Atom] ++ rebar_define_to_epp_define(T);
      _ -> rebar_define_to_epp_define(T)
    end.

get_include_paths_from_rebar_config(File) ->
    RebarConfig = find_rebar_config(filename:dirname(File)),
    case RebarConfig of
      undefined ->
          [];
      _ ->
	  Consult = file:consult(RebarConfig),
	  ErlOptsPaths = case Consult of
			   {ok, Terms} ->
			       ErlOpts = proplists:get_value(erl_opts, Terms, []),
			       IncludePaths = proplists:get_all_values(i, ErlOpts),
			       lists:map(fun (Path) ->
						 filename:absname(Path, filename:dirname(RebarConfig))
					 end, IncludePaths);
			   _ -> 
                   []
			 end,
        Deps = [],
        %TODO: if include of each rebar dependency should be add to include paths, uncomment below 
    %   Deps = case Consult of
    %       {ok, DepsTerms} ->
    %         Profiles = proplists:get_value(profiles, DepsTerms, []),
    %         ErlDeps = lists:flatmap(fun({_,Opts})-> proplists:get_value(deps,Opts) end, Profiles),
    %         RootDir = gen_lsp_config_server:root(),
    %         [filename:join([RootDir, "_build", "default", "plugins", X, "include"]) || X <- ErlDeps ];
    %       _ -> 
    %         []
    %       end,
	  DefaultPaths = [filename:dirname(RebarConfig), filename:join([filename:dirname(RebarConfig), "include"])],
	  ErlOptsPaths ++ Deps ++ DefaultPaths
    end.

find_rebar_config(Dir) ->
    RebarConfig = filename:join(Dir, "rebar.config"),
    case filelib:is_file(RebarConfig) of
      true ->
          RebarConfig;
      _ ->
	  Elements = filename:split(Dir),
	  case Elements of
	    [_] ->
            undefined;
	    _ ->
		find_rebar_config(filename:join(lists:droplast(Elements)))
	  end
    end.

abspath(BaseDir, Path) ->
    case filename:pathtype(Path) of
        relative ->
            filename:absname_join(BaseDir, Path);
        _ ->
            Path
    end.


parse_transform(FileSyntaxTree, Transformers) ->
    lists:foldl(fun(M, Syntax) -> 
            %call transform/2 on 'Transformer' module
            apply(M, parse_transform, [Syntax, []]) 
        end, 
    FileSyntaxTree, Transformers).
    %FileSyntaxTree.


lint(FileSyntaxTree, File) ->
    %%no lint for erlang file under erlang lib dir
    LintResult = case lsp_utils:is_erlang_lib_file(File) of
        false -> erl_lint:module(FileSyntaxTree, File,[ {strong_validation} ]);
        _ -> {ok, []}
    end,
    case LintResult of
      % nothing wrong
      {ok, []} -> #{parse_result => true};
      % just warnings
      {ok, [Warnings]} ->
	  #{parse_result => true,
	    errors_warnings =>
		extract_error_or_warning(<<"warning">>, filter_unused_functions(Warnings))};
      % errors, no warnings
      {error, [Errors], []} ->
	  #{parse_result => true,
	    errors_warnings =>
		extract_error_or_warning(<<"error">>, Errors)};
      % errors and warnings
      {error, [Errors], [Warnings]} ->
	  #{parse_result => true,
	    errors_warnings =>
		extract_error_or_warning(<<"error">>, Errors) ++
		  extract_error_or_warning(<<"warning">>, filter_unused_functions(Warnings))};
      {error, [], [Warnings]} ->
	  #{parse_result => true,
	    errors_warnings =>
		extract_error_or_warning(<<"warning">>, filter_unused_functions(Warnings))};
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
                        FindResult = string:find(atom_to_list(FuncName), "_test", trailing) =/= "_test",
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
