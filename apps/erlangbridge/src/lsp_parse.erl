-module(lsp_parse).
-export([parse_source_file/2, file_syntax_tree/1, module_syntax_tree/1, get_include_path/1, parse_config_file/2]).

parse_source_file(File, ContentsFile) ->
    case epp_parse_file(ContentsFile, get_include_path(File), get_define_from_rebar_config(File)) of
        {ok, FileSyntaxTree} ->
            UpdatedSyntaxTree = update_file_in_forms(File, ContentsFile, FileSyntaxTree),
            gen_lsp_doc_server:set_document_syntax_tree(File, UpdatedSyntaxTree),
            case epp_dodger:parse_file(ContentsFile) of
                {ok, Forms} -> gen_lsp_doc_server:set_document_dodged_syntax_tree(File, Forms);
                _ -> ok
            end,
            #{parse_result => true};
        _ ->
            #{parse_result => false, error_message => <<"Cannot open file">>}
    end.

file_syntax_tree(File) ->
    case gen_lsp_doc_server:get_document_syntax_tree(File) of
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

parse_config_file(File, ContentsFile) ->
    case file:path_consult(filename:dirname(ContentsFile), ContentsFile) of
      {ok,_, _} -> #{parse_result => true};
      {error, Reason} -> #{
            parse_result => true,
            errors_warnings => [#{type => <<"error">>,
            file => list_to_binary(File),
		    info => extract_info(Reason)}] }
    end.

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
        lsp_utils:absolute_path(RootDir, Path) 
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

epp_parse_file(File, IncludePath, Defines) ->
    case otp_24_or_newer() of
        true ->
            case epp:parse_file(as_string(File), [{includes, IncludePath}, {macros, Defines}, {location, {1, 1}}]) of
                {ok, Result} ->
                    {ok, Result};
                _Err -> 
                    gen_lsp_server:lsp_log("epp_parse_file error : ~p", [_Err]),
                    {error, file_could_not_parsed}
            end;
        false ->
            case file:open(File, [read]) of
                {ok, FIO} ->
                    Ret = do_epp_parse_file(File, FIO, IncludePath, Defines),
                    file:close(FIO),
                    Ret;
                _Err -> 
                    gen_lsp_server:lsp_log("epp_parse_file error : ~p", [_Err]),
                    {error, file_could_not_opened}
            end
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

update_file_in_forms(File, File, FileSyntaxTree) ->
    FileSyntaxTree;
update_file_in_forms(File, ContentsFile, FileSyntaxTree) ->
    lists:map(fun 
        ({attribute, A1, file, {FunContentsFile, A2}}) when FunContentsFile =:= ContentsFile ->
		      {attribute, A1, file, {File, A2}};
		  (Form) ->
              Form
	      end, FileSyntaxTree).

otp_24_or_newer() ->
    VersionNumber = (catch list_to_integer(erlang:system_info(otp_release))),
    is_integer(VersionNumber) andalso VersionNumber >= 24.
