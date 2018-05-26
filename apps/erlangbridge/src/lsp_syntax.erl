-module(lsp_syntax).

-export([parse_source_file/2, validate_parsed_source_file/1, parse_config_file/2, file_syntax_tree/1, module_syntax_tree/1, find_module_file/2]).

parse_source_file(File, ContentsFile) ->
    case epp_parse_file(ContentsFile, get_include_path(File)) of
        {ok, FileSyntaxTree} ->
            UpdatedSyntaxTree = update_file_in_forms(File, ContentsFile, FileSyntaxTree),
            {_Result, Contents} = file:read_file(ContentsFile),
            gen_lsp_doc_server:add_or_update_document(File, {UpdatedSyntaxTree, Contents}),
            #{parse_result => true};
        _ ->
            #{parse_result => false, error_message => <<"Cannot open file">>}
    end.

validate_parsed_source_file(File) ->
    {ok, {FileSyntaxTree, _Contents}} = gen_lsp_doc_server:get_document(File),
    lint(FileSyntaxTree, File).

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
    case gen_lsp_doc_server:get_document(File) of
        {ok, {FileSyntaxTree, _Contents}} ->
            FileSyntaxTree;
        not_found -> 
            case epp_parse_file(File, get_include_path(File)) of
                {ok, FileSyntaxTree} ->
                    FileSyntaxTree;
                _ -> undefined
            end
    end.

module_syntax_tree(Module) ->
    File = find_module_file(Module, maps:get(root, gen_lsp_doc_server:get_config(), "")),
    case File of
        undefined -> undefined;
        _ -> {file_syntax_tree(File), File}
    end.

find_module_file(Module, RootDir) ->
    Files = filelib:fold_files(RootDir, atom_to_list(Module) ++ ".erl", true, fun (Found, Acc) ->
        case list_to_atom(filename:rootname(filename:basename(Found))) of
            Module -> [Found | Acc];
            _ -> Acc
        end
    end, []),
    case Files of
        [] ->
            undefined;
        [OneFile] ->
            OneFile;
        [AFile|_] ->
            BuildElements = filename:split(RootDir) ++ ["_build"],
            NoBuildFiles = lists:filter(fun (Filename) ->
                not lists:prefix(BuildElements, filename:split(Filename))
            end, Files),
            case NoBuildFiles of
                [ANoBuioldFile|_] ->
                    ANoBuioldFile;
                _ ->
                    AFile
            end
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

epp_parse_file(File, IncludePath) ->
    case file:open(File, [read]) of
    {ok, FIO} -> 
        Ret = do_epp_parse_file(File, FIO, IncludePath),
        file:close(FIO), 
        Ret;
    _ -> {error, file_could_not_opened}
    end.

do_epp_parse_file(File, FIO, IncludePath) ->
    case epp:open(File, FIO, {1,1}, IncludePath, []) of
        {ok, Epp} -> {ok, epp:parse_file(Epp)};
        {error, _Err} -> {error, _Err} 
    end.

get_include_path(File) ->
    Candidates = get_standard_include_paths() ++
                get_settings_include_paths() ++
                get_file_include_paths(File) ++
                get_include_paths_from_rebar_config(File),
    Paths = lists:filter(fun filelib:is_dir/1, Candidates),
    error_logger:error_msg("get_include_path: ~p", [Paths]),
    Paths.

get_standard_include_paths() ->
    RootDir = maps:get(root, gen_lsp_doc_server:get_config(), ""),
    [
        filename:join([RootDir, "_build", "default", "lib"]),
        filename:join([RootDir, "apps"]),
        filename:join([RootDir, "lib"])
    ].

get_settings_include_paths() ->
    SettingPaths = maps:get(includePaths, gen_lsp_doc_server:get_config(), []),
    RootDir = maps:get(root, gen_lsp_doc_server:get_config(), ""),
    lists:map(fun (Path) ->
        abspath(RootDir, Path)
    end, SettingPaths).

get_file_include_paths(File) ->
    [filename:dirname(File), filename:rootname(File)].

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
            DefaultPaths = [filename:dirname(RebarConfig), filename:join([filename:dirname(RebarConfig), "include"])],
            ErlOptsPaths ++ DefaultPaths
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

lint(FileSyntaxTree, File) ->
    LintResult = erl_lint:module(FileSyntaxTree, File,[ {strong_validation}]),
    %error_logger:info_msg("lint result '~p'",[LintResult]),
    case LintResult of
    % nothing wrong
    {ok, []} -> #{parse_result => true};
    % just warnings
    {ok, [Warnings]} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"warning">>, Warnings)};
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
        extract_error_or_warning(<<"warning">>, Warnings)};
    {error, [], [Warnings]} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"warning">>, Warnings)};
    _Any ->
        #{parse_result => false, error_message => <<"lint error">>}
    end.

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
