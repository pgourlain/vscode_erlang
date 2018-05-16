-module(lsp_syntax).

-export([parse_source_file/2, validate_parsed_source_file/1, parse_config_file/2, file_syntax_tree/1]).

parse_source_file(File, ContentsFile) ->
    case epp_parse_file(ContentsFile, get_include_path(File)) of
        {ok, FileSyntaxTree} ->
            gen_lsp_doc_server:add_or_update_document(File, update_file_in_forms(File, ContentsFile, FileSyntaxTree)),
            #{parse_result => true};
        _ ->
            #{parse_result => false, error_message => <<"Cannot open file">>}
    end.

validate_parsed_source_file(File) ->
    {ok, FileSyntaxTree} = gen_lsp_doc_server:get_document(File),
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
        {ok, FileSyntaxTree} ->
            FileSyntaxTree;
        not_found -> 
            case epp_parse_file(File, get_include_path(File)) of
                {ok, FileSyntaxTree} -> FileSyntaxTree;
                _ -> undefined
            end
    end.

%{attribute,1,file, {"C:\\Users\\WOJTEK~1.SUR\\AppData\\Local\\Temp\\3607772",1}}
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
    get_settings_include_paths() ++ [filename:dirname(File), filename:rootname(File) | get_include_path_from_rebar_config()].

get_settings_include_paths() ->
    string:tokens(maps:get(include_paths, gen_lsp_doc_server:get_config(), ""), "|").

get_include_path_from_rebar_config() ->
    RebarConfig = maps:get(rebar_config, gen_lsp_doc_server:get_config(), undefined),
    case RebarConfig of
        undefined ->
            [];
        _ ->
            Consult = file:consult(RebarConfig),
            PathsFromRebarConfig = case Consult of
                {ok, Terms} ->
                    ErlOpts = proplists:get_value(erl_opts, Terms, []),
                    IncludePaths = proplists:get_all_values(i, ErlOpts),
                    lists:map(fun (Path) ->
                        filename:absname(Path, filename:dirname(RebarConfig))
                    end, IncludePaths);
                _ ->
                    []
            end,
            LibDir = filename:join([filename:dirname(RebarConfig), "_build", "default", "lib"]),
            [LibDir | PathsFromRebarConfig]
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
