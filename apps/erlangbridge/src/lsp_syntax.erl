-module(lsp_syntax).

-export([parse/1, lint/2, parse_src_file/1, parse_and_lint/1]).

parse_and_lint(File) ->
    do_parse_and_lint(File).

do_parse_and_lint(File) ->
    case parse(File) of
      {ok, Forms} -> lint(Forms, File);
      {error, _} ->
	  #{parse_result => false,
	    error_message => <<"Cannot open file">>}
    end.

parse(File) ->
    %error_logger:info_msg("parse '~p'",[File]),
    case epp_parse_file(File) of
    {ok, Forms} ->
        gen_lsp_doc_server:add_or_update_document(File, Forms),
        {ok, Forms};
    {error, Any} -> {error, Any};
    _Other -> error_logger:info_msg("parse_file other case '~p'",[_Other]), {error, "unknown"}
    end.

epp_parse_file(File) ->
    case file:open(File, [read]) of
    {ok, FIO} -> 
        Ret = do_epp_parse_file(File, FIO),
        file:close(FIO), 
        %error_logger:info_msg("epp_parse_file '~p'",[Ret]),
        Ret;
    _ -> {error, file_could_not_opened}
    end.

do_epp_parse_file(File, FIO) ->
    case epp:open(File, FIO, {1,1}, get_include_path(File), []) of
    {ok, Epp} -> {ok, epp:parse_file(Epp)};
    {error, _Err} -> {error, _Err} 
    end.

get_include_path(File) ->
    RebarConfig = lsp_navigation:find_rebar_config(File),
    case RebarConfig of
        undefined ->
            [];
        _ ->
            Consult = file:consult(RebarConfig),
            case Consult of
                {ok, Terms} ->
                    ErlOpts = proplists:get_value(erl_opts, Terms, []),
                    IncludePaths = proplists:get_all_values(i, ErlOpts),
                    lists:map(fun (Path) ->
                        filename:absname(Path, filename:dirname(RebarConfig))
                    end, IncludePaths);
                _ ->
                    []
            end
    end.

lint(Forms, File) ->
    LintResult = erl_lint:module(Forms, File,[ {strong_validation}]),
    error_logger:info_msg("lint result '~p'",[LintResult]),
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

parse_src_file(File) ->
    case file:path_consult(filename:dirname(File), File) of
    {ok,_, _} -> #{parse_result => true};
    {error, Reason} -> #{
        parse_result => true,
		errors_warnings => [#{type => <<"error">>, 
        file => list_to_binary(File),
        info => extract_info(Reason)}] }
    end.
