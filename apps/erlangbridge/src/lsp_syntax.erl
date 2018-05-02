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
        OriginalCodePath = code:get_path(),
        DepsDir = filename:join(filename:dirname(File), "../deps"),
        case file:list_dir(DepsDir) of
            {ok, Dirs} ->
                [code:add_path(filename:join(DepsDir, X)) || X <- Dirs];
            {error, _} ->
                ok
        end,

        Ret = do_epp_parse_file(File, FIO),

        code:set_path(OriginalCodePath),
        file:close(FIO), 
        %error_logger:info_msg("epp_parse_file '~p'",[Ret]),
        Ret;
    _ -> {error, file_could_not_opened}
    end.

do_epp_parse_file(File, FIO) ->
    case epp:open(File, FIO, {1,1},["../include"],[]) of
    {ok, Epp} -> {ok, epp:parse_file(Epp)};
    {error, _Err} -> {error, _Err} 
    end.

lint(Forms, File) ->
    LintResult = erl_lint:module(Forms, File,[ {strong_validation}]),
    error_logger:info_msg("lint result '~p'",[LintResult]),
    case remove_include_errors(LintResult) of
    % nothing wrong
    {ok, []} -> #{parse_result => true};
    % just warnings
    {ok, [Warnings]} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"warning">>, Warnings)};
    % errors, no warnings
    {error, Errors, []} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"error">>, Errors)};
    % errors and warnings
    {error, Errors, [Warnings]} ->
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

remove_include_errors({error, [Errors], []}) ->
    {error, filter_epp_errors(Errors), []};
remove_include_errors({error, [Errors], [Warnings]}) ->
    {error, filter_epp_errors(Errors), [Warnings]};
remove_include_errors(_Any) ->
    _Any.

filter_epp_errors({File, Errors}) ->
    case lists:filter(fun (X) -> is_not_epp(X) end, Errors) of
    [] -> {File, []};
    _Any -> {File, _Any}
    end.

is_not_epp(X) ->
    element(2, X) =/= epp.

extract_error_or_warning(Type, ErrorsOrWarnings) ->
    [#{type => Type,
       file =>
	   erlang:list_to_binary(element(1, ErrorsOrWarnings)),
       info => extract_info(X)}
     || X <- element(2, ErrorsOrWarnings)];

extract_error_or_warning(_Type, {_, []}) ->
    [].

extract_info({{Line, _Column}, Source, MessageElements}) ->
    extract_info({Line, Source, MessageElements});
extract_info({Line, Source, MessageElements}) ->
    % samples of X
    %{20,erl_parse,["syntax error before: ","load_xy"]}
    %{11,erl_lint,{undefined_function,{load_xy,1}}}]}
    #{
        line => Line,
        message => erlang:list_to_binary(format_message_elements(Source, MessageElements))
    }.

format_message_elements(erl_parse, MessageElements) ->
    lists:flatten(MessageElements);
format_message_elements(erl_lint, MessageElements) ->
    lists:flatten(lists:join(" ", lists:map(fun format_lint_message_element/1, tuple_to_list(MessageElements)))).

format_lint_message_element({Function, Arity}) when is_atom(Function) andalso is_integer(Arity) ->
    atom_to_list(Function) ++ "/" ++ integer_to_list(Arity);
format_lint_message_element(Element) when is_atom(Element) ->
    string:replace(atom_to_list(Element), "_", " ").

parse_src_file(File) ->
    case file:path_consult(filename:dirname(File), File) of
    {ok,_, _} -> #{parse_result => true};
    {error, Reason} -> #{
        parse_result => true,
		errors_warnings => [#{type => <<"error">>, 
        file => list_to_binary(File),
        info => extract_info(Reason)}] }
    end.
