-module(vscode_lsperlangserver).

%%%%%%%%%%%%
% NOT USED, see vscode_lsp_entry.erl 
%%%%%%%%%%%


-behaviour(gen_connection).

-export([start/0, start/1, start/2]).

% export for gen_connection behaviour
-export([decode_request/1, get_port/0, init/1]).

-define(LINE_START, 0).
-define(COLUMN_START, 0).

start() ->
    error_logger:info_msg("vscode_lsperlangserver started/0"),
    gen_connection:start(?MODULE).

start(_Args) ->
    error_logger:info_msg("vscode_lsperlangserver started/1"),
    gen_connection:start(?MODULE).

start(_Type, _Args) ->
    error_logger:info_msg("vscode_lsperlangserver started/2 (~p, "
	      "~p)",
	      [_Type, _Args]),
    gen_connection:start(?MODULE).

get_port() ->
    {ok, [[P]]} = init:get_argument(vscode_port), P.

init(_Port) ->
    ok.

decode_request(Data) ->
    % next step use on gen_server per document, to keep
    % 1) syntaxtree in state
    % 2) variables
    % 3) methods descriptions
    % ...
    io:format("vscode_lsperlangserver decode_request/1 "
	      "(~p)",
	      [Data]),
    case parse_request(Data) of
      {validate_text_document, FileName} ->
	  parse_file_uri(FileName);
      {format_document, FileName} ->
	  format_file_uri(FileName);
      {completion_items, FileName, Line, Column, _LastEnterChar} ->
	  get_completion_items(FileName, Line, Column);
      _ ->
	  #{parse_result => false,
	    error_message => <<"unknown command">>}
    end.

parse_request(Data) ->
    Content = binary_to_list(Data),
    %"POST debugger_continue HTTP/1.1\r\nContent-Type: plain/text\r\nContent-Length: 1\r\nHost: 127.0.0.1:36477\r\nConnection: close\r\n\r\n3"
    Lines = string:tokens(Content, "\r\n"),
    Command = list_to_atom(lists:nth(2,
				     string:tokens(lists:nth(1, Lines), " "))),
    %Body = string:join(lists:nthtail(5, Lines), "\r\n"),
    %{Command, Body}.
    command_and_args({Command}, lists:nthtail(5, Lines)).

command_and_args(State, [H | T]) ->
    command_and_args(erlang:append_element(State, H), T);
command_and_args(State, []) -> State.

file_uri_to_file(FileName) ->
    case string:left(FileName, 7) of
      "file://" -> string:sub_string(FileName, 8);
      _ -> FileName
    end.

parse_file_uri(FileName) ->
    parse_file(file_uri_to_file(FileName)).

parse_file(File) ->
    case epp:parse_file(File, []) of
      {ok, Forms} ->
	  case erl_lint:module(Forms, File) of
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
		%io:format("~p --- ~p", [Errors, Warnings]),
		#{parse_result => true,
		  errors_warnings =>
		      extract_error_or_warning(<<"error">>, Errors) ++
			extract_error_or_warning(<<"warning">>, Warnings)}
	  end;
      {error, _} ->
	  #{parse_result => false,
	    error_message => <<"Cannot open file">>}
    end.

extract_error_or_warning(Type, ErrorsOrWarnings) ->
    [#{type => Type,
       file =>
	   erlang:list_to_binary(element(1, ErrorsOrWarnings)),
       info => extract_info(X)}
     || X <- element(2, ErrorsOrWarnings)].

extract_info(X) ->
    % samples of X
    %{20,erl_parse,["syntax error before: ","load_xy"]}
    %{11,erl_lint,{undefined_function,{load_xy,1}}}]}
    #{line => element(1, X),
      message =>
	  erlang:list_to_binary(io_lib:fwrite("~p",
					      [element(3, X)]))}.

format_file_uri(FileName) ->
    format_file(file_uri_to_file(FileName)).

format_file(FileName) ->
    erl_tidy:file(FileName, [{backups, false}]),
    #{format_result => true}.

get_completion_items(FileName, Line, Column) ->
    F = file_uri_to_file(FileName),
    case file:read_file(F) of
      {ok, Text} ->
	  case erl_scan:string(erlang:binary_to_list(Text),
			       {?LINE_START, ?COLUMN_START})
	      of
	    {ok, Tokens, _} ->
        try get_module_name_from_tokens(Tokens, Line, Column) of
        M -> #{completion_result => <<"ok">>, methods => get_module_exports(M)}
        catch
        _:X -> #{completion_result => <<"ko">>, message => erlang:list_to_binary(io_lib:fwrite("~p, stack_trace : ~p",[X, erlang:get_stacktrace()]))}
        end;

	    Other -> #{completion_result => <<"ko">>,
            message => erlang:list_to_binary(io_lib:fwrite("~p",[Other])) }
	  end;
      _ -> #{}
    end.

get_module_name_from_tokens(Tokens, L, C) -> 
    io:format("get_wget_module_name_from_tokensord_at (~p,~p) ~p",[L, C, Tokens]),
    case get_word_at(Tokens, L, C,[]) of
        [] -> [];
        Other -> io:format("get_word_at (~p)",[Other])
    end,
    erlang.

get_word_at([H|T], L, C, Previous) -> 
    case check_token_at(H, L, C) of
        true -> {ok,  Previous};
        false -> get_word_at(T, L, C, H)
    end;

get_word_at([], _L, _C, _Previous) -> 
    {ko, []}.

check_token_at(T, L, C) ->
%{var,{84,24},'B'},
%{']',{84,25}},
%{')',{84,26}},
%{')',{84,27}},
%{')',{84,28}},
%{dot,{84,29}},
    case T of
    {TName, {L1, C1}, TValue} ->
        (L == L1) and (C == C1+token_length(TName, TValue));
    {TName, {L1, C1}} ->
        (L == L1) and (C == C1+token_length(TName));
    Token ->
        io:format("get_word_at unknown token (~p)",[Token]),
        false
    end.

token_length(dot) ->
    1;
token_length(TName) when is_list(TName) ->
    erlang:length(TName);

token_length(TName) when is_atom(TName) ->
    length(atom_to_list(TName));

token_length(TName) ->
    io:format("uknown token : '~p'", [TName]),
    1.
    %%length(atom_to_list(TName));

token_length(atom, TValue) ->
    token_length(atom_to_list(TValue));
token_length(var, TValue) ->
    token_length(atom_to_list(TValue));
token_length(integer, TValue) ->
    token_length(integer_to_list(TValue));


token_length(TName, TValue) when is_atom(TName) ->
    token_length(TValue).
     
get_module_exports(M) ->
    case erlang:module_loaded(M) of
        false -> [];
        _ ->
            Exports = M:module_info(exports),
            [erlang:list_to_binary(erlang:atom_to_list(X)) || {X, _} <- Exports]
    end.
