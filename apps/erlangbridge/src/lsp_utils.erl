-module(lsp_utils).

-export([client_range/3,
         file_uri_to_file/1, file_uri_to_vscode_uri/1,
         glob_to_regexp/1,
         is_path_excluded/2,
         search_exclude_globs_to_regexps/1,
         to_string/1,
         is_erlang_lib_file/1]).

client_range(Line, StartChar, EndChar) ->
    #{
        <<"start">> => #{line => Line - 1, character => StartChar - 1},
        <<"end">> => #{line => Line - 1, character => EndChar - 1}
    }.

file_uri_to_file(Uri) ->    
    NewUri = re:replace(case Uri of
        <<"file:///", Drive, "%3A", Rest/binary>> -> <<Drive, ":", Rest/binary>>;
        <<"file://", Rest/binary>> -> Rest;
      _ -> Uri
    end, <<"\\\\">>, <<"/">>, [global, {return, list}]),
    lists:flatten(string_replace(NewUri, "%20", " ")).

file_uri_to_vscode_uri(Uri) ->
    UriWithOutSpace = lists:flatten(string_replace(to_string(Uri), " ", "%20")),
    EncodeUri = if
        is_binary(Uri) ->  erlang:list_to_binary(UriWithOutSpace);
        true -> UriWithOutSpace
    end,
    case EncodeUri of
        <<"file://", Drive, ":/", Rest/binary>> -> <<"file:///", Drive, "%3A/", Rest/binary>>;
      _ -> EncodeUri
    end.

-ifdef(OTP_RELEASE).
string_replace(String, Pattern, NewString) ->
    string:replace(String, Pattern, NewString).
string_prefix(String, Prefix) ->
    string:prefix(String, Prefix).
-else.
string_replace(String, Pattern, NewString) ->
    case string:str(String, Pattern) of
        0 -> String;
        Index -> 
            S = string:sub_string(String, 1, Index-1),
            SEnd = string:sub_string(String, Index+length(Pattern)),
            S ++ NewString ++ string_replace(SEnd, Pattern, NewString) 
    end.

string_prefix(String, Prefix) ->
    L = string:left(String, length(Prefix)),
    if 
        L =:= Prefix -> string:sub_string(String, length(Prefix)+1);
        true -> nomatch
    end.

-endif.

to_string(X) when is_binary(X) ->
    erlang:binary_to_list(X);
to_string(X) when is_atom(X) ->
    erlang:atom_to_list(X);

to_string(X) ->
    X.

%% -------------------------------------------------------------------
%% @doc Replace file globs to regular expressions in search exclude filters
%% @end
%% -------------------------------------------------------------------
-spec search_exclude_globs_to_regexps(GlobExcludes) -> RegexpExcludes
      when GlobExcludes   :: #{Glob :: atom() => Exclude},
           RegexpExcludes :: #{RegExp :: string() => Exclude},
           Exclude        :: boolean().
search_exclude_globs_to_regexps(Excludes = #{}) ->
    maps:fold(
        fun(Glob, Exclude, Acc) ->
            RegExp = search_regexp(glob_to_regexp(atom_to_list(Glob))),
            Acc#{RegExp => Exclude}
        end,
        #{},
        Excludes).

%% -------------------------------------------------------------------
%% @doc Translate a Unix-style file glob to regular expression.
%%
%% Known and translated glob patterns:
%%
%% * `?': Matches one character in the filename.
%% * `*': Matches any number of characters up to the end of the filename or the
%%   next slash.
%% * `**': Two adjacent `*' used as a single pattern match all files and zero or
%%   more directories and subdirectories.
%% * `[Character1,Character2,...]': Matches any of the characters listed. Two
%%   characters separated by a hyphen match a range of characters.
%%   Example: `[A-Z]' matches any uppercase letter.
%% * `{Item1,...}' -> `(Item1|...)': Alternation. Matches one of the
%%   alternatives.
%% @end
%% @see filelib:wildcard/1
%% -------------------------------------------------------------------
-spec glob_to_regexp(Glob :: string()) -> RegExp :: string().
glob_to_regexp(Glob) ->
    glob_alternations_to_regexp(glob_wo_alternatives_to_regexp(Glob)).

%% Translate glob patterns except alternatives to regular expression patterns
glob_wo_alternatives_to_regexp([]) ->
    [];
%% Keep escaped special characters as literals
glob_wo_alternatives_to_regexp("\\?" ++ Chars) ->
    ["\\?" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("\\*" ++ Chars) ->
    ["\\*" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("\\[" ++ Chars) ->
    ["\\[" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("\\]" ++ Chars) ->
    ["\\]" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("\\{" ++ Chars) ->
    ["\\{" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("\\}" ++ Chars) ->
    ["\\}" | glob_wo_alternatives_to_regexp(Chars)];
%% Change Windows-style path separator to Unix-style because Erlang file*
%% modules always use that
glob_wo_alternatives_to_regexp("\\" ++ Chars) ->
    ["/" | glob_wo_alternatives_to_regexp(Chars)];
%% Glob patterns to regular expression patterns
glob_wo_alternatives_to_regexp("**" ++ Chars) ->
    [".*" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("*" ++ Chars) ->
    ["[^/]*" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp("?" ++ Chars) ->
    ["[^/]" | glob_wo_alternatives_to_regexp(Chars)];
glob_wo_alternatives_to_regexp([Char | Chars]) ->
    [Char | glob_wo_alternatives_to_regexp(Chars)].

%% Translate glob alternations to regular expression alternations
-spec glob_alternations_to_regexp(Glob :: string()) -> RegExp :: string().
glob_alternations_to_regexp(Glob) ->
    RE = "^(.*)\\{([^{}]+,[^{}]+)\\}(.*)$",
    case re:run(Glob, RE, [{capture, all_but_first, list}]) of
        {match, [Prefix, Inner, Postfix]} ->
            Inner2 = string:replace(Inner, ",", "|", all),
            glob_alternations_to_regexp([Prefix, "(", Inner2, ")", Postfix]);
        nomatch ->
            lists:flatten(io_lib:format("~s", [Glob]))
    end.

%% -------------------------------------------------------------------
%% @doc Transform a file glob regular expression to search filter glob regular
%% expression
%% @end
%% @see https://code.visualstudio.com/docs/editor/codebasics#_advanced-search-options
%% -------------------------------------------------------------------
-spec search_regexp(GlobRegExp :: string()) -> SearchGlobRegExp :: string().
%% `/example' matches `example' in the top level of filesystem (root folder)
search_regexp("/" ++ Chars) ->
    case os:type() of
        {win32, _} -> "^[a-zA-Z]:/" ++ Chars ++ "(/.*)?$";
        {unix,  _} -> "^/" ++ Chars ++ "(/.*)?$"
    end;
%% `./example' matches `example' at the top level of your workspace
search_regexp("./" ++ Chars) ->
    RootParts = filename:split(gen_lsp_config_server:root()),
    RelParts = filename:split(Chars),
    "^" ++ filename:join(RootParts ++ RelParts) ++ "(/.*)?$";
search_regexp(RegExp) ->
    RegExp ++ "(/.*)?$".

%% -------------------------------------------------------------------
%% @doc Check if `Path' is excluded by exclude filters.
%%
%% Exclude filters must contain regular expressions instead of file globs.
%% If `Path' matches to any `RegExp' with `Exclude == false' (~include) then
%% `Path' is not excluded. Otherwise if `Path' matches to any `RegExp' with
%% `Exclude == true' then `Path' is excluded. Otherwise `Path' is not excluded
%% (~included).
%% @end
%% @see search_exclude_globs_to_regexps/1
%% -------------------------------------------------------------------
-spec is_path_excluded(Path :: string(), ExcludeMap) -> boolean()
      when ExcludeMap :: #{RegExp :: string() => Exclude :: boolean()}.
is_path_excluded(Path, ExcludeMap) ->
    ExcludeFilters = maps:to_list(ExcludeMap),
    do_is_path_excluded(Path, ExcludeFilters, false).

do_is_path_excluded(_Path, [], PreliminaryAnswer) ->
    PreliminaryAnswer;
%% `Path' matches to a non-exclude filter -> include
do_is_path_excluded(Path, [{RegExp, false} | ExcFilters], PreliminaryAnswer) ->
    case re:run(Path, RegExp) of
        {match, _} -> false;
        nomatch    -> do_is_path_excluded(Path, ExcFilters, PreliminaryAnswer)
    end;
%% `Path' matches to an exclude filter -> exclude unless it's included later
do_is_path_excluded(Path, [{RegExp, true} | ExcFilters], PreliminaryAnswer) ->
    case re:run(Path, RegExp) of
        {match, _} -> do_is_path_excluded(Path, ExcFilters, true);
        nomatch    -> do_is_path_excluded(Path, ExcFilters, PreliminaryAnswer)
    end.


is_erlang_lib_file(File) ->
    case string_prefix(File, code:lib_dir()) of
        nomatch -> false;
        _ -> true
    end.