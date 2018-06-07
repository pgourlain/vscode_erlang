-module(lsp_utils).

-export([client_range/3, file_uri_to_file/1, file_uri_to_vscode_uri/1]).

client_range(Line, StartChar, EndChar) ->
    #{
        <<"start">> => #{line => Line - 1, character => StartChar - 1},
        <<"end">> => #{line => Line - 1, character => EndChar - 1}
    }.

file_uri_to_file(Uri) ->
    re:replace(case Uri of
        <<"file:///", Drive, "%3A", Rest/binary>> -> <<Drive, ":", Rest/binary>>;
        <<"file://", Rest/binary>> -> Rest;
      _ -> Uri
    end, <<"\\\\">>, <<"/">>, [global, {return, list}]).

file_uri_to_vscode_uri(Uri) ->
    case Uri of 
        <<"file://", Drive, ":/", Rest/binary>> -> <<"file:///", Drive, "%3A/", Rest/binary>>;
      _ -> Uri
    end.
