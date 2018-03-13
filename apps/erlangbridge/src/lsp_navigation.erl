-module(lsp_navigation).


-export([goto_definition/3]).


goto_definition(File, Line, Column) ->
    try internal_goto_definition(File, Line, Column) of
    _Any -> _Any
    catch
    _Err:_Reason -> error_logger:info_msg("goto_definition error ~p:~p", [_Err, _Reason])
    end.

internal_goto_definition(File, Line, Column) ->
    %get document from memory cache
    DocResult = case gen_lsp_doc_server:get_document(File) of
    {ok, Document} -> {ok, Document};
    not_found -> 
        case lsp_syntax:parse(File) of
        {ok, Forms} -> {ok, Forms};
        _ -> not_found
        end        
    end,
    case DocResult of
    {ok, NewForms} -> 
        %TODO: find in AST
        %A = erl_syntax_lib:analyze_forms(NewForms),
        %error_logger:info_msg("goto_definition ~p", [NewForms]),
        #{result => <<"ok">>, uri => list_to_binary(File), line => Line, character => Column +1};
    _ -> #{result => <<"ko">>}
    end.
