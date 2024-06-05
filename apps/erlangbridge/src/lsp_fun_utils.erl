-module(lsp_fun_utils).


-export([get_function_range/1]).

-include("lsp_log.hrl").


get_function_range({function, {L, C}, FnName, FnArity, _Body}) ->
    LastClause = lists:last(_Body),
    %?LOG("get_function_range: ~p~n", [LastClause]),
    case get_latest_lc(LastClause) of
        {-1,-1} -> {L,C,L,C};
        {L1,C1} -> {L,C,L1+1,C1}
    end.

% basic filter to get the latest line/column
get_latest_lc({clause, {L, C}, _, _, Body}) ->
    case Body of
        [] -> {L,C};
        _ -> get_latest_lc(lists:last(Body))
    end;
get_latest_lc({T, {L, C}, _, Args}) when T =:= call orelse T =:= 'case' ->
    case Args of
        [] -> {L,C};
        _ -> get_latest_lc(lists:last(Args))
    end;
get_latest_lc({M, {_, _}, _, Args}) when M =:= match orelse M =:= cons ->
    get_latest_lc(Args);
get_latest_lc({'try', {_, _}, A1, A2, A3, A4}) ->
    List = A1 ++ A2 ++ A3 ++ A4,
    get_latest_lc(lists:last(List));
get_latest_lc({T, {_, _}, Clauses}=_Other) when T =:= tuple orelse T =:= 'if' ->
    get_latest_lc(lists:last(Clauses));

%todo tuple

get_latest_lc({_, {L, C}}=_Other) ->
    %?LOG("get_latest_lc: 0 token: ~p", [_Other]),
    {L,C};
get_latest_lc({_, {L, C}, _}=_Other) ->
    %?LOG("get_latest_lc: 1 token: ~p", [_Other]),
    {L,C};
get_latest_lc({_, {L, C}, _, _}=_Other) ->
    %?LOG("get_latest_lc: 2 token: ~p", [_Other]),
    {L,C};
get_latest_lc({_, {L, C}, _, _, _}=_Other) ->
    %?LOG("get_latest_lc: 3 token: ~p", [_Other]),
    {L,C};
get_latest_lc(_Other) ->
    ?LOG("get_latest_lc: unkonwn token: ~p", [_Other]),
    {-1,-1}.

